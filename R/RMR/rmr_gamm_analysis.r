
# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(ggplot2)
  library(ggh4x)
  library(gamm4)
  library(gamlss)
  library(gratia)
  library(here)
  library(itsadug)
  library(lubridate)
  library(lemon)
  library(janitor)
  library(lme4)
  library(mgcv)
  library(multcomp)
  library(openxlsx)
  library(patchwork)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidymv)
  library(tidyr)
  library(visreg)
  source(here("R", 
              "Cleaning and Calculations", 
              "julian_date_reorder.r"))
}
# bring in RDS -----
# 
ful <- read_rds(here("Saved Data",
                     "BioE_lt.rds"))

glimpse(ful)
##### SUBSET OUT ONLY RMR/M_Swim DATA and determine daily temp FOR 2017 - 2021-------


# ful_rmr <- ful %>%
#   filter(sensor_unit %in% "m/s²" &
#            year %in% c(2019, 2020)) %>%
#   filter(!m_swim == is.nan(m_swim)) %>%
#   group_by(floy_tag, weight, fish_basin, date,
#            week, month, season, year,
#            sensor_unit, labels) %>%
#   summarise(mean_rmr = mean(m_swim)) %>%
#   ungroup() %>%
#   mutate(
#     floy_tag = factor(floy_tag),
#     fish_basin = factor(
#       stringr::str_replace(as.character(fish_basin), " Basin", ""),
#       levels = c("East", "West", "North")),
#     doy = yday(date),
#     month = factor(month,
#                    levels = c("May", "June", "July",
#                               "August", "September", "October",
#                               "Novemeber", "December", "January",
#                               "February", "March", "April")),
#     doy_id = days(date, end = "05-22"),
#     season = forcats::fct_relevel(season, "Spring", "Summer",
#                                   "Fall", "Winter")
#   ) %>%
#   arrange(date) %>%
#   dplyr::select(floy_tag:sensor_unit, doy, doy_id, mean_rmr)
# glimpse(ful_rmr)
# 
# write_rds(ful_rmr, here("Saved Data",
#                          "Daily_RMR.rds"))
# # remove big objects to free up RAM -----
# # rm(ful)
# gc()


ful_rmr <- read_rds(here("Saved Data", 
                         "Daily_RMR.rds"))

glimpse(ful_rmr)
# look at data to see what dist it fits 

descdist(ful_rmr$mean_rmr) 

norm <- fitdist(ful_rmr$mean_rmr, distr = "norm", method = "mle")
plot(norm)
gammas <- fitdist(ful_rmr$mean_rmr, distr = "gamma", method = "mme")
plot(gammas)

ggplot(data = ful_rmr, aes(x = mean_rmr)) + 
  geom_histogram()

# ful_rmr %>% 
#   group_by(date, fish_basin) %>% 
#   summarise(mean_rmr = mean(mean_rmr)) %>% 
#   ungroup() %>% 
# ggplot() +
# geom_point(aes(x = date, y = mean_rmr,
#                                 colour = fish_basin), 
#            alpha = 0.25, size = 2) +
#   
# scale_colour_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   scale_shape_discrete(name = "Basin") +
#   scale_fill_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
#   #                # ncol = 1
#   # ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         legend.position = c(0.95, 0.92),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Date",
#        y = "Daily Temperature (°C)")
ful_rmr <-  ful_rmr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_rmr)





# -----------------------START GAMMS -------------------------------
m <- bam(mean_rmr ~ fish_basin + 
           s(doy_id, by = fish_basin, bs = "cc", k = 20) +
           s(floy_tag, by = fish_basin, bs = c("re")) + 
           ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3)),  
         method = "fREML",
         family = Gamma(link = "identity"),
         data = ful_rmr, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 11)
r1



m1 <- update(m, . ~ fish_basin  + 
               s(doy_id, by = fish_basin, bs = "cc", k = 20) +
               s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
                 k = c(20, 4)) +
               ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3)), 
             discrete = TRUE,
             rho = r1, 
             AR.start = ful_rmr$start_event
)
# check model fit -----
# par(mfrow = c(2, 2))
gam.check(m)
# summary(m)
gam.check(m1)
summary(m1)
# look at overall effect terms -----
m_overall <- anova.gam(m1, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m_overall$pTerms.table %>% 
  as_tibble(rownames = "term") %>% 
  clean_names()
# grab inddial effect 
ind_parm <- tidy(m1, parametric = TRUE) %>% 
  clean_names()
# smoother effect 
smoothers <- tidy(m1) %>% 
  clean_names()

# model comparison and fit info 
m_glance <- glance(m1) %>% 
  clean_names()


# view all model info ----

overall_parm
ind_parm
smoothers
m_glance

# =---- save summaries ------

overall_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "gamm_rmr_param_overall.xlsx"))
ind_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "gamm_rmr_param_ind.xlsx"))

smoothers %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "gamm_rmr_smoothers.xlsx"))
m_glance %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "gamm_rmr_model_fit.xlsx"))
# pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_rmr %>% 
  mutate(
    floy_tag = "a", 
    year = 0
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m1, newdata = dat_2, 
                    type = "response", se = TRUE, discrete = FALSE,  
                    exclude = c("s(floy_tag)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit, 
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec", "Jan",
                                       "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)

# double check that predicts looks correct 
glimpse(predicts) 

# calculate daily mean temp by fish basin 
# ful %>%
#   filter(sensor_unit %in% "m/s²" & 
#            year %in% c(2019, 2022)) %>%
#   group_by(date, fish_basin) %>%
#   summarise(mean_rmr = mean(m_swim)) %>%
#   ungroup() -> mean_rmr_right
ful_rmr %>%
  group_by(doy_id, fish_basin) %>%
  summarise(mean_rmr = mean(mean_rmr)) %>%
  ungroup() -> mean_rmr


glimpse(ful_rmr)
glimpse(mean_rmr)

glimpse(predicts)

predicts %>% 
  filter(month %in% "May" & year == 2019) %>% 
  tail()

# create month labels 
predicts %>% 
  filter(doy_id %in% seq(09, 344, 67)) %>%
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 
predicts %>% 
  group_by(season) %>% 
  summarise(first = first(doy_id),
            last = last(doy_id)) %>% 
  ungroup()

rect_summer <- tibble(
  season = "Summer",
  xmin = 10,
  xmax = 101,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 193,
  xmax = 283,
  ymin = -Inf,
  ymax = Inf
)

# ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
ggplot(data = predicts) +
  geom_rect(data = rect_summer, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = "grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill ="grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_text(
    aes(x = xmin + 30, y = 125, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = 125, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_point(data = mean_rmr, aes(x = doy_id, y = mean_rmr,
  #                                  colour = fish_basin,
  # ), alpha = 0.25, size = 2.75) +
  geom_point(data = mean_rmr, aes(x = doy_id, y = mean_rmr,
                                  colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), linewidth = 1) +
  geom_ribbon(
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(45, 135, 15)) +
  scale_x_continuous(breaks = seq(09, 344, 67),
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  
  # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
  #                # ncol = 1
  # ) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = expression(paste("Active Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p


write_rds(p, here("Plot Objects", 
                  "daily_rmr_GAMM_plot.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "gamm_BioE_doy.png"), width = 11,
       height = 7 )

# ------------ plot each fish's profile facted --------
dat_3 <- ful_rmr 

glimpse(dat_3)

# use prediction to get interpolated points 
fits_id <- predict.bam(m1, newdata = dat_3, 
                       type = "response", se = TRUE)



# calcuate lower and upper ci per id add month abb 
pred_id <- data.frame(dat_3, fits_id) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec", "Jan",
                                       "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)



# double check that predicts looks correct 
glimpse(pred_id)
# create month labels 
pred_id %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label






# ---- plot 2017 - 2020 gamm with mean daily temp per ID ------
ggplot() +
  geom_point(data = ful_rmr, aes(x = doy_id, y = mean_rmr,
                                 colour = fish_basin,
  ), alpha = 0.25, size = 2) +
  geom_line(data = pred_id, 
            aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(data = pred_id, aes(ymin = lower,
                                  ymax = upper,
                                  x = doy_id, y = fit,
                                  fill = fish_basin), alpha = 0.5) +
  # scale_y_continuous(breaks = seq(20, 60, 10)
  # ) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") + 
  
  facet_rep_wrap(.~ floy_tag, repeat.tick.labels = FALSE,
                 # ncol = 1
  ) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = c(0.95, 0.14),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = expression(paste("Active Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1

p1

ggsave(plot = p1, filename = here("plots",
                                  "Individaul Plots",
                                  "gamm_rmr_2017_2021_id.png"),
       width = 11 * 2,
       height = 7 * 2.15)

