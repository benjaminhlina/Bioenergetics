# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(ggplot2)
  library(ggh4x)
  library(gamm4)
  library(gratia)
  library(glmmTMB)
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
# ful <- read_rds(here("Saved Data",
#                      "BioE_lt.rds"))
# # 
# glimpse(ful)
# ##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------
# 
# 
# 
# ful_temp <- ful %>%
#   filter(sensor_unit %in% "°C") %>%
# 
#   group_by(floy_tag, weight, date, fish_basin,
#            week, month, season, year,
#            sensor_unit, labels) %>%
#   summarise(mean_temp = mean(sensor_value)) %>%
#   ungroup() %>%
#   mutate(date_2 = as.numeric(date),
#          floy_tag = factor(floy_tag)) %>%
#   filter(date_2 <= 18559) %>%
#   mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
#                                                   ""),
#                              levels = c("East", "West", "North")),
#          doy = yday(date),
#          month = factor(month,
#                         levels = c("May", "June", "July",
#                                    "August", "September", "October",
#                                    "Novemeber", "December", "January",
#                                    "February", "March", "April")
#          )
#   ) %>%
#   arrange(month) %>%
#   mutate(doy = days(date)) %>%
#   arrange(date)
# 
# glimpse(ful_temp)
# 
# 
# 
# # remove big objects to free up RAM -----
# # gc()
# 
# 
# 
# tl_d <- ful_temp %>%
#   group_by(fish_basin, floy_tag, year) %>%
#   summarise(doy_min = min(doy)) %>%
#   ungroup()
# 
# 
# # test <- start_event(ful_temp, column = c("floy_tag", "doy"))
# 
# 
# 
# 
# ful_temp <-  ful_temp %>%
#   group_by(floy_tag, year) %>%
#   arrange(floy_tag, year, doy) %>%
#   mutate(start_event = if_else(doy == min(doy), true = TRUE,
#                                false = FALSE)) %>%
#   ungroup() %>%
#   arrange(date, start_event)
# 
# 
# write_rds(ful_temp, here("Saved Data",
#                "Daily_temp.rds"))

ful_temp <- read_rds(here("Saved Data", 
                          "Daily_temp.rds"))

days()
ful_temp <- ful_temp %>% 
  mutate(doy = days(date, end = "02-28")) %>% 
  group_by(floy_tag, year) %>%
  arrange(floy_tag, year, doy) %>%
  mutate(start_event = if_else(doy == min(doy), true = TRUE,
                               false = FALSE)) %>%
  ungroup() %>%
  arrange(date, start_event)


glimpse(ful_temp)

# crete day of year (doy) variable and refactor month 
# have doy start in may and end in april... 


glimpse(ful_temp)
temp_sum <- ful_temp %>% 
  group_by(season) %>% 
  summarise(
    temp = mean(mean_temp), 
    sem = sd(mean_temp) / sqrt(n())
  ) %>% 
  ungroup()


temp_sum %>% 
  print(n = 36)

# -----------------------START GAMMS -------------------------------



m18 <- bam(mean_temp ~ fish_basin  + 
             s(doy, by = fish_basin, bs = "cc", k = 17) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)), method = "fREML",
           family = Gamma(link = "log"),
           data = ful_temp, 
           select = TRUE
)

acf(resid_gam(m18))

r1 <- start_value_rho(m18, plot = TRUE, lag = 17)
r1




m19 <- bam(mean_temp ~ fish_basin  + 
             s(doy, by = fish_basin, bs = "cc", k = 17) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)), 
           method = "fREML",
           family = Gamma(link = "log"),
           data = ful_temp, 
           select = TRUE,
           discrete = TRUE, 
           rho = r1, 
           AR.start = ful_temp$start_event
           
)

acf(resid_gam(m19))


# check model fit -----
par(mfrow = c(2, 2))
gam.check(m18)
gam.check(m19)
appraise(m19)

# concurvity(m18)
# concrvity(m18)
# concurvity(m19)

# look at overall effect terms -----
m19_overall <- anova.gam(m19, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m19_overall$pTerms.table %>% 
  as_tibble(rownames = "terms") %>% 
  clean_names()
# grab inddial effect 
ind_parm <- tidy(m19, parametric = TRUE) %>% 
  clean_names()
# smoother effect 
smoothers <- tidy(m19) %>% 
  clean_names()

# model comparison and fit info 
m19_glance <- glance(m19) %>% 
  clean_names()


# write_rds(m19, here("model objects", 
#                     "temp_gamm_model.rds"))
# view all model info ----

overall_parm
ind_parm
smoothers
m19_glance

glimpse(ful_temp)

# m <- glmmTMB(mean_temp ~ fish_basin + (1 | floy_tag) + (1 | year),
#              data = ful_temp,
#              family = Gamma(link = "log"), REML = TRUE,
# )
# 
# car::Anova(m, type = "III")
# anova.gam(m19,freq = FALSE)
# summary(m)
# summary(m19)



# =---- save summaries 

# overall_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp results",
#                                   "gamm_temp_param_overall.xlsx"))
# ind_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp results",
#                                   "gamm_temp_param_ind.xlsx"))
# 
# smoothers %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp results",
#                                   "gamm_temp_smoothers.xlsx"))
# m19_glance %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp results",
#                                   "gamm_temp_model_fit.xlsx"))
#  pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_temp %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  ) 
# dplyr::select(floy_tag, fish_basin, doy, doy, start_event, year, mean_temp)

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m19, newdata = dat_2, se = TRUE, discrete = FALSE, 
                    exclude = c("s(floy_tag, year)"),
                    newdata.guaranteed = TRUE)
ba <- augment(m19, 
              newdata = dat_2, 
              se = TRUE, type.predict = "link", 
              # discrete = FALSE, 
              # exclude = c("s(floy_tag, year)"),
              # newdata.guaranteed = TRUE
)

ba <- ful_temp %>% 
  mutate(
    floy_tag = "a",
    year = 0
  ) %>% 
  augment(m19,data = ., 
          # type.predict = "link", 
          se = TRUE, 
          exclude = "s(floy_tag, year)", discrete = FaLSE) %>% 
  mutate(
    
    
    lower = exp(1) ^ (.fitted - 1.96 * .se.fit),
    upper = exp(1) ^ (.fitted + 1.96 * .se.fit), 
    .fitted = exp(1) ^ .fitted, 
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("Jan",
                                  "Feb", "Mar", "Apr", 
                                  "May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec"))) %>% 
  arrange(floy_tag, doy)


# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <-
  data.frame(dat_2, fits) %>%
  mutate(
    # fit = exp(1) ^ fit, 
    # se.fit = exp(1) ^ se.fit,
    lower = exp(1) ^ (fit - (1.96 * se.fit)),
    upper = exp(1) ^ (fit + (1.96 * se.fit)), 
    fit = exp(1) ^ fit, 
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("Jan",
                                  "Feb", "Mar", "Apr", 
                                  "May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec"))) %>% 
  arrange(floy_tag, doy)

# double check that predicts looks correct 
glimpse(predicts) 
glimpse(ba)
predicts
# calculate daily mean temp by fish basin 
ful_temp %>%
  group_by(doy, fish_basin) %>% 
  summarise(mean_temp = mean(mean_temp)) %>% 
  ungroup() -> temp_mean

# create month labels 


# plotting prep -------
month_doy <- predicts %>%
  group_by(month_abb) %>%
  summarise(first = first(doy),
            last = last(doy)) %>%
  ungroup() %>%
  mutate(
    # month_abb = forcats::fct_relevel(month_abb, "Jan",
    #                                  "Feb", "Mar", "Apr", "May", "Jun",
    #                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) %>%
  arrange(month_abb) %>%
  mutate(
    # first = case_when(
    # month_abb %in% "May" ~ 1,
    # month_abb %in% "June" ~ 10 + 20, false = first
    # )
  ) %>%
  .$first

month_doy
# month_doy <- c(1, 32, 62, 93, 123, 154, 184, 215, 246, 274, 305, 335)
predicts %>%
  filter(doy %in% month_doy) %>%
  group_by(month_abb) %>%
  summarise() %>%
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan",
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>%
  arrange(month_abb) %>%
  .$month_abb -> month_label
month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 

rect_summer <- tibble(
  season = "Summer",
  xmin = 152,
  xmax = 244,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 1,
  xmax = 60,
  ymin = -Inf,
  ymax = Inf
)
rect_winter_dec <- tibble(
  season = "Winter",
  xmin = 335,
  xmax = 365,
  ymin = -Inf,
  ymax = Inf
)

write_rds(predicts, here("model objects", 
                         "temp_gamm_predicts_update_jan.rds"))
write_rds(ba, here("model objects", 
                   "temp_gamm_predicts_a_jan.rds"))
# ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
ggplot(predicts) +
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
    aes(x = xmin + 30, y = 12.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = 12.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = temp_mean, aes(x = doy, y = mean_temp,
                                   colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy, y = fit, colour = fish_basin), linewidth = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  scale_x_continuous(breaks = month_doy, 
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
        legend.position = c(0.05, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Temperature (°C)") -> p 

p

write_rds(p, here("Plot Objects", 
                  "daily_temp_GAMM_plot_update.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots",
                                 "gamm_temp_doy_update.png"), width = 11,
       height = 7 )




# ------------ plot each fish's profile facted --------
dat_3 <- ful_temp 

glimpse(dat_3)

# use prediction to get interpolated points 
fits_id <- predict.bam(m19, newdata = dat_3, 
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
  arrange(floy_tag, doy)




# double check that predicts looks correct 
glimpse(pred_id)
# create month labels 
predicts %>% 
  filter(doy %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label


# ---- plot 2017 - 2020 gamm with mean daily temp per ID ------
ggplot(data = pred_id) +
  geom_point(aes(x = doy, y = mean_temp,
                 colour = fish_basin,
  ), alpha = 0.25, size = 2) +
  geom_line(aes(x = doy, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  x = doy, y = fit,
                  fill = fish_basin), alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 18, 2)) +
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
       y = "Daily Temperature (°C)") -> p1

p1

ggsave(plot = p1, filename = here("plots",
                                  "gamm_temp_2017_2021_id.png"),
       width = 11 * 2,
       height = 7 * 2.15)

