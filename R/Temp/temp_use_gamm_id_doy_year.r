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
# 
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
#   mutate(doy_id = days(date)) %>%
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
#   summarise(doy_min = min(doy_id)) %>%
#   ungroup()
# 
# 
# # test <- start_event(ful_temp, column = c("floy_tag", "doy_id"))
# 
# 
# 
# 
# ful_temp <-  ful_temp %>%
#   group_by(floy_tag, year) %>%
#   arrange(floy_tag, year, doy_id) %>%
#   mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE,
#                                false = FALSE)) %>%
#   ungroup() %>%
#   arrange(date, start_event)
# 
# 
# write_rds(ful_temp, here("Saved Data",
#                "Daily_temp.rds"))

ful_temp <- read_rds(here("Saved Data", 
                          "Daily_temp.rds"))

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
             s(doy_id, by = fish_basin, bs = "cc", k = 15) +
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
             s(doy_id, by = fish_basin, bs = "cc", k = 15) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)), method = "fREML",
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

concurvity(m18)
concrvity(m18)
concurvity(m19)

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


write_rds(m19, here("model objects", 
                    "temp_gamm_model.rds"))
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

overall_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Temp results",
                                  "gamm_temp_param_overall.xlsx"))
ind_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Temp results",
                                  "gamm_temp_param_ind.xlsx"))

smoothers %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Temp results",
                                  "gamm_temp_smoothers.xlsx"))
m19_glance %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Temp results",
                                  "gamm_temp_model_fit.xlsx"))
#  pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_temp %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m19, newdata = dat_2, 
                    type = "response", se = TRUE, discrete = FALSE, 
                    exclude = c("s(floy_tag, year)"),
                    newdata.guaranteed = TRUE)
fits <- predict.bam(m19, newdata = dat_2, 
                    type = "response", se = TRUE, discrete = FALSE, 
                    exclude = c("s(floy_tag, year)"),
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
ful_temp %>%
  group_by(doy_id, fish_basin) %>% 
  summarise(mean_temp = mean(mean_temp)) %>% 
  ungroup() -> temp_mean

# create month labels 
predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
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
  xmin = 32,
  xmax = 123,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 220,
  xmax = 305,
  ymin = -Inf,
  ymax = Inf
)


write_rds(predicts, here("model objects", 
                         "temp_gamm_predicts.rds"))
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
  geom_point(data = temp_mean, aes(x = doy_id, y = mean_temp,
                                   colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
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
       y = "Daily Temperature (°C)") -> p 

p

write_rds(p, here("Plot Objects", 
                  "daily_temp_GAMM_plot.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots",
                                 "gamm_temp_doy.png"), width = 11,
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
  arrange(floy_tag, doy_id)




# double check that predicts looks correct 
glimpse(pred_id)
# create month labels 
predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label


# ---- plot 2017 - 2020 gamm with mean daily temp per ID ------
ggplot(data = pred_id) +
  geom_point(aes(x = doy_id, y = mean_temp,
                 colour = fish_basin,
  ), alpha = 0.25, size = 2) +
  geom_line(aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
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

