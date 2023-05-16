# load packages ----

library(broom.mixed)
library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(ggh4x)
library(gamm4)
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

# bring in RDS -----

ful <- read_rds(here("Saved Data", 
                     "BioE_lt_hour.rds"))

glimpse(ful)
##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------



ful_temp <- ful %>% 
  filter(sensor_unit %in% "°C") %>%
  mutate(date_2 = as.numeric(date), 
         floy_tag = factor(floy_tag)) %>%
  filter(date_2 <= 18559) %>% 
  group_by(floy_tag, hour, year, fish_basin, season, 
           sensor_unit, labels) %>% 
  summarise(mean_temp = mean(sensor_value)) %>% 
  ungroup() %>% 
  
  mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
                                                  ""), 
                             levels = c("East", "West", "North"), 
                             ), 
         season = forcats::fct_relevel(season, "Spring", "Summer", 
                                       "Fall", "Winter")) %>%
  arrange(year)
# remove big objects to free up RAM -----
gc()

glimpse(ful_temp)
# look at tod distribuation -----
descdist(ful_temp$mean_temp)
ggplot(data = ful_temp, aes(x = mean_temp)) + 
  geom_histogram()


norm <- fitdist(ful_temp$mean_temp, distr = "norm", method = "mle")
plot(norm)

gammas <- fitdist(ful_temp$mean_temp, distr = "gamma", method = "mme")
plot(gammas)


ful_temp <- ful_temp %>% 
  mutate(season = o)


glimpse(ful_temp)
# -----------------------START GAMMS -------------------------------
m <- bam(mean_temp ~ hour * season * fish_basin + 
           s(hour, by = fish_basin, 
             bs = "cc", k = 10) +
           s(hour, by = season, bs = "cc", k = 10) +
           s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             k = c(20, 4)), 
         method = "fREML", 
         family = Gamma(link = "log"),
         data = ful_temp, 
         select = TRUE
)


# check model fit -----
par(mfrow = c(2, 2))
gam.check(m)


plot(m)
summary(m)
# look at overall effect terms -----
m_overall <- anova.gam(m, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m_overall$pTerms.table %>% 
  as_tibble(rownames = "term") %>% 
  clean_names()
# grab inddial effect 
ind_parm <- tidy(m, parametric = TRUE) %>% 
  clean_names()
# smoother effect 
smoothers <- tidy(m) %>% 
  clean_names()

# model comparison and fit info 
m_glance <- glance(m) %>% 
  clean_names()


# view all model info ----

overall_parm
ind_parm
smoothers
m_glance

# =---- save summaries 

# overall_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp Hour results",
#                                   "gamm_temp_h_param_overall.xlsx"))
# ind_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp Hour results",
#                                   "gamm_temp_h_param_ind.xlsx"))
# 
# smoothers %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp Hour results",
#                                   "gamm_temp_h_smoothers.xlsx"))
# m_glance %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp Hour results",
#                                   "gamm_temp_h_model_fit.xlsx"))
# # pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_temp %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m, newdata = dat_2, 
                    type = "response", se = TRUE, 
                    exclude = c("s(floy_tag, year)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit) %>% 
  arrange(floy_tag, hour)

# double check that predicts looks correct 
glimpse(predicts) 




# plotting prep ------

rect_sunrise <- tibble(
  tod = rep("Daylight", 4), 
  season = factor(c("Spring", "Summer", 
                    "Fall", "Winter"), 
                  levels = c("Spring", "Summer", 
                             "Fall", "Winter")), 
  xmin = c(5.3, 5 + (7/30), 6.4, 6 + (2/3)), 
  xmax = c(19 + (43/60), 20.9, 19 + (2/3), 17.8),
  ymin = -Inf,
  ymax = Inf
)


# ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
ggplot(predicts) +
  
  geom_rect(data = rect_sunrise, aes(xmin = xmin,
                                     xmax = xmax,
                                     ymin = ymin,
                                     ymax = ymax),
            fill = "grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_text(data = rect_sunrise,
            aes(
              x = xmin + (xmax - xmin) / 2 - 0.5,
              y = 9.25, label = tod),
            size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  stat_summary(data = ful_temp, fun = mean, geom = "point", 
               aes(x = hour, y = mean_temp, 
                   colour = fish_basin), alpha = 0.5,
               # position = position_dodge(0.95),
               position = position_jitter(0.25),
               size = 3
  ) +
  geom_line(
    aes(x = hour, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = hour, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.4, end = 0.8) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.4, end = 0.8) +
  # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  
  facet_rep_wrap(. ~ season, ncol = 2, repeat.tick.labels = TRUE) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.04, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Time of Day (h)",
       y = "Temperature (°C)") -> p 

# p
write_rds(p, here("Plot Objects", 
                  "hourly_temp_GAMM_plots.rds"))


ggsave(plot = p, filename = here("Plots",
                                 "hour_temp_mean_season_basin_gamm.png"),
       # height = 4.34,
       height = 5.2 * 1.5,
       # width = 8.34 * 2,
       width = 10 * 1.5
)


# ---------- plot per invidual -------------

dat_3 <- ful_temp %>% 
  mutate(
    # floy_tag = "a",
    # year = 0, 
  )

glimpse(dat_3)

# use prediction to get interpolated points 
fits_id <- predict.bam(m, newdata = dat_3, 
                       type = "response", se = TRUE, 
                       # exclude = c("s(floy_tag)"),
                       newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts_id <- data.frame(dat_3, fits_id) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit) %>% 
  arrange(floy_tag, hour)

# double check that predicts looks correct 
glimpse(predicts_id) 

glimpse(ful)



# ---------- plot hour gamm for 2017 - 2020 with mean hour temp per id  ------
ggplot(data = predicts_id) +
  geom_point(data = ful_temp, aes(x = hour, y = mean_temp,
                                  colour = floy_tag), alpha = 0.5,
             # position = position_jitter(width = 0.25), 
             size = 3) +
  geom_line(aes(x = hour, y = fit,
                colour = floy_tag),
            size = 1)  +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  x = hour, y = fit,
                  fill = floy_tag),
              alpha = 0.25) +
  scale_colour_viridis_d(begin = 0.2, end = 0.85, 
                         option = "B", name = "Fish ID") + 
  scale_fill_viridis_d(begin = 0.2, end = 0.85, 
                       option = "B", name = "Fish ID") + 
  facet_rep_grid(fish_basin ~ season, 
                 # ncol = 2,
                 repeat.tick.labels = TRUE) +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", 
                                 size = 12), 
        axis.title = element_text(size = 15), 
        legend.title = element_text(size = 15, hjust = 0.5), 
        legend.text = element_text(size = 15, hjust = 0.5), 
        # legend.position = c(0.95, 0.93),
        # legend.position = c(0.06, 0.925), 
        legend.background = element_blank(), 
        strip.text = element_text(size = 15)) + 
  labs(x = "Time of Day (h)", 
       y = "Daily Temperature (°C)") -> p1 
p1 

ggsave(plot = p1, filename = here("Plots",
                                  "hour_temp_mean_season_basin_gamm_id.png"),
       # height = 4.34,
       height = 5.2 * 3,
       # width = 8.34 * 2,
       width = 10 * 2
)




