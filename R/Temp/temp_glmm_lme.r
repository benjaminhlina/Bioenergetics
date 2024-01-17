# load packages ----
{
library(broom.mixed)
library(dplyr)
library(DHARMa)
library(emmeans)
library(fitdistrplus)
library(ggplot2)
library(ggh4x)
library(glmmTMB)
library(here)
library(lubridate)
library(lemon)
library(janitor)
library(lme4)
library(multcomp)
library(openxlsx)
library(patchwork)
library(purrr)
library(readr)
library(tibble)
library(tidyr)
  source(here("R", 
              "Cleaning and Calculations", 
              "julian_date_reorder.r"))
}
# bring in RDS -----

ful <- read_rds(here("Saved Data", 
                     "BioE_lt.rds"))

glimpse(ful)
##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------



ful_temp <- ful %>% 
  filter(sensor_unit %in% "°C") %>%
  
  group_by(floy_tag, date, fish_basin, 
           week, month, season, year,
           sensor_unit, labels) %>% 
  summarise(mean_temp = mean(sensor_value)) %>% 
  ungroup() %>% 
  mutate(date_2 = as.numeric(date), 
         floy_tag = factor(floy_tag)) %>%
  filter(date_2 <= 18559) %>% 
  mutate(fish_basin = factor(stringr::str_replace(
    as.character(fish_basin), " Basin", ""), 
                             levels = c("East", "West", "North")),
         doy = yday(date), 
         month = factor(month, 
                        levels = c("May", "June", "July", 
                                   "August", "September", "October",
                                   "Novemeber", "December", "January",
                                   "February", "March", "April")),
    season = forcats::fct_relevel(season, "Winter", "Spring", "Summer", 
                                  "Fall")) %>% 
  arrange(month) %>%
  mutate(doy_id = days(date))
# remove big objects to free up RAM -----
rm(ful)
gc()


# evualted for normallity and hetroscadistitiy ----
descdist(ful_temp$mean_temp)
ggplot(data = ful_temp, aes(x = mean_temp)) + 
  geom_histogram()


norm <- fitdist(ful_temp$mean_temp, distr = "norm", method = "mle")
plot(norm)

gammas <- fitdist(ful_temp$mean_temp, distr = "gamma", method = "mme")
plot(gammas)


# -----------------------START lmew -------------------------------
m <- glmmTMB(mean_temp ~ fish_basin * season + (1 | floy_tag) + (1 | year), 
             data = ful_temp, 
             family = Gamma(link = "log"), REML = TRUE,
             )
m1 <- glmmTMB(mean_temp ~ fish_basin * season + (1 | floy_tag) + (1 | year), 
             data = ful_temp, 
             family = Gamma(link = "log"), REML = FALSE,
             )

res <- simulateResiduals(m)
plot(res)
res_m1 <- simulateResiduals(m1)
plot(res_m1)

car::Anova(m)
car::Anova(m1)


# model choosen is M1 ------

main_effects <- tidy(car::Anova(m1))

ind_effects <- tidy(m1)

glance(m)
glance(m1)

model_fit <- glance(m1)
# main_effects %>%
# openxlsx::write.xlsx(here::here("results",
#                                 "Temp results",
#                                 "lmer_main_effect_m1_basin_season_temp.xlsx"))
# ind_effects %>%
# openxlsx::write.xlsx(here::here("results",
#                                 "Temp results",
#                                 "lmer_ind_effects_m1_basin_season_temp.xlsx"))
# 
# model_fit %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "Temp results",
#                                   "lmer_model_fit_m1_basin_season_temp.xlsx"))
# multiple comparissions ----

multi_comp <- emmeans(m1, pairwise ~ fish_basin * season, 
                      adjust = "Tukey", type = "response",)
contrast(multi_comp)
contrast_effects <- contrast(multi_comp, method = "pairwise")


basin_season_contrast <- tidy(contrast_effects) %>% 
  clean_names() %>% 
  arrange(adj_p_value)


print(basin_season_contrast, n = 66)

basin_season_contrast %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Temp results",
                                  "lmer_multi_comp_basin_season_temp.xlsx"))




sums <- ful_temp %>% 
  group_by(fish_basin, season) %>% 
  summarise(temp_mean = mean(mean_temp), 
            sem = sd(mean_temp) / sqrt(n())
            ) %>% 
  ungroup() %>% 
  arrange(season, fish_basin)


sums



# ----------------boxplot season basin temp usage -----
ggplot(data = ful_temp, aes(x = season, y = mean_temp)) + 
  geom_boxplot(aes(fill = fish_basin), alpha = 0.5
               ) + 
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = "Daily Temperature (°C)") -> p1

p1
# ggsave(p1, filename = here("Plots",
#                            "temp_season_boxplot.png"),
#        height = 7, width = 11)

ggplot(data = ful_temp, aes(x = season, y = mean_temp)) +
  geom_violin(aes(fill = fish_basin), alpha = 0.5
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 2, position = position_dodge(0.9), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   
                   x = season, y = mean_temp)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = mean_temp)) +
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = "Daily Temperature (°C)") -> p2

p2

write_rds(p2, here("Plot Objects",  
                   "daily_temp_GLMM_violin_plot.rds"))
# ggsave(plot = p1, filename = here("plots",
#                                   "gamm_BioE_season_basin_boxplot.png"),
#        width = 11,
#        height = 7 )
ggsave(plot = p2, filename = here("plots",
                                  "Violin Plots", 
                                  "temp_season_basin_violin.png"),
       width = 11,
       height = 7 )


glimpse(ful_temp)

ggplot(data = ful_temp, aes(x = season, y = mean_temp)) +
  geom_boxplot(
    aes(fill = fish_basin),
    width = 0.15,
    position = position_dodge(0.9),
    outlier.colour = NA
  ) + 
  ggdist::stat_halfeye(
    aes(fill = fish_basin),
    position = position_dodge(0.9), 
    adjust = 0.5, 
    # outline_bars = TRUE,
    slab_color = "black", 
    slab_size = 0.5, 
    width = 0.7, 
    .width = 0, 
    justification = -0.2, 
    point_colour = NA
  ) + 
  
  stat_summary(fun = mean, 
               geom = "point",  
               size = 2, position = position_dodge(0.9), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   
                   x = season, y = mean_temp)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = mean_temp)) +
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75, 
                       alpha = 0.5) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = "Daily Temperature (°C)") -> p3 
p3
write_rds(p3, here("Plot Objects",
                   "daily_temp_GLMM_raincloud_plot.rds"))


ggsave(plot = p3, filename = here("plots",
                                  "Violin Plots",
                                  "temp_season_basin_raincloud.png"),
       width = 11,
       height = 7 )


