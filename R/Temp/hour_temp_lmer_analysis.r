# load packages ----

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

# bring in RDS -----

ful <- read_rds(here("Saved Data", 
                     "BioE_lt_hour.rds"))

glimpse(ful)
##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------



ful_temp <- ful %>% 
  filter(sensor_unit %in% "°C") %>%
  mutate(floy_tag = factor(floy_tag), 
         fish_basin = factor(
           stringr::str_replace(as.character(fish_basin), " Basin", ""), 
           levels = c("East", "West", "North")) ) %>%
  group_by(floy_tag, hour, year, fish_basin, season, 
           sensor_unit, labels) %>% 
  summarise(mean_temp = mean(sensor_value)) %>% 
  ungroup() %>% 
  arrange(year)

glimpse(ful_temp)
# remove big objects to free up RAM -----

gc()


# evualted for normallity and hetroscadistitiy ----
descdist(ful_temp$mean_temp)
ggplot(data = ful_temp, aes(x = mean_temp)) + 
  geom_histogram()


norm <- fitdist(ful_temp$mean_temp, distr = "norm", method = "mle")
plot(norm)

ful_temp <- ful_temp %>% 
  mutate(hour_f = as.factor(hour))

# -----------------------START lmew -------------------------------
m <- glmmTMB(mean_temp ~ fish_basin * season * hour_f + (1 | floy_tag), 
          data = ful_temp, family = Gamma(link = "log"), REML = TRUE
)


m1 <- update(m, . ~ fish_basin * season + (1 | floy_tag), data = ful_temp)
m2 <- update(m, . ~ fish_basin * hour_f + (1 | floy_tag), data = ful_temp)
m3 <- update(m, . ~ hour_f * season + (1 | floy_tag), data = ful_temp)
m4 <- update(m, . ~ hour_f + (1 | floy_tag), data = ful_temp)
m5 <- update(m, . ~ season + (1 | floy_tag), data = ful_temp)
m6 <- update(m, . ~ fish_basin + (1 | floy_tag), data = ful_temp)

car::Anova(m)
res <- simulateResiduals(m)
plot(res)
res_m1 <- simulateResiduals(m1)
plot(res_m1)






# create model list for model selection ------
model_list <- list(m, m1, m2, m3, m4, m5, m6
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2", "m3", "m4", 
                       "m5", "m6"
)
glance(m)

# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)

glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>% 
  mutate(model = lapply(model_list, formula) %>%
           as.character() 
  ) %>% 
  dplyr::select(model, id:df.residual) %>% 
  arrange(AIC)


# view model selection ------ 
glance_summary
glance_summary %>% 
  openxlsx::write.xlsx(here::here("results",
                                  "temp hour results",
                                  "lmer_model_selection_hour_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


main_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "temp hour results",
                                  "lmer_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "temp hour results",
                                  "lmer_ind_effects_m_basin_season.xlsx"))


# multiple comparissions ----

multi_comp_fs <- emmeans(m, pairwise ~ fish_basin * season , 
                         adjust = "tukey", type = "response", 
                         pbkrtest.limit = 5802, 
                         lmerTest.limit = 5802)
contrast(multi_comp_fs)
contrast_effects_fs <- contrast(multi_comp_fs, method = "pairwise")


basin_season_contrast_fs <- tidy(contrast_effects_fs) %>% 
  clean_names() %>% 
  arrange(adj_p_value)


basin_season_contrast_fs
basin_season_contrast_fs %>%
  openxlsx::write.xlsx(here::here("results",
                                  "temp hour results",
                                  "lmer_multi_comp_basin_season.xlsx"))

rm(ful)
gc()

multi_comp_hs <- emmeans(m, pairwise ~ hour_f * season , 
                         adjust = "tukey", type = "response", 
                         pbkrtest.limit = 5802, 
                         lmerTest.limit = 5802)
contrast(multi_comp_hs)




contrast_effects_hs <- contrast(multi_comp_hs, method = "pairwise", )


basin_season_contrast_hs <- tidy(contrast_effects_hs) %>% 
  clean_names() %>% 
  arrange(adj_p_value)

beepr::beep()


basin_season_contrast_hs
basin_season_contrast_hs %>%
  openxlsx::write.xlsx(here::here("results",
                                  "temp hour results",
                                  "lmer_multi_comp_hour_season.xlsx"))




glimpse(ful_temp)
sums <- ful_temp %>% 
  group_by(season) %>% 
  summarise(mean = mean(mean_temp), 
            sem = sd(mean_temp) / sqrt(n())) %>% 
  ungroup() %>% 
  arrange(season) 

sums
 
# ---- violin plot -----







ggplot(data = ful_temp, aes(x = hour, y = mean_temp)) +
  geom_violin(aes(fill = fish_basin, group = interaction(hour, fish_basin)),
              alpha = 0.5
  ) +
  stat_summary(fun = mean,
               geom = "point",
               size = 2, position = position_dodge(0.9),
               colour = "black",
               aes(
                 group = fish_basin,
                 colour = fish_basin,
                 
                 x = hour, y = mean_temp)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9),
               aes(x = hour,
                   group = fish_basin,
                   y = mean_temp)) +
  # facet_rep_wrap(. ~ season,
  #                ncol = 2,
  #                repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        # strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.background = element_blank(), 
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Time of Day (h)",
       y = "Daily Temperature (°C)") -> p
p


ggsave(plot = p, filename = here("plots",
                                 "temp_hour_basin_violin.png"),
       width = 11 * 1.5,
       height = 7 * 1.25 )



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

  


ggplot(data = ful_temp, aes(x = hour, y = mean_temp)) +
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
              y = 13.25, label = tod),
            size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_violin(aes(fill = season, group = interaction(hour, season)),
              alpha = 0.5
  ) +
  stat_summary(fun = mean,
               geom = "point",
               size = 2, position = position_dodge(0.9),
               colour = "black",
               aes(
                 group = season,
                 colour = season,
                 
                 x = hour, y = mean_temp)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9),
               aes(x = hour,
                   group = season,
                   y = mean_temp)) +
  facet_rep_wrap(. ~ season,
                 ncol = 2,
                 repeat.tick.labels = TRUE) +
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Season",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Season",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        # strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.90),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Time of Day (h)",
       y = "Daily Temperature (°C)") -> p1

p1

write_rds(p1, here("Plot Objects", 
                   "hourly_temp_GLMM_violin_plot.rds"))


ggsave(plot = p1, filename = here("plots",
                                  "temp_hour_season_violin.png"),
       width = 11 * 1.5,
       height = 7 * 1.25 )
