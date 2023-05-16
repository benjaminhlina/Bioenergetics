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



ful_rmr <- ful %>% 
  filter(sensor_unit %in% "m/s²" & year %in% c(2019, 2020)) %>%
  filter(!m_swim == is.nan(m_swim)) %>% 
  mutate(floy_tag = factor(floy_tag), 
         fish_basin = factor(
           stringr::str_replace(as.character(fish_basin), " Basin", ""), 
           levels = c("East", "West", "North")) ) %>%
  group_by(floy_tag, hour, year, fish_basin, season, 
           sensor_unit, labels) %>% 
  summarise(mean_rmr = mean(m_swim)) %>% 
  ungroup() %>% 
  arrange(year)
# remove big objects to free up RAM -----

gc()


# evualted for normallity and hetroscadistitiy ----
descdist(ful_rmr$mean_rmr)
ggplot(data = ful_rmr, aes(x = mean_rmr)) + 
  geom_histogram()


norm <- fitdist(ful_rmr$mean_rmr, distr = "norm", method = "mle")
plot(norm)

ful_rmr <- ful_rmr %>% 
  mutate(hour_f = as.factor(hour))

# -----------------------START lmew -------------------------------
m <- lmer(mean_rmr ~ fish_basin * season * hour_f + (1 | floy_tag), 
             data = ful_rmr
)


m1 <- update(m, . ~ fish_basin * season + (1 | floy_tag), data = ful_rmr)
m2 <- update(m, . ~ fish_basin * hour_f + (1 | floy_tag), data = ful_rmr)
m3 <- update(m, . ~ hour_f * season + (1 | floy_tag), data = ful_rmr)
m4 <- update(m, . ~ hour_f + (1 | floy_tag), data = ful_rmr)
m5 <- update(m, . ~ season + (1 | floy_tag), data = ful_rmr)
m6 <- update(m, . ~ fish_basin + (1 | floy_tag), data = ful_rmr)

car::Anova(m)

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
                                  "RMR hour results",
                                  "lmer_model_selection_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


main_effects %>%
openxlsx::write.xlsx(here::here("results",
                                "RMR hour results",
                                "lmer_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR hour results",
                                  "lmer_ind_effects_m_basin_season.xlsx"))


# multiple comparissions ----

multi_comp_fs <- emmeans(m, pairwise ~ fish_basin * season , 
                      adjust = "tukey", type = "response", 
                      pbkrtest.limit = 3353, 
                      lmerTest.limit = 3353)
contrast(multi_comp_fs)
contrast_effects_fs <- contrast(multi_comp_fs, method = "pairwise")


basin_season_contrast_fs <- tidy(contrast_effects_fs) %>% 
  clean_names() %>% 
  arrange(adj_p_value)


basin_season_contrast_fs
basin_season_contrast_fs %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR hour results",
                                  "lmer_multi_comp_basin_season.xlsx"))



multi_comp_hs <- emmeans(m, pairwise ~ hour_f * season , 
                      adjust = "tukey", type = "response", 
                      pbkrtest.limit = 3353, 
                      lmerTest.limit = 3353)
contrast(multi_comp_hs)

rm(ful)
gc()


contrast_effects_hs <- contrast(multi_comp_hs, method = "pairwise", )


basin_season_contrast_hs <- tidy(contrast_effects_hs) %>% 
  clean_names() %>% 
  arrange(adj_p_value)

beepr::beep()


basin_season_contrast_hs
basin_season_contrast_hs %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR hour results",
                                  "lmer_multi_comp_hour_season.xlsx"))








ggplot(data = ful_rmr, aes(x = hour, y = mean_rmr)) +
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

                   x = hour, y = mean_rmr)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9),
               aes(x = hour,
                   group = fish_basin,
                   y = mean_rmr)) +
  # facet_rep_wrap(. ~ season,
  #                ncol = 2,
  #                repeat.tick.labels = TRUE) +
  scale_y_continuous(breaks = seq(40, 160, 20), 
                     # limits = c(40, 165)
                     ) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        # strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.background = element_blank(), 
        legend.position = c(0.08, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Time of Day (h)",
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p
p


ggsave(plot = p, filename = here("plots",
                                  "rmr_hour_basin_violin.png"),
       width = 11 * 1.5,
       height = 7 * 1.25 )

ggplot(data = ful_rmr, aes(x = hour, y = mean_rmr)) +
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

                   x = hour, y = mean_rmr)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9),
               aes(x = hour,
                   group = season,
                   y = mean_rmr)) +
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
  labs(x = "Season",
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1

# p1



ggsave(plot = p1, filename = here("plots",
                                  "rmr_hour_season_violin.png"),
       width = 11 * 1.5,
       height = 7 * 1.25 )
