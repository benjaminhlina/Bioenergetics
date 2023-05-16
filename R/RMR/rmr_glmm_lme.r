# load function -------


# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(glmmTMB)
  library(fitdistrplus)
  library(ggplot2)
  library(ggh4x)
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
##### SUBSET OUT ONLY RMR/M_Swim DATA and determine daily temp FOR 2017 - 2021-------


ful_rmr <- ful %>% 
  filter(sensor_unit %in% "m/s²" &
           year %in% c(2019, 2020)) %>% 
  filter(!m_swim == is.nan(m_swim)) %>% 
  group_by(floy_tag, fish_basin, date,
           week, month, season, year,
           sensor_unit, labels) %>% 
  summarise(mean_rmr = mean(m_swim)) %>% 
  ungroup() %>%  
  mutate(
    floy_tag = factor(floy_tag), 
    fish_basin = factor(
      stringr::str_replace(as.character(fish_basin), " Basin", ""), 
      levels = c("East", "West", "North")), 
    doy = yday(date), 
    month = factor(month, 
                   levels = c("May", "June", "July", 
                              "August", "September", "October",
                              "Novemeber", "December", "January",
                              "February", "March", "April")), 
    doy_id = days(date, end = "05-22"), 
    season = forcats::fct_relevel(season, "Spring", "Summer", 
                                  "Fall", "Winter")
  ) %>% 
  arrange(date) %>% 
  dplyr::select(floy_tag:sensor_unit, doy, doy_id, mean_rmr)
glimpse(ful_rmr)
# remove big objects to free up RAM -----
# rm(ful)
gc()

# look at data to see what dist it fits 

descdist(ful_rmr$mean_rmr) 

norm <- fitdist(ful_rmr$mean_rmr, distr = "norm", method = "mle")
plot(norm)
gammas <- fitdist(ful_rmr$mean_rmr, distr = "gamma", method = "mme")
plot(gammas)

ggplot(data = ful_rmr, aes(x = mean_rmr)) + 
  geom_histogram()




# ------ lme ------
m <- glmmTMB(mean_rmr ~ fish_basin * season + (1 | floy_tag), 
             data = ful_rmr, 
             family = gaussian(link = "identity")
)
m1 <- update(m, . ~ fish_basin + (1 | floy_tag),  REML = FALSE)

m2 <- update(m, . ~ season + (1 | floy_tag),  REML = FALSE)



# check model fit ----

res <- simulateResiduals(m)
plot(res)
residuals(res)

res_m1 <- simulateResiduals(m1)
plot(res_m1)
res_m2 <- simulateResiduals(m2)
plot(res_m2)



# create model list for model selection ------
model_list <- list(m, m1, m2
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2"
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
                                  "RMR results",
                                  "lmer_model_selection_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>% 
main_effects %>% 
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_ind_effects_m_basin_season.xlsx"))

# multiple comparissions ----

multi_comp <- emmeans(m, pairwise ~ fish_basin * season, 
                      adjust = "Tukey", type = "response", 
                      pbkrtest.limit = 3353, 
                      lmerTest.limit = 3353)
contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects <- contrast(multi_comp, method = "pairwise",
                             adjust = "Tukey")

basin_season_contrast <- tidy(contrast_effects) %>% 
  clean_names() %>% 
  arrange(adj_p_value, contrast) 


basin_season_contrast


print(basin_season_contrast, n = 66)


compares <- cld(
  object = multi_comp,
  adjust = "Tukey",
  Letters = letters,
  alpha = 0.05, 
)
compares$.group


basin_season_contrast %>%
  # filter(adj_p_value < 0.05) %>% 
  arrange(contrast, adj_p_value) %>%  
  
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_multi_comp_basin_season.xlsx"))



sums <- ful_rmr %>% 
  group_by(season, fish_basin) %>% 
  summarise(means = mean(mean_rmr), 
            sem = sd(mean_rmr) / sqrt(n())) %>% 
  ungroup() %>% 
  arrange(season, fish_basin)

sums

# ----- plot ------
ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_boxplot(alpha = 0.5, width = 0.35
  ) +
  
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
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
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p

p
# ggsave(plot = p, filename = here("plots",
#                                  "gamm_BioE_season_boxplot.png"), width = 11,
#        height = 7)


ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_boxplot(aes(fill = fish_basin), alpha = 0.25
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 3, position = position_dodge(0.75), 
               aes(colour = fish_basin, 
                   x = season, y = mean_rmr)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.75), 
               aes(x = season, group = fish_basin, 
                   y = mean_rmr)) +
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
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1

p1



ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_violin(aes(fill = fish_basin), alpha = 0.5
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 2, position = position_dodge(0.9), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   
                   x = season, y = mean_rmr)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = mean_rmr)) +
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
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2

p2
# ggsave(plot = p1, filename = here("plots",
#                                   "gamm_BioE_season_basin_boxplot.png"),
#        width = 11,
#        height = 7 )
#      
#      
#      


write_rds(p2, here("Plot Objects",
                   "daily_rmr_GLMM_violin_plot.rds"))


ggsave(plot = p2, filename = here("plots",
                                  "violin plots",
                                  "gamm_BioE_season_basin_violin.png"),
       width = 11,
       height = 7 )


ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_boxplot(
    aes(fill = fish_basin),
    width = 0.15,
    position = position_dodge(0.9), 
    outlier.colour = NA
  ) + 
  # geom_violin(aes(fill = fish_basin), alpha = 0.5
  # ) +
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
                   
                   x = season, y = mean_rmr)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = mean_rmr)) +
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
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p3 

write_rds(p3, here("Plot Objects",
                   "daily_rmr_GLMM_raincloud_plot.rds"))


ggsave(plot = p3, filename = here("plots",
                                  "Violin Plots",
                                  "gamm_BioE_season_basin_raincloud.png"),
       width = 11,
       height = 7 )

