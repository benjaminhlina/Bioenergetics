# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(emmeans)
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
  source(here("R", 
              "Cleaning and Calculations", 
              "julian_date_reorder.r"))
}
# bring in RDS -----

m_tot <- read_rds(here(
  "Saved Data", 
  "Daily_m_total.rds"))

m_tot <- m_tot %>% 
  mutate(season = forcats::fct_relevel(season, 
                                       "Spring", "Summer", 
                                       "Fall", "Winter"))
# look at distibution ------
ggplot(data = m_tot, aes(x = m_total)) + 
  geom_histogram() 

descdist(m_tot$m_total)

norm <- fitdist(m_tot$m_total, distr = "norm", method = "mle")
plot(norm)


# ------ lme ------
m <- lm(mean_rmr ~ fish_basin * season, data = m_tot)
m1 <- update(m, . ~ fish_basin)

m2 <- update(m, . ~ season)



# check model fit ----
par(mfrow = c(2, 2))

plot(m)

summary(m)
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
                                  "Total Metabolism",
                                  "lm_model_selection_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)




main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>% 
main_effects %>% 
  openxlsx::write.xlsx(here::here("results",
                                  "Total Metabolism",
                                  "lm_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Total Metabolism",
                                  "lm_ind_effects_m_basin_season.xlsx"))

# multiple comparissions ----

multi_comp <- emmeans(m, pairwise ~ fish_basin * season, 
                      adjust = "tukey", type = "response", 
                      # pbkrtest.limit = 3353, 
                      # lmerTest.limit = 3353
)
contrast(multi_comp)
contrast_effects <- contrast(multi_comp, method = "pairwise")


basin_season_contrast <- tidy(contrast_effects) %>% 
  clean_names() %>% 
  arrange(adj_p_value)


basin_season_contrast
basin_season_contrast %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Total Metabolism",
                                  "lm_multi_comp_basin_season.xlsx"))



glimpse(m_tot)


summs <- m_tot %>% 
  group_by(fish_basin, season) %>% 
  summarise(means = mean(m_total), 
            sem = sd(m_total) / sqrt(n())) %>% 
  ungroup() %>% 
  arrange(season, fish_basin) 

summs
# ----- plot ------
ggplot(data = m_tot, aes(x = season, y = m_total)) +
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
       y = expression(paste("Total Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p

p
# ggsave(plot = p, filename = here("plots",
#                                  "gamm_BioE_season_boxplot.png"), width = 11,
#        height = 7)


ggplot(data = m_tot, aes(x = season, y = m_total)) +
  geom_boxplot(aes(fill = fish_basin), alpha = 0.25
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 3, position = position_dodge(0.75), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   x = season, y = m_total)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.75), 
               aes(x = season, group = fish_basin, 
                   y = m_total)) +
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
       y = expression(paste("Total Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1

p1



ggplot(data = m_tot, aes(x = season, y = m_total)) +
  geom_violin(aes(fill = fish_basin), alpha = 0.5
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 2, position = position_dodge(0.9), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   
                   x = season, y = m_total)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = m_total)) +
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
       y = expression(paste("Total Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2

p2
# ggsave(plot = p1, filename = here("plots",
#                                   "gamm_BioE_season_basin_boxplot.png"),
#        width = 11,
#        height = 7 )
#        
#        
#        
#        
write_rds(p2, here("plot Objects", 
                   "total_metabolism_plot.rds"))


ggsave(plot = p2, filename = here("plots",
                                  "Violin Plots", 
                                  "Total_Metabolism_season_basin_violin.png"),
       width = 11,
       height = 7 )


glimpse(m_tot)
ggplot(data = m_tot, aes(x = season, y = m_total)) +
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
                   
                   x = season, y = m_total)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = m_total)) +
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
       y = expression(paste("Scope for Activity (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p3 

write_rds(p3, here("Plot Objects",
                   "Total_Metabolism_LM_raincloud_plot.rds"))


ggsave(plot = p3, filename = here("plots",
                                  "Violin Plots",
                                  "Total_Metabolism_season_basin_raincloud.png"),
       width = 11,
       height = 7 )


