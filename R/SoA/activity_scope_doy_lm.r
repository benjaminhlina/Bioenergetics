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

rmr <- read_rds(here("Saved Data", 
                     "Daily_RMR.rds")) %>% 
  arrange(date)

smr <- read_rds(here("Saved Data", 
                     "Daily_SMR.rds")) %>% 
  arrange(date)

# view dataframes 
glimpse(rmr)
glimpse(smr)


# ----- determine daily mean for smr and rmr -----
rmr_sum <- rmr %>%
  group_by(fish_basin, date, doy, week, month, season, year) %>% 
  summarise(mean_rmr = mean(mean_rmr)) %>% 
  group_by()

smr_sum <- smr %>%
  group_by(fish_basin, date, doy, week, month, season, year) %>% 
  summarise(mean_smr = mean(mean_smr), 
            mean_temp = mean(mean_temp)) %>% 
  group_by()

glimpse(smr_sum)
glimpse(rmr_sum)

tail(smr)
tail(rmr)


# -------- calculate activity scope -------
fs <- smr_sum %>% 
  left_join(rmr_sum, by = c("fish_basin", 
                            "date",
                            # "doy_id", 
                            "doy", 
                            "week", "month",
                            "season",
                            "year"
  ))


# cacluate activity scope 

fs <- fs %>% 
  mutate(fs = mean_rmr - mean_smr, 
         doy_id = days(date), 
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec", "Jan",
                                       "Feb", "Mar", "Apr")), 
         season = forcats::fct_relevel(season, "Spring", "Summer", 
                                       "Fall", "Winter")) %>% 
  filter(fs > 0)

fs %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_labels


glimpse(fs)
month_labels
# look at distibution ------
ggplot(data = fs, aes(x = fs)) + 
  geom_histogram() 

descdist(fs$fs)

norm <- fitdist(fs$fs, distr = "norm", method = "mle")
plot(norm)


# ------ lme ------
m <- lm(mean_rmr ~ fish_basin * season, data = fs)
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
                                  "activity Scope",
                                  "lm_model_selection_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)




main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>% 
main_effects %>% 
  openxlsx::write.xlsx(here::here("results",
                                  "activity Scope",
                                  "lm_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "activity Scope",
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
                                  "activity Scope",
                                  "lm_multi_comp_basin_season.xlsx"))



glimpse(fs)


summs <- fs %>% 
  group_by(fish_basin, season) %>% 
  summarise(means = mean(fs), 
            sem = sd(fs) / sqrt(n())) %>% 
  ungroup() %>% 
  arrange(season, fish_basin) 

summs
# ----- plot ------
ggplot(data = fs, aes(x = season, y = fs)) +
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
       y = expression(paste("Scope for Activity (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p

p
# ggsave(plot = p, filename = here("plots",
#                                  "gamm_BioE_season_boxplot.png"), width = 11,
#        height = 7)


ggplot(data = fs, aes(x = season, y = fs)) +
  geom_boxplot(aes(fill = fish_basin), alpha = 0.25
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 3, position = position_dodge(0.75), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   x = season, y = fs)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.75), 
               aes(x = season, group = fish_basin, 
                   y = fs)) +
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
       y = expression(paste("Swimming Activity (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1

p1



ggplot(data = fs, aes(x = season, y = fs)) +
  geom_violin(aes(fill = fish_basin), alpha = 0.5
  ) +
  stat_summary(fun = mean, 
               geom = "point",  
               size = 2, position = position_dodge(0.9), 
               colour = "black", 
               aes(group = fish_basin,
                   # colour = fish_basin, 
                   
                   x = season, y = fs)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = fs)) +
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
       y = expression(paste("Swimming Activity (mg",
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
                   "scope_for_activity_lm_plot.rds"))


ggsave(plot = p2, filename = here("plots",
                                  "Violin Plots", 
                                  "SoA_season_basin_violin.png"),
       width = 11,
       height = 7 )


glimpse(fs)
ggplot(data = fs, aes(x = season, y = fs)) +
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
                   
                   x = season, y = fs)) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9), 
               aes(x = season, group = fish_basin, 
                   y = fs)) +
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
       y = expression(paste("Swimming Activity (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p3 

write_rds(p3, here("Plot Objects",
                   "SoA_LM_raincloud_plot.rds"))


ggsave(plot = p3, filename = here("plots",
                                  "Violin Plots",
                                  "SoA_season_basin_raincloud.png"),
       width = 11,
       height = 7 )


