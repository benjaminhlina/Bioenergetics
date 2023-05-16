# load packages ----

{
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
}
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
           levels = c("East", "West", "North")), 
         season = forcats::fct_relevel(season, "Spring", "Summer", 
                                       "Fall", "Winter")) %>%
  group_by(floy_tag,fish_basin, hour, season, year,  
           sensor_unit, labels) %>% 
  summarise(mean_rmr = mean(m_swim)) %>% 
  ungroup() %>% 
  arrange(year)

ful_rmr_nd <- ful %>% 
  filter(sensor_unit %in% "m/s²" & year %in% c(2019, 2020)) %>%
  filter(!m_swim == is.nan(m_swim)) %>% 
  mutate(floy_tag = factor(floy_tag), 
         fish_basin = factor(
           stringr::str_replace(as.character(fish_basin), " Basin", ""), 
           levels = c("East", "West", "North")), 
         season = forcats::fct_relevel(season, "Spring", "Summer", 
                                       "Fall", "Winter")) %>%
  group_by(floy_tag,fish_basin, hour, day_night, season, year,  
           sensor_unit, labels) %>% 
  summarise(mean_rmr = mean(m_swim)) %>% 
  ungroup() %>% 
  arrange(year)
# remove big objects to free up RAM -----
gc()

glimpse(ful_rmr)

# 
write_rds(ful_rmr, here("Saved Data",
                        "hourly_RMR.rds"))

# look at hour distribuation -----
descdist(ful_rmr$mean_rmr)
ggplot(data = ful_rmr, aes(x = mean_rmr)) + 
  geom_histogram()



gammas <- fitdist(ful_rmr$mean_rmr, distr = "gamma", method = "mme")
plot(gammas)


glimpse(ful_rmr)
length(unique(ful_rmr$floy_tag))
# -----------------------START GAMMS -------------------------------
m <- bam(mean_rmr ~ hour * season * fish_basin + 
           s(hour, by = fish_basin, 
             bs = "cc", k = 10) +
           s(hour, by = season, bs = "cc", k = 10) +
           s(floy_tag, by = fish_basin, bs = c("re"), 
             k = c(16)), method = "fREML",
         family = Gamma(link = "log"),
         data = ful_rmr, 
         select = TRUE
)


# check model fit -----
par(mfrow = c(2, 2))
gam.check(m)

# 
# plot(m)
# # look at overall effect terms -----
m_overall <- anova.gam(m, freq = FALSE)

# grab parametic overall effect
overall_parm <- m_overall$pTerms.table %>%
  as_tibble(rownames = "terms") %>%
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


# # view all model info ----

overall_parm
ind_parm
smoothers
m_glance

# =---- save summaries 

overall_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR Hour results",
                                  "gamm_hour_param_overall.xlsx"))
ind_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR hour results",
                                  "gamm_hour_param_ind.xlsx"))

smoothers %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR Hour results",
                                  "gamm_hour_smoothers.xlsx"))
m_glance %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR Hour results",
                                  "gamm_hour_model_fit.xlsx"))
# # pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_rmr %>% 
  mutate(
    floy_tag = "a",
    # year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m, newdata = dat_2, 
                    type = "response", se = TRUE, 
                    exclude = c("s(floy_tag)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit) %>% 
  arrange(floy_tag, hour)

# double check that predicts looks correct 
glimpse(predicts) 

glimpse(ful)
# calculate daily mean temp by fish basin 
ful_rmr %>%
  group_by(hour, season, fish_basin) %>%
  summarise(mean_rmr = mean(mean_rmr)) %>%
  ungroup() -> mean_rmr
ful_rmr
# # create month labels 
# predicts %>% 
#   filter(doy_id %in% seq(25, 350, 65)) %>% 
#   group_by(year, month_abb) %>% 
#   summarise() %>% 
#   .$month_abb -> month_label 
# month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 


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
  # 
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
              y = 118.25, label = tod),
            size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  stat_summary(data = ful_rmr, fun = mean, geom = "point", 
               aes(x = hour, y = mean_rmr, 
                   colour = fish_basin), alpha = 0.5,
               # position = position_dodge(0.95),
               position = position_jitter(0.25),
               size = 3
  ) +
  # stat_summary(data = ful_rmr, fun.data = mean_se, geom = "errorbar", 
  #              aes(x = hour, y = mean_rmr, group = fish_basin), alpha = 0.5,
  #            position = position_dodge(width = 0.95),
  #            width = 0.15) +
  # geom_point(data = ful_rmr, aes(x = hour, y = mean_rmr, 
  #                colour = fish_basin), alpha = 0.5,
  #            position = position_jitter(width = 0.25), size = 3) +
  geom_line(aes(x = hour, y = fit,
                colour = fish_basin),
            linewidth = 1)  +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  x = hour, y = fit,
                  fill = fish_basin),
              alpha = 0.25) +
  scale_colour_viridis_d(begin = 0.4, end = 0.8, 
                         option = "B", name = "Basin") + 
  scale_fill_viridis_d(begin = 0.4, end = 0.8, 
                       option = "B", name = "Basin") + 
  
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  theme_bw() + 
  facet_rep_wrap(. ~ season, ncol = 2, repeat.tick.labels = TRUE) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.04, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  # theme(panel.grid = element_blank(), 
  #       axis.text = element_text(colour = "black", 
  #                                size = 12), 
  #       axis.title = element_text(size = 15), 
  #       legend.title = element_text(size = 15, hjust = 0.5), 
  #       legend.text = element_text(size = 15, hjust = 0.5), 
  #       # legend.position = c(0.95, 0.93),
  
  #       legend.background = element_blank(), 
  #       strip.text = element_text(size = 15)) + 
  labs(x = "Time of Day (h)", 
       y = expression(paste("Active Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 
p


ggsave(plot = p, filename = here("Plots",
                                 "Hourly Plots", 
                                 "hour_rmr_mean_season_basin_gamm_est.png"),
       # height = 4.34,
       height = 5.2 * 1.5,
       # width = 8.34 * 2,
       width = 10 * 1.5
)




# ---------------- plot hourly activity per indiidual --------------
# create new datafreame with dummmy variables for RE for plotting 
dat_3 <- ful_rmr %>% 
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
# calculate daily mean temp by fish basin 
# ful_rmr %>%
#   group_by(hour, season, fish_basin) %>%
#   summarise(mean_rmr = mean(mean_rmr)) %>%
#   ungroup() -> mean_rmr
# ful_rmr
# # create month labels 
# predicts %>% 
#   filter(doy_id %in% seq(25, 350, 65)) %>% 
#   group_by(year, month_abb) %>% 
#   summarise() %>% 
#   .$month_abb -> month_label 
# month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 
# predicts %>% 
#   group_by(season) %>% 
#   summarise(first = first(doy_id),
#             last = last(doy_id)) %>% 
#   ungroup()
# 
# rect_summer <- tibble(
#   season = "Summer",
#   xmin = 32,
#   xmax = 123,
#   ymin = -Inf,
#   ymax = Inf
# )
# 
# rect_winter <- tibble(
#   season = "Winter",
#   xmin = 215,
#   xmax = 305,
#   ymin = -Inf,
#   ymax = Inf
# )

# ---------- plot hour gamm for 2017 - 2020 with mean hour temp per id  ------
ggplot(data = predicts_id) +
  # geom_rect(data = rect_summer, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
  #           fill = "grey80",
  #           alpha = 0.75,
  #           inherit.aes = FALSE) +
  # geom_rect(data = rect_winter, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
#           fill ="grey80",
#           alpha = 0.75,
#           inherit.aes = FALSE) +
# geom_text(
#   aes(x = xmin + 25, y = 12.25, label = season),
#   data = rect_summer,
#   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
# geom_text(
#   aes(x = xmin + 30, y = 12.25, label = season),
#   data = rect_winter,
#   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +

geom_point(data = ful_rmr, aes(x = hour, y = mean_rmr,
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
  scale_y_continuous(breaks = seq(0, 200, 25)) +
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
       y = expression(paste("Active Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1 
p1 

ggsave(plot = p1, filename = here("Plots",
                                  "Hourly Plots", 
                                  "hour_rmr_mean_season_basin_gamm_id_est.png"),
       # height = 4.34,
       height = 5.2 * 3,
       # width = 8.34 * 2,
       width = 10 * 2
)


# ---- violin plot -----

# 
# ggplot(data = ful_rmr, aes(x = hour, y = mean_rmr)) +
#   geom_violin(aes(fill = fish_basin, group = interaction(hour, fish_basin)), 
#                   alpha = 0.5
#   ) +
#   stat_summary(fun = mean, 
#                geom = "point",  
#                size = 2, position = position_dodge(0.9), 
#                colour = "black", 
#                aes(
#                  # group = fish_basin,
#                    colour = fish_basin,
#                    
#                    x = hour, y = mean_rmr)) +
#   stat_summary(fun.data = mean_se, 
#                geom = "errorbar", width = 0.15,
#                position = position_dodge(0.9), 
#                aes(x = hour, 
#                    group = fish_basin, 
#                    y = mean_rmr)) +
#   facet_rep_wrap(. ~ season, 
#                  ncol = 2,
#                  repeat.tick.labels = TRUE) +
#   # scale_y_continuous(breaks = seq(15, 20, 2)) +
#   scale_fill_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   scale_colour_viridis_d(name = "Basin",
#                          option = "B", begin = 0.35, end = 0.75) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         # strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         legend.position = c(0.92, 0.93),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Season",
#        y = expression(paste("Active Metabolism (mg", 
#                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2
# p2


glimpse(ful_rmr_nd)
beepr::beep()

p3 <- ggplot(data = ful_rmr_nd, aes(x = day_night, y = mean_rmr)) + 
  geom_boxplot(
    aes(fill = fish_basin),
    width = 0.15,
    position = position_dodge(0.9), 
    outlier.colour = NA
  ) + 
  stat_summary(fun = mean, aes(y = mean_rmr, group = fish_basin),
               geom = "point", size = 2,
               colour = "black",
               position = position_dodge(0.9)) +
  stat_summary(
    aes(x = day_night, y = mean_rmr, group = fish_basin),
    fun.data = mean_se,
    position = position_dodge(0.9),
    geom = "errorbar",
    width = 0.05
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
  
  facet_rep_wrap(. ~ season, 
                 ncol = 2,
                 repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(begin = 0.35, end = 0.75, alpha = 0.5, 
                       option = "B", name = "Basin") +
  scale_x_discrete(labels = c("Day", "Night")) + 
  # scale_y_continuous(breaks = seq(0, 200, 25)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", 
                                 size = 12), 
        axis.title = element_text(size = 15), 
        legend.title = element_text(size = 15, hjust = 0.5), 
        legend.text = element_text(size = 15, hjust = 0.5), 
        # legend.position = c(0.95, 0.93),
        legend.position = c(0.95, 0.38),
        legend.background = element_blank(), 
        strip.text = element_text(size = 15)) + 
  labs(x = "Time of Day", 
       y = expression(paste("Active Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")))

# p3
ggsave(plot = p3, filename = here("Plots", 
                                  "Day and Night Activity", 
                                  "day_night_season_raincloud.png"), 
       width = 11, height = 8.5)

p4 <- ggplot(data = ful_rmr_nd, aes(x = day_night, y = mean_rmr)) + 
  geom_jitter(
    aes(colour = fish_basin),
    size = 2,
    width = 0.15) + 
  facet_rep_wrap(. ~ season, 
                 ncol = 2,
                 repeat.tick.labels = TRUE) +
  scale_colour_viridis_d(begin = 0.2, end = 0.85, alpha = 0.55, 
                         option = "B", name = "Basin") +
  # scale_y_continuous(breaks = seq(0, 200, 25)) +
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
       y = expression(paste("Active Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")))

p4  
