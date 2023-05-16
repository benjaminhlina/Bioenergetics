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
  source(here("R", 
              "Cleaning and Calculations", 
              "julian_date_reorder.r"))
}
# bring in RDS -----
# 
# ful <- read_rds(here("Saved Data",
#                      "BioE_lt.rds"))
# glimpse(ful)


# date_sum <- ful %>% 
#   filter(year == 2019) %>% 
#   group_by(date) %>% 
#   summarise(date = unique(date)) %>% 
#   ungroup() %>% 
#   mutate(doy_id = days(date))

# write_rds(date_sum, here("saved data", 
#                          "date_list_labels.rds"))
# 
# glimpse(ful)
# ful_temp <- ful %>%
#   filter(sensor_unit %in% "°C"
#            & !(floy_tag %in% "07478") &
#            sensor_value > 15, 
#          )  %>%
#   group_by(floy_tag, weight, date, fish_basin,
#            week, month, season, year,
#            sensor_unit, labels) %>%
#   summarise(mean_smr = mean(smr, na.rm = TRUE),
#             mean_temp = mean(sensor_value, na.rm = TRUE), 
#             n = n_distinct(detection_timestamp_utc)) %>%
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
#                                    "February", "March", "April"))
#          ) %>%
#   arrange(month) %>%
#   mutate(doy_id = days(date)) %>%
#   arrange(date)
# 
# 
# glimpse(ful_temp)

# write_rds(ful_temp, here("Saved Data", 
#                "temp_smr_+15_forrays.rds"))

ful_temp <- read_rds(here("Saved Data", 
                          "temp_smr_+15_forrays.rds"))

# day_sum <- read_rds(here("saved data", 
#                          "date_list_labels.rds"))
# 

ful_temp %>% 
  mutate(month_abb = month(date, label = TRUE)) %>% 
  filter(doy_id %in% seq(50, 150, 50)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 

mean_forrays <- ful_temp %>% 
  group_by(date, doy_id, fish_basin, season, year) %>% 
  summarise(
    temp = mean(mean_temp), 
    sem_temp = sd(mean_temp) / sqrt(n()), 
    smr = mean(mean_smr), 
    sem_smr = sd(mean_smr) / sqrt(n()), 
  ) %>% 
  ungroup()

# tapply(ful_temp$mean_temp,ful_temp$doy_id, function (x) sd(x)/sqrt(length(x)))

ful_temp %>% 
  group_by(date, doy_id, fish_basin, season, year) %>% 
  distinct(temps = mean_temp) %>% 
  ungroup()
# month_label <- day_sum %>% 
#   mutate(month_abb = month(date, label = TRUE)) %>% 
#   filter(doy_id %in% seq(25, 350, 65)) %>% 
#   group_by(month_abb) %>% 
#   summarise() %>% 
#   mutate(month_abb = forcats::fct_relevel(month_abb, 
#                                           "May", "Jul", "Oct", 
#                                           "Dec", "Feb", "Apr"))  


cols <- rev(rainbow(6)[-6])

ggplot(data = ful_temp, aes(x = date, y = mean_smr)) + 
  geom_point(aes(fill = mean_temp, size = n), 
             shape = 21, stroke = 0.25,
             colour = "black"
               ) + 
  scale_size_continuous(range = c(1, 8), 
                        breaks = seq(15, 90, 15)) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # scale_fill_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  theme_classic(base_size = 15) + 
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p

ggplot(data = ful_temp, aes(x = doy_id, y = mean_smr)) + 
  geom_point(aes(fill = mean_temp, size = n),
             shape = 21, stroke = 0.25, colour = "black") + 
  scale_size_continuous(range = c(1, 8), 
                        breaks = seq(15, 90, 15)) + 
  scale_shape(name = "Basin") + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(50, 150, 50),
                     label = month_label) +
  theme_classic(base_size = 15) + 
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1
p1



ggplot(data = mean_forrays, aes(x = doy_id, y = smr)) + 
  geom_point(aes(fill = temp, 
                 shape = fish_basin, 
                 group = fish_basin), 
             size = 3, 
             position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin = smr - sem_smr,
                    group = fish_basin, 
                    ymax = smr + sem_smr), 
                width = 0.15, position = position_dodge(0.9)) + 
  scale_shape_manual(name = "Basin", values = 21:23) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # scale_size_continuous(range = c(2, 5)) + 
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(50, 150, 50),
                     label = month_label) +
  theme_classic(base_size = 15) + 
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2
p2
ggplot(data = ful_temp, aes(x = doy_id, y = mean_smr)) + 
  geom_point(aes(fill = mean_temp, size = n, 
                 shape = fish_basin), 
             stroke = 0.25, colour = "black") + 
  scale_size_continuous(range = c(1, 8), 
                        breaks = seq(15, 90, 15)) + 
  scale_shape_manual(name = "Basin", values = 21:23) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(50, 150, 50),
                     label = month_label) +
  theme_classic(base_size = 15) + 
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p3
p3


ggplot(data = mean_forrays, aes(x = doy_id, y = smr)) + 
  geom_point(aes(fill = temp, shape = fish_basin, 
                 group = fish_basin), size = 3, 
             # position = position_dodge(0.9)
             ) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # geom_errorbar(aes(ymin = mean_smrs - sem_smr,
  #                   group = fish_basin, 
  #                   ymax = mean_smrs + sem_smr), 
  #               width = 0.15, position = position_dodge(0.9)) + 
  scale_shape_manual(name = "Basin", values = 21:23) + 
  # scale_size_continuous(range = c(2, 5)) + 
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(50, 150, 50),
                     label = month_label) +
  theme_classic(base_size = 15) + 
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p4
p4


ggplot(data = ful_temp, aes(x = date, y = mean_smr)) + 
  geom_point(aes(fill = mean_temp, shape = fish_basin, size = n),
             stroke = 0.25) + 
  scale_size_continuous(range = c(2, 5)) + 
  scale_shape_manual(name = "Basin", values = 21:23) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  theme_classic(base_size = 15) + 
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p5
# ggplot(data = mean_forrays, aes(x = doy_id, y = mean_smrs)) + 
#   geom_jitter(aes(colour = mean_temps, shape = fish_basin, 
#                  group = fish_basin), size = 3, 
#               position = position_jitter(width=0.5), ) + 
#   geom_errorbar(aes(ymin = mean_smrs - sem_smr,
#                     group = fish_basin, 
#                     ymax = mean_smrs + sem_smr), 
#                 width = 0.15, 
#                 position = position_jitter(width=0.5), ) + 
#   # scale_size_continuous(range = c(2, 5)) + 
#   scale_colour_viridis_c(name = "Temperature (°C)",
#                          option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
#   scale_x_continuous(breaks = seq(50, 150, 50),
#                      label = month_label) +
#   theme_classic(base_size = 15) + 
#   labs(x = "Date", 
#        y = expression(paste("Standard Metabolism (mg", 
#                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p3
# p3
p5

ggplot(data = ful_temp, aes(x = doy_id, y = mean_smr)) + 
  geom_point(aes(fill = mean_temp, size = n, 
                 # shape = fish_basin
                 ), 
             stroke = 0.25, 
             shape = 21, colour = "black") + 
  scale_size_continuous(range = c(1, 8), 
                        breaks = seq(15, 90, 15)) + 
  scale_shape_manual(name = "Basin", values = 21:23) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  facet_wrap(~fish_basin, ncol = 1) + 
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(50, 150, 50),
                     label = month_label) +
  guides(size = guide_legend(ncol = 2)) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = c(0.125, 0.15), 
    legend.background = element_blank()
  ) +
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p6




ggplot(data = ful_temp, aes(x = doy_id, y = mean_smr)) + 
  geom_point(aes(fill = mean_temp, size = n, 
                 # shape = fish_basin
  ), 
  stroke = 0.25, 
  shape = 21, colour = "black") + 
  scale_size_continuous(range = c(1, 8), 
                        breaks = seq(15, 90, 15)) + 
  scale_shape_manual(name = "Basin", values = 21:23) + 
  scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
                       name = "Temperature (°C)",
                       # breaks = seq(, 10, 2), 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black")
  ) +
  # facet_wrap(~fish_basin, ncol = 1) + 
  # scale_colour_viridis_c(name = "Temperature (°C)",
  #                        option = "B", begin = 0.35, end = 0.75, alpha = 0.5) +
  scale_x_continuous(breaks = seq(50, 150, 50),
                     label = month_label) +
  # guides(size = guide_legend(ncol = 2)) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = c(0.10, 0.75), 
    legend.background = element_blank()
  ) +
  labs(x = "Date", 
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p7

p7
p6


ggsave(here("Plots", 
            "Temp Forrays", 
            "individual_mean_smr_forrays_year.png"), plot = p, 
       width = 11, height = 7)


ggsave(here("Plots", 
            "Temp Forrays", 
            "individual_mean_smr_forrays.png"), plot = p1, 
       width = 11, height = 7)
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_forrays.png"), plot = p2, 
       width = 11, height = 7)
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_forrays_basin.png"), plot = p3, 
       width = 11, height = 7)
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_forrays_basin_no_sem.png"), plot = p4, 
       width = 11, height = 7)
ggsave(here("Plots", 
            "Temp Forrays", 
            "dindividual_mean_smr_forrays_year_basin.png"), plot = p5, 
       width = 11, height = 7)
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_forrays_year_facet.png"), plot = p6, 
       width = 7.75, height = 11)
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_forrays_year_nb.png"), plot = p7, 
       width = 11, height = 8.5)


ggplot(data = ful_temp, aes(x = mean_smr)) + 
  geom_histogram() + 
  facet_wrap(~ fish_basin)

ggplot(data = mean_forrays, aes(x = smr)) + 
  geom_histogram(fill = "black") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  facet_wrap(~ fish_basin, ncol = 1) + 
  labs(x = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")), 
       y = "Frequency") -> p8



ggplot(data = mean_forrays, aes(x = smr)) + 
  geom_histogram(aes(fill = fish_basin), 
                 colour = "black") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    legend.position = c(0.9, 0.92)
  ) + 
  scale_fill_viridis_d("Basin", option = "C", end = 0.8, begin = 0.2, 
                       alpha = 0.8) + 
  labs(x = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")), 
       y = "Frequency") -> p9
ggplot(data = mean_forrays, aes(x = temp)) + 
  geom_histogram(aes(fill = fish_basin), 
                 colour = "black") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    legend.position = c(0.9, 0.92)
  ) + 
  scale_fill_viridis_d("Basin", option = "C", end = 0.8, begin = 0.2, 
                       alpha = 0.8) + 
  labs(x = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")), 
       y = "Frequency") -> p10


ggplot(data = mean_forrays, aes(x = temp)) + 
  geom_histogram(fill = "black") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  facet_wrap(~ fish_basin, ncol = 1) + 
  labs(x = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")), 
       y = "Frequency") -> p11

p9
p8
p10
p11
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_frequency_facet.png"), plot = p8, 
       width = 8.5, height = 11)
ggsave(here("Plots", 
            "Temp Forrays", 
            "daily_mean_smr_frequency.png"), plot = p9, 
       width = 11, height = 8.5)



mean_forrays
