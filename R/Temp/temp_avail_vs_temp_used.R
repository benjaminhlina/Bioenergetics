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


ful_temp <- read_rds(here("Saved Data", 
                          "Daily_temp.rds"))


temp <- read_rds(here("Saved Data", 
                      "daily_temp_range_measured.rds"))
predicts <- read_rds(here("model objects", 
                          "temp_gamm_predicts_update_jan.rds"))

glimpse(ful_temp)
glimpse(temp)


# plotting prep -------
month_doy <- predicts %>%
  group_by(month_abb) %>%
  summarise(first = first(doy),
            last = last(doy)) %>%
  ungroup() %>%
  mutate(
    # month_abb = forcats::fct_relevel(month_abb, "Jan",
    #                                  "Feb", "Mar", "Apr", "May", "Jun",
    #                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) %>%
  arrange(month_abb) %>%
  mutate(
    # first = case_when(
    # month_abb %in% "May" ~ 1,
    # month_abb %in% "June" ~ 10 + 20, false = first
    # )
  ) %>%
  .$first

month_doy
# month_doy <- c(1, 32, 62, 93, 123, 154, 184, 215, 246, 274, 305, 335)
predicts %>%
  filter(doy %in% month_doy) %>%
  group_by(month_abb) %>%
  summarise() %>%
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan",
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>%
  arrange(month_abb) %>%
  .$month_abb -> month_label
month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 

rect_summer <- tibble(
  season = "Summer",
  xmin = 152,
  xmax = 244,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 1,
  xmax = 60,
  ymin = -Inf,
  ymax = Inf
)
rect_winter_dec <- tibble(
  season = "Winter",
  xmin = 335,
  xmax = 365,
  ymin = -Inf,
  ymax = Inf
)



temp <- temp %>% 
  mutate(
    year = as.numeric(as.character(year))
  )


temp_sum <- temp %>% 
  mutate(
    doy = yday(daily)
  ) %>% 
  group_by(doy) %>% 
  summarise(
    md_temp  = mean(mdt), 
    sem = sd(mdt)/ sqrt(n()), 
    min_temps = min(min_temp), 
    max_temps = max(max_temp), 
    # med_temps = median(), 
  ) %>% 
  ungroup()

temp_sum



temp_combo <- ful_temp %>% 
  left_join(
    temp_sum, 
    by = c("doy" = "doy"), 
    multiple = "all"
  )


glimpse(temp_combo)

temp_comb_long <- temp_combo %>% 
  dplyr::select(floy_tag:max_temps) %>% 
  pivot_longer(cols = -c(floy_tag:sem), 
               names_to = "min_max", 
               values_to = "temp_range"
               
  )

glimpse(temp_comb_long)

glimpse(temp_combo)

ggplot(data = temp_combo, aes(x = doy, y = mean_temp)) + 
  geom_point(aes(color = fish_basin), 
             size = 2) + 
  geom_ribbon(aes(ymin = min_temps, 
                  ymax = max_temps), 
              alpha = 0.15) + 
  scale_color_viridis_d(end = 0.85, begin = 0.25, option = "B", 
                        alpha = 0.3, 
                        name = "Basin") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  labs(x = "Day of Year", 
       y = "Daily Temperature (°C) Used") -> p


p



glimpse(temp_combo)

sum_temp_comb <- temp_combo %>% 
  group_by(doy, fish_basin, season) %>% 
  summarise(
    
    mean_fish_temp = mean(mean_temp),
    sem = sd(mean_temp) / sqrt(n()), 
    min_temp = min(min_temps), 
    max_temp = max(max_temps)
  ) %>% 
  ungroup() %>% 
  mutate(
    max_temp_adjust = case_when(max_temp > 13 ~ 13, 
                                TRUE ~ max_temp)
  )


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
#   xmin = 220,
#   xmax = 305,
#   ymin = -Inf,
#   ymax = Inf
# )

season_line <- tibble(
  xmark = c(32, 123, 220, 305)
)
glimpse(predicts)

ggplot(data = sum_temp_comb, aes(x = doy, 
                                 y = mean_fish_temp)) + 
  geom_rect(data = rect_summer, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = NA,
            colour = "black",
            linewidth = 0.8,
            linetype = 3,
            # alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = NA,
            linewidth = 0.8,
            # alpha = 0.75,
            colour = "black",
            linetype = 3,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter_dec, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = NA,
            linewidth = 0.8,
            # alpha = 0.75,
            colour = "black",
            linetype = 3,
            inherit.aes = FALSE) +
  # geom_vline(data = season_line,
  #            aes(xintercept = xmark),
  #            # fill = NA,
  #            colour = "black",
  #            linewidth = 0.8,
  #            linetype = 3,
  #            # alpha = 0.75,
  #            # inherit.aes = FALSE
  #            ) +

  geom_text(
    aes(x = xmin + 31, y = 13, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 17.5, y = 13, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 3.75, y = 13, label = season),
    data = rect_winter_dec,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_linerange(aes(ymin = mean_fish_temp - sem,
  #                    ymax = mean_fish_temp + sem,
  #                    group = fish_basin)) +
  geom_point(aes(color = fish_basin), 
             size = 3, 
             alpha = 0.5
  ) + 
  geom_ribbon(aes(
    ymin = min_temp, 
    # ymax = max_temp
    ymax = max_temp_adjust
  ), 
  alpha = 0.15) + 
  
  geom_line(data = predicts, 
            aes(x = doy, y = fit, colour = fish_basin), 
            linewidth = 1) +
  geom_ribbon(data = predicts,
              aes(ymin = lower,
                  ymax = upper,
                  x = doy, y = fit,
                  fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(0, 27.5, 2.5), 
                     limits = c(0, 13)
  ) +
  scale_x_continuous(breaks = month_doy, 
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Daily Temperature (°C)") -> p1
p1


write_rds(x = p1, here("Plot Objects", 
                       "mdt_used_GAMM_available_13.rds"))

cols <- rev(rainbow(6)[-6])

sum_temp_comb %>% 
  pivot_longer(
    cols = -c(doy:mean_fish_temp), 
    names_to = "min_max", 
    values_to = "water_temp"
  ) %>% 
  filter(water_temp != is.na(water_temp) &
           min_max %in% c("min_temp", "max_temp")) %>% 
  ggplot(aes(x = fish_basin, 
             y = mean_fish_temp)) + 
  geom_jitter(aes(colour = water_temp), 
              size = 3, width = 0.15) + 
  scale_y_continuous(breaks = seq(0, 12.5, 2.5)) + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  # geom_errorbar(aes(ymin = min_temp, 
  #                   ymax = max_temp, group = fish_basin), 
  #               width = 0.15) + 
  scale_colour_gradientn(colours = alpha(cols, f = 0.35), 
                         name = "Water Temperature (°C)",
                         # breaks = seq(2, 10, 2), 
                         guide = guide_colorbar(frame.colour = "black", 
                                                ticks.colour = "black")
  ) +
  # scale_colour_viridis_c(
  #   name = "Water Temperature (°C)",
  #   end = 0.75,
  #   begin = 0.25, 
  #   option = "B", 
  #   alpha = 0.5) + 
  labs(x = "Basin", 
       y = "Daily Temperature (°C) Used") -> p2
p2



ggsave(filename = here("Plots", 
                       "thermal habitat available", 
                       "ind_mdt_used_ribbon_available.png"), 
       height = 7, width = 11, plot = p)

ggsave(filename = here("Plots", 
                       "thermal habitat available", 
                       "mdt_used_ribbon_available_13.png"), 
       height = 8.5, width = 11, plot = p1)

ggsave(filename = here("Plots", 
                       "thermal habitat available", 
                       "mdt_used_per_basin.png"), 
       height = 7, width = 11, plot = p2)


ggplot(data = sum_temp_comb %>% 
         filter(fish_basin == "North"), 
       aes(x = doy, 
           y = mean_fish_temp)) + 
  geom_point(aes(color = fish_basin), 
             size = 2) + 
  geom_ribbon(aes(ymin = min_temp, 
                  ymax = max_temp), 
              alpha = 0.15) + 
  scale_color_viridis_d(end = 0.75, begin = 0.25, option = "B", 
                        alpha = 0.3, name = "Basin") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank()
  ) + 
  labs(x = "Day of Year", 
       y = "Daily Temperature (°C) Used")
