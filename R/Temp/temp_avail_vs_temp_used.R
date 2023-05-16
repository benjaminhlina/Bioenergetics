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
                          "temp_gamm_predicts.rds"))

glimpse(ful_temp)
glimpse(temp)


predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by( month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 

predicts %>% 
  filter(doy_id == 155) %>% 
  as_tibble()

temp <- temp %>% 
  mutate(
    year = as.numeric(as.character(year))
  )


temp_sum <- temp %>% 
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
    by = c("doy_id" = "doy"), 
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

ggplot(data = temp_combo, aes(x = doy_id, y = mean_temp)) + 
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

# p



glimpse(temp_combo)

sum_temp_comb <- temp_combo %>% 
  group_by(doy_id, fish_basin, season) %>% 
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


ggplot(data = sum_temp_comb, aes(x = doy_id, 
                                 y = mean_fish_temp)) + 
  geom_linerange(aes(ymin = mean_fish_temp - sem,
                     ymax = mean_fish_temp + sem,
                     group = fish_basin)) +
  geom_point(aes(color = fish_basin), 
             size = 3, 
             alpha = 0.5
             ) + 
  geom_ribbon(aes(ymin = min_temp, 
                  ymax = max_temp), 
              alpha = 0.15) + 
  
  geom_line(data = predicts, 
            aes(x = doy_id, y = fit, colour = fish_basin), 
            linewidth = 1) +
  geom_ribbon(data = predicts, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(0, 27.5, 2.5), 
                     # limits = c(0, 13)
                     ) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
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


cols <- rev(rainbow(6)[-6])

sum_temp_comb %>% 
  pivot_longer(
    cols = -c(doy_id:mean_fish_temp), 
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
                       "mdt_used_ribbon_available_sem.png"), 
       height = 7, width = 11, plot = p1)

ggsave(filename = here("Plots", 
                       "thermal habitat available", 
                       "mdt_used_per_basin.png"), 
       height = 7, width = 11, plot = p2)


ggplot(data = sum_temp_comb %>% 
         filter(fish_basin == "North"), 
       aes(x = doy_id, 
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
