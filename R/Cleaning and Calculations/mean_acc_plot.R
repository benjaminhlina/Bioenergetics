
# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(ggplot2)
  library(ggh4x)
  library(gamm4)
  library(gamlss)
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
ful <- read_rds(here("Saved Data",
                     "BioE_lt.rds"))

glimpse(ful)
##### SUBSET OUT ONLY RMR/M_Swim DATA and determine daily temp FOR 2017 - 2021-------


ful_rmr <- ful %>%
  filter(sensor_unit %in% "m/s²" &
           year %in% c(2019, 2020)) %>%
  filter(!sensor_value == is.nan(sensor_value)) %>%
  group_by(floy_tag, weight, fish_basin, date,
           week, month, season, year,
           sensor_unit, labels) %>%
  summarise(mean_acc = mean(sensor_value)) %>%
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
  dplyr::select(floy_tag:sensor_unit, doy, doy_id, mean_acc)
glimpse(ful_rmr)


ggplot(data = ful_rmr) + 
  geom_point(data = ful_rmr, aes(x = doy_id, y = mean_acc,
                                  colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
 
  # scale_y_continuous(breaks = seq(45, 135, 15)) +
  scale_x_continuous(breaks = seq(09, 344, 67),
                     # label = month_label
                     ) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  
  # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
  #                # ncol = 1
  # ) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Mean Acceleration (m/s²)") -> p 

p
