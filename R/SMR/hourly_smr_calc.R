# load packages ----

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

# bring in RDS -----

ful <- read_rds(here("Saved Data", 
                     "BioE_lt.rds"))

glimpse(ful)

# ful_temp %>% 
#   filter(floy_tag %in% "07478"))
# 


##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------

ful_temp <- ful %>% 
  filter(sensor_unit %in% "°C" 
         & !(floy_tag %in% "07478")
  ) %>%
  group_by(floy_tag,fish_basin,  time_bins, date, 
           week, month, season, year,
           sensor_unit, labels) %>% 
  summarise(mean_smr = mean(smr, na.rm = TRUE), 
            mean_temp = mean(sensor_value, na.rm = TRUE)) %>% 
  ungroup() %>%  
  mutate(date_2 = as.numeric(date), 
         floy_tag = factor(floy_tag)) %>%
  filter(date_2 <= 18559) %>%
  mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
                                                  ""), 
                             levels = c("East", "West", "North")), 
         hour = hour(time_bins), 
         month = factor(month, 
                        levels = c("May", "June", "July", 
                                   "August", "September", "October",
                                   "Novemeber", "December", "January",
                                   "February", "March", "April"))
  ) %>% 
  arrange(floy_tag, season, hour)

glimpse(ful_temp)

write_rds(ful_temp, here("Saved Data",
                         "Hourly_SMR.rds"))
