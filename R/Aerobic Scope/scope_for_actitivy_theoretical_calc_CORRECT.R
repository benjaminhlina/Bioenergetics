# # load packages ----
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
  library(lme4)
  library(janitor)
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
                     "BioE_lt.rds"))# # ful_temp %>%
# #   filter(floy_tag %in% "07478"))
# #
# 
# 
# ##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------
# 
ful_temp <- ful %>%
  filter(sensor_unit %in% "°C"
         # & !(floy_tag %in% "07478")
  ) %>%
  group_by(floy_tag, weight, date, fish_basin,
           week, month, season, year,
           sensor_unit, labels) %>%
  summarise(mean_smr = mean(smr, na.rm = TRUE),
            sem_smr = sd(smr) / sqrt(n()),
            cv_smr = raster::cv(smr),
            mean_temp = mean(sensor_value, na.rm = TRUE),
            sem_temp = sd(sensor_value) / sqrt(n()),
            cv_temp = raster::cv(sensor_value), 
            mean_mmr = mean(mmr, na.rm = TRUE),
            sem_mmr = sd(mmr) / sqrt(n()),
            cv_mmr = raster::cv(mmr), 
            
  ) %>%
  ungroup() %>%
  mutate(date_2 = as.numeric(date),
         floy_tag = factor(floy_tag)) %>%
  filter(date_2 <= 18559) %>%
  mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
                                                  ""),
                             levels = c("East", "West", "North")),
         doy = yday(date),
         month = factor(month,
                        levels = c("May", "June", "July",
                                   "August", "September", "October",
                                   "Novemeber", "December", "January",
                                   "February", "March", "April")), 
         aerobic_scope = mean_mmr - mean_smr
  ) %>%
  arrange(month) %>%
  mutate(doy_id = days(date)) %>%
  arrange(date)

# # 
# # # remove big objects to free up RAM -----
# # # rm(ful)
# # gc()
# # 
# glimpse(ful_temp)
# 
# 
# # tail(ful_temp)
# # # 2020-10-24
write_rds(ful_temp, here("Saved Data",
                         "Daily_theoratical_smr_mmr.rds")) 

