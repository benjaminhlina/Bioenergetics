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

# bring in model to predict ----

mod <- read_rds(here("model objects", 
                     "smr_gamm_model.rds"))
smr <- read_rds(here("Saved Data", 
                     "Daily_SMR.rds"))  


glimpse(smr)
formula.gam(mod)

smr_predict <- crossing(
  # tibble(date = unique(df$daily)),
  tibble(doy_id = unique(smr$doy_id)),
  tibble(year = as.factor(seq(2025, 2075, 1))),
  tibble(fish_basin = unique(smr$fish_basin)),
  tibble(floy_tag = "1")
  
) %>%
  
  mutate(
    temp = predict.gam(
      mod,
      newdata = tibble(
        floy_tag,
        # date = date,
        doy_id = doy_id,
        # day_in_year,  
        year = year,
        fish_basin = fish_basin,
        # depth = depth, 
      )
    )
  )


glimpse(temp_raster_smooth)
temp_raster_smooth <- temp_raster_smooth %>%
  mutate(temp = exp(1) ^ temp)
