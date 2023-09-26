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

# ---- bring in to calcuate daily theroritcal scope ---- 


dat <- read_rds(here("Saved Data",
                     "Daily_theoratical_smr_mmr.rds")) 

glimpse(dat)