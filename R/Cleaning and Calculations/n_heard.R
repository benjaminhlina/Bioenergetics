# load packages -----

{
  library(dplyr)
  library(here)
  library(openxlsx)
  library(readr)
  
}


ful <- read_rds(here("Saved Data", 
                     "BioE_lt.rds"))

glimpse(ful)

n_heard <- ful %>% 
  group_by(fish_basin, sensor_unit) %>% 
  summarise(
    n = n_distinct(floy_tag)
  ) %>% 
  ungroup() %>% 
  arrange(sensor_unit)


n_heard

n_heard %>% 
  write.xlsx(here("results", 
                  "n_heard.xlsx"))

