

# load packages ----

library(dplyr)
library(data.table)
library(ggplot2)
library(ggh4x)
library(here)
library(lubridate)
library(lemon)
library(janitor)
library(patchwork)
library(purrr)
library(readr)
library(tibble)
library(tidyr)



# bring in metadata for tagged fish -----
fish_tag_data <- read_csv(here::here("Data", 
                                     "all fish tagged kenauk.csv")) %>%
  clean_names()

glimpse(fish_tag_data)

fish_tag_data <- fish_tag_data %>% 
  mutate(tag_date = dmy(tag_date), 
         year = year(tag_date), 
         basin = stringr::str_replace(basin, "Main", "East"))


tag_summary <- fish_tag_data %>% 
  filter(species %in% "LT" 
         & vemco_type %in% c("T/P", "Acc")) %>% 
  rename(floy_tag = gray_floy_tag_number) %>% 
  select(floy_tag, year, basin, weight, tl, fl, girth,
         vemco_type, weight) %>% 
  mutate(weight = if_else(is.na(weight), true = 
                            round((((girth * girth) * tl) / 30000), 
                                  digits = 0),
                          false = weight)
  ) %>% 
  group_by(year, basin, vemco_type) %>% 
  summarise(tl = mean(tl), 
            tl_sem = sd(.$tl) / sqrt(n()), 
            fl = mean(fl), 
            fl_sem = sd(.$fl) / sqrt(n()),
            girth = mean(girth), 
            girth_sem = sd(.$girth) / sqrt(n()),
            weight = mean(weight),
            weight_sem = sd(.$weight) / sqrt(n()),
            n = n_distinct(floy_tag)) %>% 
  ungroup() %>% 
  arrange(year, basin, vemco_type) %>% 
  rename(Year = year, 
         "Fish Basin" = basin, 
         "Transmitter Type" = vemco_type, 
         "Total Length (mm)" = tl, 
         "Fork Length (mm)" = fl, 
         "Girth (mm)" = girth, 
         "Weight (g)" = weight)



tag_summary


openxlsx::write.xlsx(tag_summary, here("Results", 
                                       "BioE_tagging_summary.xlsx"))


 tag_summary <- fish_tag_data %>% 
  filter(species %in% "LT" 
         & vemco_type %in% c("T/P", "Acc")) %>% 
  rename(floy_tag = gray_floy_tag_number) %>% 
  select(floy_tag, year, basin, weight, tl, fl, girth,
         vemco_type, weight) %>% 
  mutate(weight = if_else(is.na(weight), true = 
                            round((((girth * girth) * tl) / 30000), 
                                  digits = 0),
                          false = weight)
  ) %>% 
  group_by(year, vemco_type) %>% 
  summarise(tl = mean(tl), 
            tl_sem = sd(.$tl) / sqrt(n()), 
            fl = mean(fl), 
            fl_sem = sd(.$fl) / sqrt(n()),
            girth = mean(girth), 
            girth_sem = sd(.$girth) / sqrt(n()),
            weight = mean(weight),
            weight_sem = sd(.$weight) / sqrt(n()),
            n = n_distinct(floy_tag)) %>% 
  ungroup() %>% 
  arrange(vemco_type, year)

tag_summary



tag_summary
fish_tag_data %>% 
  filter(species %in% "LT" 
         & vemco_type %in% c("T/P", "Acc")) %>% 
  rename(floy_tag = gray_floy_tag_number) %>% 
  select(floy_tag, year, basin, weight, tl, fl, girth,
         vemco_type, weight) %>% 
  mutate(weight = if_else(is.na(weight), true = 
                            round((((girth * girth) * tl) / 30000), 
                                  digits = 0),
                          false = weight)
  ) %>% 
  group_by(year, vemco_type) %>% 
  summarise(n = n_distinct(floy_tag))

options(pillar.sigfig = 5)



fish_tag_data %>% 
  filter(species %in% "LT" 
         & vemco_type %in% c("T/P", "Acc")) %>% 
  rename(floy_tag = gray_floy_tag_number) %>% 
  select(floy_tag, year, basin, weight, tl, fl, girth,
         vemco_type, weight) %>% 
  mutate(weight = if_else(is.na(weight), true = 
                            round((((girth * girth) * tl) / 30000), 
                                  digits = 0),
                          false = weight)
  ) %>% 
  summarise(tl = mean(tl), 
            tl_sem = sd(.$tl) / sqrt(n()), 
            fl = mean(fl), 
            fl_sem = sd(.$fl) / sqrt(n()),
            girth = mean(girth), 
            girth_sem = sd(.$girth) / sqrt(n()),
            weight = mean(weight),
            weight_sem = sd(.$weight) / sqrt(n()),
            n = n_distinct(floy_tag))