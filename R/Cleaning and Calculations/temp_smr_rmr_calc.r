# load packages ----

library(dplyr)
library(ggplot2)
library(ggh4x)
library(gamm4)
library(here)
library(lubridate)
library(janitor)
library(readr)
library(tibble)
library(tidymv)
library(tidyr)

# bring in clean downloaded ----

lt <- read_rds(here::here("Saved data", 
                          "kenauk lake trout 2017 - 2020.rds"))


glimpse(lt)

# bring in metadata for tagged fish -----
fish_tag_data <- read_csv(here::here("Data", 
                                     "all fish tagged kenauk.csv")) %>%
  clean_names()

glimpse(fish_tag_data)

fish_tag_data <- fish_tag_data %>% 
  mutate(tag_date = dmy(tag_date), 
         year = year(tag_date))

#  grab just lake trout to add weights -----

fish_acel <- fish_tag_data %>%
  filter(species %in% "LT" 
         # & vemco_type %in% "Acc" 
  ) %>% 
  rename(floy_tag = gray_floy_tag_number) %>% 
  dplyr::select(floy_tag, weight, tl, fl, girth,
                vemco_type, basin)

# select just weights and floy tag 
fish_acel_w <- fish_acel %>% 
  dplyr::select(floy_tag, weight) 

glimpse(fish_acel_w)

# join weights to lt 
lt <- lt %>% 
  left_join(fish_acel_w, by = "floy_tag")

# weight correction -----
lt <- lt %>% 
  mutate(
    # weight_1 = if_else(is.na(weight), true = 
    #                         round((((girth * girth) * tl) / 30000), 
    #                               digits = 0),
    #                       false = weight), 
    weight = if_else(is.na(weight), true = 
                       round((10 ^ (-5.218126 +  3.038077 * log10(tl))), 
                             digits = 0),
                     false = weight), 
  )


wg_tl <- lt %>% 
  group_by(floy_tag, tl, weight, 
           # weight_1, weight_2
           ) %>% 
  summarise() %>% 
  arrange(tl) %>% 
  View()



# weights per basin 
wt <- lt %>% 
  group_by(fish_basin) %>% 
  summarise(
    mean_wt = round(mean(weight, na.rm = TRUE), digits = 0), 
    # mean_wt_1 = round(mean(weight_1), digits = 0), 
    # mean_wt_2 = round(mean(weight_2), digits = 0), 
  ) %>% 
  ungroup()


wt


############## create full dataset for bioE froim May 2019 - May 2020 ---------
ful <- lt %>% 
  filter(sensor_value > 0 & sensor_unit %in% c("m/s²", "m", "°C")) %>%
  mutate(
    raw_acel = case_when(
      sensor_unit %in% "m/s²" & !transmitter_serial %in% c("1193450", "1193454") ~  
        (sensor_value - 0) / 0.01922, 
      sensor_unit %in% "m/s²" & transmitter_serial %in% c("1193450", "1193454") ~  
        (sensor_value - 0) / 0.013588, 
      # sensor_unit %in%  "°C" ~ ((sensor_value - (-5)) / 0.1569), 
      # sensor_unit %in% "m" ~ ((sensor_value - (-1.2129)) / 0.3032)
    ),
    time_bins = floor_date(detection_timestamp_utc, unit = "1 hours")
  )

# determined numbered heard 
number_heard <- lt %>% 
  filter(sensor_unit %in% c("°C", "m", "m/s²")) %>% 
  group_by(fish_basin) %>% 
  summarise(n = n_distinct(floy_tag)) %>% 
  ungroup()


number_heard
# mean lab weights from Cruz-font et al. 2016 ------
lab_wt <- c(1551, 1341, 1227)

mean(lab_wt) / 1000
# add mean of lab weight from cruz-font et al. 2016 to ful dataset
ful$lab_wt <- mean(lab_wt)
ful$lab_wt <- ful$lab_wt / 1000
ful$weight <- ful$weight /1000
# ful$weight_2 <- ful$weight_2 /1000

glimpse(ful)


# create summarized bioE per day----
# mean daily temp, depth, and accel 
ful_1 <- ful %>% 
  dplyr::select(floy_tag, detection_timestamp_utc, time_bins, date, fish_basin, 
                week, month, season, year, tl, fl, girth, weight, lab_wt,
                sensor_unit, sensor_value, raw_acel) %>% 
  
  mutate(
    raw_acel = if_else(sensor_unit %in% c("m/s²") & raw_acel < 9, 
                       false = raw_acel, true = NaN), 
    log_raw_acel = log10(raw_acel), 
    log_ss = 0.91 + 0.46 * log_raw_acel,
    ss = 10 ^ log_ss,
    log_mo_ss = 0.22 + 1.17 * log10(ss + 1), # page 1243 if Cruz-Font et al., 2016 Corre
    mo_ss = 10 ^ log_mo_ss,
    smr = case_when(
      sensor_unit %in% ("°C") ~ 
        46.072 * (exp(1) ^ (0.0607 * sensor_value)) * (0.1) * ((weight / 0.1) ^ (0.85)),
      sensor_value %in% c("m/s²", "m") ~ NaN),
    mmr = case_when(
      sensor_unit %in% ("°C") ~ 
        127.18 * (exp(1) ^ (0.0785 * sensor_value)) * (0.1) * 
        ((weight / 0.1) ^ (0.85)),
      sensor_value %in% c("m/s²", "m") ~ NaN),
    
    
    m_swim = case_when(
      sensor_unit %in% c("°C", "m") ~ sensor_value,
      sensor_unit %in% c("m/s²") ~ mo_ss * ((weight / lab_wt) ^ 0.67)), 
    labels = case_when(sensor_unit %in% "m/s²" ~ "Daily Activity (m/s²)", 
                       sensor_unit %in% "°C" ~ "Daily Temperature (°C)", 
                       sensor_unit %in% "m" ~ "Daily Depth (m)"), 
    labels = factor(labels, levels = c("Daily Temperature (°C)", 
                                       "Daily Depth (m)",
                                       "Daily Activity (m/s²)")), 
    fish_basin = factor(fish_basin, level = c("East Basin", 
                                              "West Basin", 
                                              "North Basin")), 
    fish_basin_2 = factor(fish_basin, levels = c("East Basin", 
                                                 "West Basin", 
                                                 "North Basin"), 
                          ordered = TRUE)
  )

glimpse(ful_1)

ful_1 %>% 
  group_by(fish_basin) %>% 
  summarise(
    wt = mean(weight),
    # wt_2 = mean(weight_2)
  ) %>% 
  ungroup()



# ful %>% 
#   filter(sensor_unit %in% ("°C"), 
# !(floy_tag %in% "07478")) %>%
#   group_by(floy_tag, fish_basin, date) %>% 
#   summarise(mean_smr = mean(smr)) %>% 
#   ggplot(aes(x = date, y = mean_smr)) + 
#   geom_point(aes(colour = floy_tag), size = 3, alpha = 0.45)

# write bioE dataframe as rds ----- 
write_rds(x = ful_1, here("Saved Data", 
                          "BioE_lt.rds"))

