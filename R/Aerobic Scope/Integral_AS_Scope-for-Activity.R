# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(emmeans)
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

# need to bring in means ---- 
as_gamm <- read_rds(here("Saved Data", 
                         "aerobic_scope_gamma_predict_jan.rds"))
soa_gamm <- read_rds(here("Saved Data", 
                          "scope_for_activity_gamma_predict.rds"))
mean_soa <- read_rds(here::here("Model Objects", 
                                "mean_soa.rds"))
mean_as <- read_rds(here::here("Model Objects", 
                               "mean_as_jan.rds"))

mean_soa$type <- "soa"
mean_as$type <- "as"

mean_as
mean_soa

mean_soa <- mean_soa %>% 
  rename(
    mean = mean_fs, 
    sd = sd_fs, 
    sem = sem_fs
  )
mean_as <- mean_as %>% 
  rename(
    mean = mean_as, 
    sd = sd_as, 
    sem = sem_as
  )

mean_soa_as <- bind_rows(mean_as, mean_soa)

soa_gamm %>% 
  filter(doy %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_labels


glimpse(as_gamm)
glimpse(soa_gamm)

# as_slim <- as_gamm %>% 
#   group_by(doy) %>% 
#   summarise(
#     as_pred = mean(fit),
#     as_upper = mean(upper),
#     as_lower = mean(lower)
#   ) %>% 
#   ungroup()
# 
# 
# soa_slim <- soa_gamm %>% 
#   group_by(
#     doy
#   ) %>% 
#   summarise(
#     soa_pred = mean(fit),
#     soa_upper = mean(upper),
#     soa_lower = mean(lower)
#   ) %>% 
#   ungroup()


as_slim <- as_gamm %>% 
  as_tibble() %>% 
  dplyr::select(doy, fit, lower, upper) %>% 
  mutate(
    type = "as"
  )

soa_slim <- soa_gamm %>%
  as_tibble() %>% 
  dplyr::select(doy, fit, lower, upper) %>% 
  mutate(
    type = "soa"
  )

soa_slim
as_slim

activity <- bind_rows(as_slim, soa_slim)

activity_wide <- activity %>% 
  pivot_wider(id_cols = doy, names_from = "type", 
              values_from = c("fit", "lower", "upper"), 
              values_fn = list(fit = mean, 
                               lower = mean, 
                               upper = mean))

activity_wide



activity_wider <- activity_wide %>% 
  mutate(
    integral_fit = fit_as - fit_soa, 
    integral_ul = lower_as - upper_soa, 
    integral_ll = lower_as - lower_soa,
    integral_up = upper_as - upper_soa, 
    multi_fit = fit_soa / fit_as,
    multi_low = lower_soa / lower_as,
    multi_upper = upper_soa / upper_as,
  )

activity_wider

summary(activity_wider)

integral_longer <- activity_wider %>% 
  dplyr::select(doy, integral_fit:integral_up) %>% 
  pivot_longer(cols = -c(doy), 
               names_to = "type", 
               values_to = "integral")





ggplot(data = integral_longer, aes(x = doy, y = integral)) + 
  geom_line(aes(colour = type), linewidth = 1) + 
  scale_colour_viridis_d(name = "Integral", option = "A", 
                         end = 0.8) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Date", 
    y = "Metabolic Rate"
  ) 
