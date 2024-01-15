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



month_doy <- soa_gamm %>% 
  group_by(month_abb) %>% 
  summarise(first = first(doy),
            last = last(doy)) %>% 
  ungroup() %>% 
  mutate(
    month_abb = forcats::fct_relevel(month_abb, "Jan", 
                                     "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) %>% 
  arrange(month_abb) %>%   
  .$first
month_doy
soa_gamm %>% 
  filter(doy %in% month_doy) %>%
  group_by(month_abb) %>% 
  summarise() %>% 
  mutate(
    month_abb = forcats::fct_relevel(month_abb, "Jan", 
                                     "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) %>% 
  arrange(month_abb) %>% 
  .$month_abb -> month_label 
month_label



soa_gamm %>% 
  group_by(season) %>% 
  summarise(first = first(doy),
            last = last(doy)) %>% 
  ungroup()

rect_summer <- tibble(
  season = "Summer",
  xmin = 152,
  xmax = 244,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 1,
  xmax = 60,
  ymin = -Inf,
  ymax = Inf
)
rect_winter_dec <- tibble(
  season = "Winter",
  xmin = 335,
  xmax = 365,
  ymin = -Inf,
  ymax = Inf
)

# ---------- plot ----------



ggplot() + 
  geom_rect(data = rect_summer, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = "grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill ="grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter_dec, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill ="grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_text(
    aes(x = xmin + 31, y = 161.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 17.5, y = 161.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 3.75, y = 161.25, label = season),
    data = rect_winter_dec,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = mean_soa_as, aes(x = doy, y = mean, colour = type), 
             alpha = 0.25) + 
  geom_line(data = activity, 
            aes(x = doy, y = fit, 
                colour = type
            ), linewidth = 1) +
  geom_ribbon(data = activity, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy, y = fit,
                  fill = type, 
              ), alpha = 0.25) +
  scale_fill_viridis_d(name = "",
                       option = "B", begin = 0.35, end = 0.75, 
                       labels = c("Aerobic Scope", "Scope for Activity")) +
  scale_colour_viridis_d(name = "",
                         option = "B", begin = 0.35, end = 0.75, 
                         labels = c("Aerobic Scope", "Scope for Activity")) + 
  scale_x_continuous(breaks = month_doy, 
                     label = month_label) +
  scale_y_continuous(breaks = seq(20, 180, 20)) +
  # coord_cartesian(ylim = c(-25, 165)) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.30, 0.95),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date", 
       y = expression(paste("Metabolic Rate (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

# p
write_rds(p, here("Plot Objects", 
                  "Scope_of_activity_and_AS_gamm_plot_jan.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "Scope_of_activity_and_AS_gamm.png"), 
       width = 11,
       height = 7)
