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


as_gamm <- read_rds(here("Saved Data", 
                         "aerobic_scope_gamma_predict_basin.rds"))
soa_gamm <- read_rds(here("Saved Data", 
                          "scope_for_activity_gamma_predict_basin.rds"))

soa_gamm %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_labels


glimpse(as_gamm)
glimpse(soa_gamm)

as_slim <- as_gamm %>% 
  as_tibble() %>% 
  dplyr::select(doy_id, fish_basin, fit, lower, upper) %>% 
  mutate(
    type = "as"
  )

soa_slim <- soa_gamm %>%
  as_tibble() %>% 
  dplyr::select(doy_id, fish_basin, fit, lower, upper) %>% 
  mutate(
    type = "soa"
  )
soa_slim

activity <- bind_rows(as_slim, soa_slim)

activity
# activity_wide <- as_slim %>% 
#   left_join(soa_slim, by = c("doy_id", "fish_basin"))
# 
# activity_long <- activity_wide %>% 
#   pivot_longer(cols = ends_with("pred"), 
#                names_to = "type",
#                values_to = "fit") %>% 
#   dplyr::select(doy_id, type, fit) %>% 
#   mutate(
#     type = factor(stringr::str_remove(type, "_pred"), 
#                   level = c("as", "soa"))
#   )
# activity_lower <- activity_wide %>% 
#   pivot_longer(cols = ends_with("lower"), 
#                names_to = "type",
#                values_to = "lower") %>% 
#   dplyr::select(doy_id, type, lower) %>% 
#   mutate(
#     type = factor(stringr::str_remove(type, "_lower"), 
#                   level = c("as", "soa"))
#   )
# activity_upper <- activity_wide %>% 
#   pivot_longer(cols = ends_with("upper"), 
#                names_to = "type",
#                values_to = "upper") %>% 
#   dplyr::select(doy_id, type, upper) %>% 
#   mutate(
#     type = factor(stringr::str_remove(type, "_upper"), 
#                   level = c("as", "soa"))
#   )
# 
# 
# activity_long <- activity_long %>% 
#   left_join(activity_lower, by = c("doy_id", "type")) %>% 
#   left_join(activity_upper, by = c("doy_id", "type"))
# 
# 
# 
# activity_long


rect_summer <- tibble(
  season = "Summer",
  xmin = 32,
  xmax = 123,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 220,
  xmax = 305,
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
  geom_text(
    aes(x = xmin + 30, y = 231.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = 231.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) + 
  geom_line(data = activity, 
            aes(x = doy_id, y = fit, 
                colour = fish_basin, 
                linetype = type,
            ), linewidth = 1) +
  geom_ribbon(data = activity, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  fill = fish_basin,
                  group = interaction(fish_basin, type)
              ), alpha = 0.25) +
  scale_linetype(
    name = "",
    labels = c("Aerboic Scope", "Scope for Activity")
  ) + 
  scale_fill_viridis_d(name = "Capture Basin",
                       option = "B", begin = 0.35, end = 0.75, 
  ) +
  scale_colour_viridis_d(name = "Capture Basin",
                         option = "B", begin = 0.35, end = 0.75, 
  ) + 
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_labels) +
  # scale_y_continuous(breaks = seq(-20, 180, 20)) +
  # coord_cartesian(ylim = c(-25, 165)) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.90, 0.85),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date", 
       y = expression(paste("Metabolic Rate (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

# p
write_rds(p, here("Plot Objects", 
                  "Scope_of_activity_and_AS_gamm_plot_basin.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "Scope_of_activity_and_AS_gamm_basin.png"), width = 11,
       height = 8.5)
