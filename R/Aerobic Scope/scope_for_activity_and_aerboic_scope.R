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
                "aerobic_scope_gamma_predict.rds"))
soa_gamm <- read_rds(here("Saved Data", 
                         "scope_for_activity_gamma_predict.rds"))

soa_gamm %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_labels


glimpse(as_gamm)
glimpse(soa_gamm)

# as_slim <- as_gamm %>% 
#   group_by(doy_id) %>% 
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
#     doy_id
#   ) %>% 
#   summarise(
#     soa_pred = mean(fit),
#     soa_upper = mean(upper),
#     soa_lower = mean(lower)
#   ) %>% 
#   ungroup()


as_slim <- as_gamm %>% 
  as_tibble() %>% 
  dplyr::select(doy_id, fit, lower, upper) %>% 
  mutate(
    type = "as"
  )

soa_slim <- soa_gamm %>%
  as_tibble() %>% 
  dplyr::select(doy_id, fit, lower, upper) %>% 
  mutate(
    type = "soa"
  )

soa_slim
as_slim

activity <- bind_rows(as_slim, soa_slim)




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
    aes(x = xmin + 30, y = 161.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = 161.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) + 
  geom_line(data = activity, 
            aes(x = doy_id, y = fit, 
                colour = type
            ), linewidth = 1) +
  geom_ribbon(data = activity, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  fill = type, 
              ), alpha = 0.25) +
  scale_fill_viridis_d(name = "",
                       option = "B", begin = 0.35, end = 0.75, 
                       labels = c("Aerboic Scope", "Scope for Activity")) +
  scale_colour_viridis_d(name = "",
                         option = "B", begin = 0.35, end = 0.75, 
                         labels = c("Aerboic Scope", "Scope for Activity")) + 
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_labels) +
  # scale_y_continuous(breaks = seq(-20, 180, 20)) +
  # coord_cartesian(ylim = c(-25, 165)) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.90, 0.92),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date", 
       y = expression(paste("Metabolic Rate (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
write_rds(p, here("Plot Objects", 
                  "Scope_of_activity_and_AS_gamm_plot.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "Scope_of_activity_and_AS_gamm.png"), width = 11,
       height = 8.5)
