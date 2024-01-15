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
                         "aerobic_scope_gamma_predict.rds")) %>% 
  as_tibble()


mmr_gamm <- read_rds(here("Saved Data",
                          "mmr_gamm_predict.rds"))

smr_gamm <- read_rds(here("Saved Data", 
                          "smr_gamma_predict.rds")) %>% 
  as_tibble()

rmr_gamm <- read_rds(here("Saved Data", 
                          "rmr_gamm_predict.rds"))

glimpse(as_gamm)
glimpse(rmr_gamm)
glimpse(smr_gamm)


mean_mmr <- read_rds(here::here("Model Objects", 
                                "mean_mmr.rds")) %>% 
  rename(
    mmr = mean_mmr
  ) %>% 
  pivot_longer(
    cols = -doy,
    # cols = -doy, 
               names_to = "type", 
               values_to = "est")

mean_rmr <- read_rds(here::here("Model Objects", 
                                "mean_rmr.rds")) %>% 
  pivot_longer(
    # cols = -doy, 
    cols = -doy,
               names_to = "type", 
               values_to = "est")
mean_smr <- read_rds(here::here("Model Objects", 
                                "mean_smr.rds")) %>% 
  rename(
    smr = mean_smr) %>% 
  dplyr::select(-mean_temp) %>% 
  pivot_longer(
    # cols = -doy, 
    cols = -doy,
               names_to = "type", 
               values_to = "est")
glimpse(mean_mmr)
glimpse(mean_rmr)
glimpse(mean_smr)

met_pnts <- bind_rows(
  mean_mmr, mean_rmr, mean_smr
)


dat_list <- list(mmr = mmr_gamm,
                 rmr = rmr_gamm, smr = smr_gamm)

slim_dat <- dat_list %>% 
  map(., ~ .x %>% 
        dplyr::select(doy, doy, fit, lower, upper)) %>% 
  bind_rows(.id = "type")
slim_dat



# create month lablel ----- 

month_doy <- mmr_gamm %>% 
  group_by(month_abb) %>% 
  summarise(first = first(doy),
            last = last(doy)) %>% 
  ungroup() %>% 
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan", 
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>% 
  arrange(month_abb) %>%   
  .$first
mmr_gamm %>% 
  filter(doy %in% month_doy) %>%
  group_by(month_abb) %>% 
  summarise() %>% 
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan", 
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>% 
  arrange(month_abb) %>% 
  .$month_abb -> month_label 
month_label
# plotting prep -------

# figure out where your shading for summer and winter goes 
predicts %>% 
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


# ---- filter and remove aerobic scope ---- 

# slim_dat_a <- slim_dat %>% 
#   filter(
#     type != "as"
#   )


# ------ plot -----
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
    aes(x = xmin + 31, y = 208.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 17.5, y = 208.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) + 
  geom_text(
    aes(x = xmin + 3.75, y = 208.25, label = season),
    data = rect_winter_dec,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = met_pnts, aes(x = doy, y = est, colour = type), 
             alpha = 0.25) +
  geom_line(data = slim_dat, 
            aes(x = doy, y = fit, 
                colour = type
            ), linewidth = 1) +
  geom_ribbon(data = slim_dat, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy, y = fit,
                  fill = type, 
              ), alpha = 0.25) +
  scale_fill_viridis_d(name = "",
                       option = "D", begin = 0.35, end = 0.75, 
                       labels = c("Maximum \nMetabolism (MMR)", 
                                  "Active \nMetabolism (RMR)",
                                  "Standard \nMetabolism (SMR)")) +
  scale_colour_viridis_d(name = "",
                         option = "D", begin = 0.35, end = 0.75, 
                         labels = c("Maximum \nMetabolism (MMR)", 
                                    "Active \nMetabolism (RMR)",
                                    "Standard \nMetabolism (SMR)")) +
  scale_x_continuous(breaks = month_doy, 
                     label = month_label) +
  scale_y_continuous(breaks = seq(0, 220, 20)) +
  coord_cartesian(ylim = c(20, 210)) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.30, 0.88),
        legend.spacing.y = unit(0.75, 'cm'),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5),
        legend.background = element_blank()) +
  guides(
    fill = guide_legend(byrow = TRUE),
    colour = guide_legend(byrow = TRUE),
  ) + 
  labs(x = "Date", 
       y = expression(paste("Metabolic Rate (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
write_rds(p, here("Plot Objects", 
                  "mmr_rmr_smr_jan.rds"))
