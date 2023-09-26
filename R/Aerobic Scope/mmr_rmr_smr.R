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

dat_list <- list(as = as_gamm, mmr = mmr_gamm,
                 rmr = rmr_gamm, smr = smr_gamm)

slim_dat <- dat_list %>% 
  map(., ~ .x %>% 
        dplyr::select(doy_id, fit, lower, upper)) %>% 
  bind_rows(.id = "type")
slim_dat



# create month labels 
smr_gamm %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label

# plotting prep -------

# figure out where your shading for summer and winter goes 

rect_summer <- tibble(
  season = "Summer",
  xmin = 32,
  xmax = 123,
  ymin = -Inf,
  ymax = Inf
)

rect_winter <- tibble(
  season = "Winter",
  xmin = 215,
  xmax = 305,
  ymin = -Inf,
  ymax = Inf
)



# ---- filter and remove aerobic scope ---- 

slim_dat_a <- slim_dat %>% 
  filter(
    type != "as"
  )


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
  geom_text(
    aes(x = xmin + 30, y = 198.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = 198.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) + 
  geom_line(data = slim_dat_a, 
            aes(x = doy_id, y = fit, 
                colour = type
            ), linewidth = 1) +
  geom_ribbon(data = slim_dat_a, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
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
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  coord_cartesian(ylim = c(20, 200)) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.9, 0.92),
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

# p
write_rds(p, here("Plot Objects", 
                  "mmr_rmr_smr.rds"))
