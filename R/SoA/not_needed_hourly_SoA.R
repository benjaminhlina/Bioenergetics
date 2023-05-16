# load packages ----

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

# bring in RDS -----

rmr <- read_rds(here("Saved Data", 
                     "Hourly_RMR.rds")) 

smr <- read_rds(here("Saved Data", 
                     "Hourly_SMR.rds")) 

# view dataframes 
glimpse(rmr)
glimpse(smr)


smr <- smr %>% 
  group_by(fish_basin, hour, season, year) %>% 
  summarise(mean_smr = mean(mean_smr), 
            mean_temp = mean(mean_temp)) %>% 
  ungroup() %>% 
  arrange(hour, season, year)

smr
rmr
# ----- determine daily mean for smr and rmr -----
rmr <- rmr %>% 
  group_by(fish_basin, hour, season, year) %>% 
  summarise(mean_rmr = mean(mean_rmr)) %>% 
  ungroup() %>% 
  arrange(hour, season, year)

smr
rmr



# -------- calculate activity scope -------
scope <- smr %>% 
  left_join(rmr, by = c("fish_basin", 
                        "hour",
                        "season",
                        "year"
  ))


# cacluate activity scope 

scope <- scope %>% 
  filter(mean_rmr > mean_smr) %>% 
  mutate(SoA = mean_rmr - mean_smr) 

# look at distibution ------
ggplot(data = scope, aes(x = SoA)) + 
  geom_histogram() 

descdist(scope$SoA)

norm <- fitdist(scope$SoA, distr = "norm", method = "mle")
plot(norm)

glimpse(scope)
#  --------- start GAMM--------
m <- bam(SoA ~ fish_basin * season + 
           s(hour, by = fish_basin, bs = "cc", k = 15) +
           s(hour, by = season, bs = "cc", k = 15), 
         # family = Gamma(link = "identity"),
         # family = gaussian(link = log),

         method = "fREML",
         data = scope, 
         select = TRUE
)

par(mfrow = c(2, 2))
gam.check(m)
# -------- predict from model ---------
# look at overall effect terms -----
m_overall <- anova.gam(m, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m_overall$pTerms.table %>% 
  as_tibble(rownames = "terms") %>% 
  clean_names()
# grab inddial effect 
ind_parm <- tidy(m, parametric = TRUE) %>% 
  clean_names()
# smoother effect 
smoothers <- tidy(m) %>% 
  clean_names()

# model comparison and fit info 
m_glance <- glance(m) %>% 
  clean_names()


# view all model info ----

overall_parm
ind_parm
smoothers
m_glance

# =---- save summaries 
overall_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Activity Scope Hourly",
                                  "gamm_SoA_h_param_overall.xlsx"))
ind_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Activity Scope Hourly",
                                  "gamm_SoA_h_param_ind.xlsx"))

smoothers %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Activity Scope Hourly",
                                  "gamm_SoA_h_smoothers.xlsx"))
m_glance %>%
  openxlsx::write.xlsx(here::here("results",
                                  "Activity Scope Hourly",
                                  "gamm_SoA_h_model_fit.xlsx"))
# # # pridicted model
# create new datafr
dat_2 <- scope

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m, newdata = dat_2, 
                    type = "response", se = TRUE)


# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit) %>% 
  arrange(hour)
# figure out where your shading for summer and winter goes 

glimpse(predicts)
# ---------- plot ----------




ggplot() + 
  # geom_rect(data = rect_summer, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
  #           fill = "grey80",
  #           alpha = 0.75,
  #           inherit.aes = FALSE) +
  # geom_rect(data = rect_winter, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
  #           fill ="grey80",
  #           alpha = 0.75,
  #           inherit.aes = FALSE) +
  # geom_text(
  #   aes(x = xmin + 25, y = 4.00, label = season),
  #   data = rect_summer,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_text(
  #   aes(x = xmin + 30, y = 4.00, label = season),
  #   data = rect_winter,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = scope, aes(x = hour, y = SoA, 
                            colour = fish_basin), 
             size = 3, alpha = 0.5) + 
  geom_line(data = predicts, 
            aes(x = hour, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(data = predicts, 
              aes(ymin = lower,
                  ymax = upper,
                  x = hour, y = fit,
                  fill = fish_basin), alpha = 0.25) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) + 
  facet_rep_wrap(.~ season, repeat.tick.labels = TRUE, nrow = 2) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        # strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date", 
       y = expression(paste("Scope-for-Activity (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p

write_rds(p, here("Plot Objects", 
                  "SoA_hourly_gamm_plot.rds"))

ggsave(plot = p, filename = here("plots",
                                 "SoA_hourly_gamm.png"), width = 11,
       height = 7 )
