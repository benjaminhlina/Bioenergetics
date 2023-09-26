{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(forcats)
  library(ggplot2)
  library(ggh4x)
  library(gamm4)
  library(gratia)
  library(here)
  library(itsadug)
  library(lubridate)
  library(lemon)
  library(lme4)
  library(janitor)
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

# ---- bring in to calcuate daily theroritcal scope ---- 


dat <- read_rds(here("Saved Data",
                     "Daily_theoratical_smr_mmr.rds")) 

glimpse(dat)

dat <- dat %>% 
  filter(!(floy_tag %in% "07478"))

dat

# ---- model ----

aec <- dat$aerobic_scope

descdist(aec)

ggplot(data = dat, aes(x = aerobic_scope)) + 
  geom_histogram()

fit_gamma <- fitdist(aec, distr = "gamma", method = "mme")
fit_norm <- fitdist(aec, distr = "norm", method = "mle")

plot(fit_gamma)
plot(fit_norm)

dat <- dat %>% 
  group_by(year) %>% 
  arrange(year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE), 
         year = factor(year)) %>% 
  ungroup() %>% 
  arrange(date, start_event)



dat %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  mutate(
    month_abb = month(date, label = TRUE)
  ) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  mutate(
    month_abb = fct_relevel(month_abb, "May", 
                                     "Jul", "Oct", "Dec", "Feb", 
                                     "Apr")
  ) %>% 
  arrange(month_abb) %>% 
  .$month_abb -> month_labels

month_labels

#  --------- start GAMM--------
m <- bam(aerobic_scope ~  s(doy_id, bs = "cc", k = 15) + 
           # ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(14, 3)) + 
           s(floy_tag,  bs = "re") + 
           s(year, bs = "re"), 
         # family = gaussian(link = "log"), 
         family = Gamma(link = "inverse"),
         method = "fREML",
         data = dat, 
         select = TRUE
)


acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 17)
r1

m1 <- update(m,
             discrete = TRUE,
             rho = r1, 
             AR.start = start_event
             
)


par(mfrow = c(2, 2))
gam.check(m)
gam.check(m1)
plot(m)


# -------- predict from model ---------
# look at overall effect terms -----
m_overall <- anova.gam(m1, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m_overall$pTerms.table %>% 
  as_tibble(rownames = "terms") %>% 
  clean_names()
# grab inddial effect 
ind_parm <- tidy(m1, parametric = TRUE) %>% 
  clean_names()
# smoother effect 
smoothers <- tidy(m1) %>% 
  clean_names()

# model comparison and fit info 
m_glance <- glance(m1) %>% 
  clean_names()


# view all model info ----

overall_parm
ind_parm
smoothers
m_glance

# # pridicted model
# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- dat %>% 
  mutate(
    floy_tag = "a", 
    year = "0"
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.gam(m1, newdata = dat_2, 
                    se.fit = TRUE, exclude = c("s(floy_tag)", "s(year)")
                    )


# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(
    
    # lower = fit - 1.96 * se.fit,
    lower = 1/(fit - 1.96 * se.fit),
    # upper = fit + 1.96 * se.fit, 
    upper = 1/(fit + 1.96 * se.fit),
    fit = 1/(fit)
    ) %>% 
  arrange(doy_id)



write_rds(predicts, here("Saved Data", 
                         "aerobic_scope_gamma_predict.rds"))
# figure out where your shading for summer and winter goes 
predicts %>% 
  group_by(season) %>% 
  summarise(first = first(doy_id),
            last = last(doy_id)) %>% 
  ungroup()

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



as <- dat %>% 
  group_by(doy_id
  ) %>% 
  summarise(
    mean_as = mean(aerobic_scope), 
    sd_as = sd(aerobic_scope),
    sem_as = sd(aerobic_scope) / sqrt(n())
  ) %>% 
  ungroup()


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
  geom_point(data = as, aes(x = doy_id, y = mean_as, 
                             # colour = fish_basin
                            ), 
             size = 3, alpha = 0.5) + 
  # geom_errorbar(data = as, aes(x = doy_id, y = mean_as, 
  #                            colour = fish_basin, 
  #                            ymin = mean_as - sem_as, 
  #                            ymax = mean_as + sem_as, 
  #                            ), width = 0.1) + 
  geom_line(data = predicts, 
            aes(x = doy_id, y = fit, 
                # colour = fish_basin
                ), size = 1) +
  geom_ribbon(data = predicts, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  # fill = fish_basin
                  ), alpha = 0.25) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) + 
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_labels) +
  scale_y_continuous(breaks = seq(0, 225, 25)) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date", 
       y = expression(paste("Aerboic Scope (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p

write_rds(p, here("Plot Objects", 
                  "areobic_activity_gamm_plot_no_basin.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "aerobic_scope_doy_gamm_no_basin.png"), width = 11,
       height = 7)
