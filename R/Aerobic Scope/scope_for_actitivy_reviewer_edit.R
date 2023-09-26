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

rmr <- read_rds(here("Saved Data", 
                     "Daily_RMR.rds")) %>% 
  arrange(date)

mmr <- read_rds(here("Saved Data", 
                     "Daily_MMR.rds")) %>% 
  arrange(date)

# view dataframes 
glimpse(rmr)
glimpse(mmr)


# ----- determine daily mean for mmr and rmr -----
rmr_sum <- rmr %>%
  group_by(fish_basin, date, doy, week, month, season, year) %>% 
  summarise(mean_rmr = mean(mean_rmr)) %>% 
  group_by()

mmr_sum <- mmr %>%
  group_by(fish_basin, date, doy, week, month, season, year) %>% 
  summarise(mean_mmr = mean(mean_mmr), 
            mean_temp = mean(mean_temp)) %>% 
  group_by()

glimpse(mmr_sum)
glimpse(rmr_sum)

tail(mmr)
tail(rmr)


# -------- calculate activity scope -------
fs <- mmr_sum %>% 
  left_join(rmr_sum, by = c("fish_basin", 
                            "date",
                            # "doy_id", 
                            "doy", 
                            "week", "month",
                            "season",
                            "year"
  ))


# cacluate activity scope 

fs <- fs %>% 
  mutate(fs = mean_mmr - mean_rmr, 
         doy_id = days(date), 
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec", "Jan",
                                       "Feb", "Mar", "Apr")), 
         season = forcats::fct_relevel(season, "Spring", "Summer", 
                                       "Fall", "Winter")) %>% 
  filter(fs >= 0)

fs %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_labels


glimpse(fs)
month_labels
# look at distibution ------
ggplot(data = fs, aes(x = fs)) + 
  geom_histogram() 

fs_scope <- fs %>% 
  filter(fs != is.na(fs)) %>% 
  .$fs

descdist(fs_scope)


fit_gamma <- fitdist(fs_scope, distr = "norm", method = "mle")
plot(fit_gamma)

fs <- fs %>% 
  arrange(year, doy_id) %>% 
  group_by(year) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(year, doy_id) %>% 
  mutate(
    year = as.factor(year)
  )

fs
#  --------- start GAMM--------
m <- bam(fs ~ 
           fish_basin + 
           s(doy_id, by = fish_basin, bs = "cc", k = 13) + 
           ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(13, 3)) + 
           s(year, by = fish_basin, bs = "re", k = 2), 
         # family = Gamma(), 
         method = "fREML",
         data = fs, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 4)
r1

m1 <- update(m,
             discrete = TRUE,
             rho = r1, 
             AR.start = start_event
             
)


par(mfrow = c(2, 2))
gam.check(m)
gam.check(m1)

plot(m1)
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

# =---- save summaries -----


dat_2 <- fs %>% 
  mutate(
    year = "0"
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.gam(m1, newdata = dat_2, se.fit = TRUE, exclude = "s(year)"
                    )


# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit) %>% 
  arrange(doy_id)
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
  geom_point(data = fs, aes(x = doy_id, y = fs, 
                             colour = fish_basin), 
             size = 3, alpha = 0.5) + 
  geom_line(data = predicts, 
            aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(data = predicts, 
              aes(ymin = lower,
                  ymax = upper,
                  x = doy_id, y = fit,
                  fill = fish_basin), alpha = 0.25) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) + 
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_labels) +
  scale_y_continuous(breaks = seq(-20, 180, 20)) +
  coord_cartesian(ylim = c(-25, 165)) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date", 
       y = expression(paste("Scope of Activity (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p

write_rds(p, here("Plot Objects", 
                  "Scope_of_activity_gamm_plot.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "factoral_scope_doy_gamm.png"), width = 11,
       height = 8.5)


