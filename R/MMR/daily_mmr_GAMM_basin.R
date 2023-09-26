# # load packages ----
{
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
} 

# ---- bring in data ----

ful_mmr <- read_rds(here("Saved Data", 
                          "Daily_MMR.rds"))  

glimpse(ful_mmr) 

# ---- look at distribution -----

ggplot(ful_mmr, aes(x = mean_mmr)) + 
  geom_histogram()


descdist(ful_mmr$mean_mmr)

fit_gamma <- fitdist(ful_mmr$mean_mmr, distr = "gamma", method = "mme")

plot(fit_gamma)

# ---- addd in start point for gamm -----
ful_mmr <-  ful_mmr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_mmr)





# -----------------------START GAMMS -------------------------------
m <- bam(mean_mmr ~ fish_basin + 
           s(doy_id, by = fish_basin, bs = "cc", k = 14) +
           s(floy_tag, by = fish_basin, bs = c("re")) + 
           ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(14, 3)),  
         method = "fREML",
         family = Gamma(link = "inverse"),
         data = ful_mmr, 
         select = TRUE
)

acf(resid_gam(m))


r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 17)
r1


m1 <- update(m, discrete = TRUE, 
             rho = r1, 
             AR.start = ful_mmr$start_event)


# check model fit -----
par(mfrow = c(2, 2))
gam.check(m)

plot(m1)

# look at overall effect terms -----
m_overall <- anova.gam(m1, freq = FALSE)
m_overall
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


write_rds(m1, file = here("model objects", 
                          "mmr_gamm_model.rds")
)

# =---- save summaries 

overall_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "MMR results",
                                  "gamm_mmr_param_overall.xlsx"))
ind_parm %>%
  openxlsx::write.xlsx(here::here("results",
                                  "MMR results",
                                  "gamm_mmr_param_ind.xlsx"))

smoothers %>%
  openxlsx::write.xlsx(here::here("results",
                                  "MMR results",
                                  "gamm_mmr_smoothers.xlsx"))
m_glance %>%
  openxlsx::write.xlsx(here::here("results",
                                  "MMR results",
                                  "gamm_mmr_model_fit.xlsx"))
# pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_mmr %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m1, newdata = dat_2, discrete = FALSE,
                    # type = "response",
                    se = TRUE, 
                    exclude = c("s(floy_tag, year)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(
    # fit = 1 / fit,
    # se.fit =  se.fit,
    lower = 1 / (fit + 1.96 * se.fit),
    upper = 1 / (fit - 1.96 * se.fit),
    fit = 1 / fit,
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec", "Jan",
                                  "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)
# predicts <- data.frame(dat_2, fits) %>% 
#   mutate(fit = exp(1) ^ fit,
#     lower = fit - 1.96 * se.fit,
#          upper = fit + 1.96 * se.fit, 
#          month_abb = month(date, label = TRUE, abbr = TRUE), 
#          month_abb = factor(month_abb, 
#                             levels = c("May", "Jun", "Jul", 
#                                        "Aug", "Sep", "Oct",
#                                        "Nov", "Dec", "Jan",
#                                        "Feb", "Mar", "Apr"))) %>% 
#   arrange(floy_tag, doy_id)

# double check that predicts looks correct 
# glimpse(predicts) 
predicts


# calculate daily mean mmr by fish basin 
ful_mmr %>%
  group_by(doy_id, fish_basin) %>% 
  summarise(
    # mean_mmr = mean(mean_mmr),
            mean_mmr = mean(mean_mmr)) %>% 
  ungroup() -> mean_mmr

# create month labels 
predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label

# plotting prep -------

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
  xmin = 215,
  xmax = 305,
  ymin = -Inf,
  ymax = Inf
)
predicts
# ---------- plot doy gamm for 2017 - 2020 with mean daily mmr ------
ggplot(predicts) +
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
    aes(x = xmin + 30, y = 64, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 32, y = 64, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  
  geom_point(data = mean_mmr, aes(x = doy_id, y = mean_mmr,
                                  colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon(
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.25) +
  # scale_y_continuous(breaks = seq(20, 60, 10)
  # ) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_colour_viridis_d(name = "Capture Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Capture Basin") +
  scale_fill_viridis_d(name = "Capture Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  
  # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
  #                # ncol = 1
  # ) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = expression(paste("Maximum Metabolic Rate (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
write_rds(p, here("Plot Objects", 
                  "daily_mmr_GAMM_plot.rds"))


ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots",
                                 "gamm_mmr_doy.png"), width = 11,
       height = 7 )
