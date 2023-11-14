
# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(ggplot2)
  library(ggh4x)
  library(gamm4)
  library(gamlss)
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
# 
# ful <- read_rds(here("Saved Data",
#                      "BioE_lt.rds"))
# 
# glimpse(ful)
##### SUBSET OUT ONLY RMR/M_Swim DATA and determine daily temp FOR 2017 - 2021-------


# ful_rmr <- ful %>%
#   filter(sensor_unit %in% "m/s²" &
#            year %in% c(2019, 2020)) %>%
#   filter(!m_swim == is.nan(m_swim)) %>%
#   group_by(floy_tag, weight, fish_basin, date,
#            week, month, season, year,
#            sensor_unit, labels) %>%
#   summarise(mean_rmr = mean(m_swim)) %>%
#   ungroup() %>%
#   mutate(
#     floy_tag = factor(floy_tag),
#     fish_basin = factor(
#       stringr::str_replace(as.character(fish_basin), " Basin", ""),
#       levels = c("East", "West", "North")),
#     doy = yday(date),
#     month = factor(month,
#                    levels = c("May", "June", "July",
#                               "August", "September", "October",
#                               "Novemeber", "December", "January",
#                               "February", "March", "April")),
#     doy = days(date, end = "05-22"),
#     season = forcats::fct_relevel(season, "Spring", "Summer",
#                                   "Fall", "Winter")
#   ) %>%
#   arrange(date) %>%
#   dplyr::select(floy_tag:sensor_unit, doy, doy, mean_rmr)
# glimpse(ful_rmr)
# 
# write_rds(ful_rmr, here("Saved Data",
#                          "Daily_RMR.rds"))
# # remove big objects to free up RAM -----
# # rm(ful)
# gc()


ful_rmr <- read_rds(here("Saved Data", 
                         "Daily_RMR.rds"))

glimpse(ful_rmr)
# look at data to see what dist it fits 

descdist(ful_rmr$mean_rmr) 

norm <- fitdist(ful_rmr$mean_rmr, distr = "norm", method = "mle")
plot(norm)
gammas <- fitdist(ful_rmr$mean_rmr, distr = "gamma", method = "mme")
plot(gammas)

ggplot(data = ful_rmr, aes(x = mean_rmr)) + 
  geom_histogram()

# ful_rmr %>% 
#   group_by(date, fish_basin) %>% 
#   summarise(mean_rmr = mean(mean_rmr)) %>% 
#   ungroup() %>% 
# ggplot() +
# geom_point(aes(x = date, y = mean_rmr,
#                                 colour = fish_basin), 
#            alpha = 0.25, size = 2) +
#   
# scale_colour_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   scale_shape_discrete(name = "Basin") +
#   scale_fill_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
#   #                # ncol = 1
#   # ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         legend.position = c(0.95, 0.92),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Date",
#        y = "Daily Temperature (°C)")
ful_rmr <-  ful_rmr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_rmr)

ful_rmr_test <- ful_rmr %>% 
  group_by(doy) %>% 
  summarise(
    rmr_mean = mean(mean_rmr)
  ) %>% 
  ungroup() %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE))


ggplot(data = ful_rmr_test, aes(x = rmr_mean)) + 
  geom_histogram()


# -----------------------START GAMMS -------------------------------
m <- bam(mean_rmr ~ 
           # fish_basin + 
           s(doy, 
             # by = fish_basin, 
             bs = "cc", k = 15) +
           s(floy_tag, 
             # by = fish_basin,
             bs = c("re")),
         method = "fREML",
         family = gaussian(link = "identity"),
         data = ful_rmr, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 11)
r1



m1 <- update(m, 
             discrete = TRUE,
             rho = r1, 
             AR.start = ful_rmr$start_event
)
# m2 <- bam(mean_rmr ~
#             # fish_basin +
#             s(doy,
#               # by = fish_basin,
#               bs = "cc", k = 15),
#           # s(floy_tag,
#           #   # by = fish_basin,
#           #   bs = c("re")),
#           # ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3)),
#           method = "fREML",
#           # family = gaussian(link = "identity"),
#           data = ful_rmr,
#           select = TRUE
# )
# 
# acf(resid_gam(m2))
# 
# r2 <- itsadug::start_value_rho(m2, plot = TRUE, lag = 11)
# r2
# 
# 
# 
# m3 <- update(m2,
#              discrete = TRUE,
#              rho = r2,
#              AR.start = ful_rmr$start_event
# )
# # check model fit -----
# par(mfrow = c(2, 2))
# gam.check(m)
# summary(m)
# gam.check(m1)
summary(m1)
# gam.check(m3)
# summary(m3)
appraise(m1)
draw(m1)
# look at overall effect terms -----
m_overall <- anova.gam(m1, freq = FALSE)

# grab parametic overall effect 
overall_parm <- m_overall$pTerms.table %>% 
  as_tibble(rownames = "term") %>% 
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

smoothers$statistic
# =---- save summaries ------

# overall_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "RMR results",
#                                   "gamm_rmr_param_overall.xlsx"))
# ind_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "RMR results",
#                                   "gamm_rmr_param_ind.xlsx"))
# 
# smoothers %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "RMR results",
#                                   "gamm_rmr_smoothers.xlsx"))
# m_glance %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "RMR results",
#                                   "gamm_rmr_model_fit.xlsx"))
# pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_rmr %>% 
  mutate(
    floy_tag = "a", 
    # year = 0
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.gam(m1, newdata = dat_2, 
                    # type = "response", 
                    se = TRUE, discrete = TRUE,  
                    exclude = c("s(floy_tag)"),
                    # newdata.guaranteed = TRUE
)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>%
  as_tibble() %>% 
  mutate(
    lower = (fit - 1.96 * se.fit),
    
    upper = (fit + 1.96 * se.fit),
    # lower = 1 / (fit - 1.96 * se.fit),
    # 
    # upper = 1 / (fit + 1.96 * se.fit),
    # fit = 1 / fit,
    # lower = exp(1) ^ (fit - 1.96 * se.fit),
    # 
    # upper = exp(1) ^ (fit + 1.96 * se.fit),
    # fit = exp(1) ^ fit,
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec", "Jan",
                                  "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy)

# double check that predicts looks correct 
glimpse(predicts) 


write_rds(predicts, here("Saved Data", 
                         "rmr_gamm_predict.rds"))

glimpse(dat_2)

# use prediction to get interpolated points 
# fits_2 <- predict.bam(m3, newdata = ful_rmr_test, 
#                       # type = "response", 
#                       se = TRUE, 
#                       # discrete = FALSE,  
#                       # exclude = c("s(floy_tag)"),
#                       # newdata.guaranteed = TRUE
# )



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
# predicts_2 <- data.frame(ful_rmr_test, fits_2) %>%
#   as_tibble() %>% 
#   mutate(lower = fit - 1.96 * se.fit,
#          upper = fit + 1.96 * se.fit, 
#   )
# month_abb = month(date, label = TRUE, abbr = TRUE), 
#        month_abb = factor(month_abb, 
#                           levels = c("May", "Jun", "Jul", 
#                                      "Aug", "Sep", "Oct",
#                                      "Nov", "Dec", "Jan",
#                                      "Feb", "Mar", "Apr"))) %>% 
# arrange(floy_tag, doy)

# double check that predicts looks correct 
glimpse(predicts) 

# calculate daily mean temp by fish basin 
# ful %>%
#   filter(sensor_unit %in% "m/s²" & 
#            year %in% c(2019, 2022)) %>%
#   group_by(date, fish_basin) %>%
#   summarise(mean_rmr = mean(m_swim)) %>%
#   ungroup() -> mean_rmr_right
ful_rmr %>%
  group_by(
    # doy, 
           doy
           # fish_basin
  ) %>%
  summarise(rmr = mean(mean_rmr),
            # sem = sd(mean_rmr) / sqrt(n())
  ) %>%
  ungroup() -> mean_rmr


write_rds(mean_rmr, here::here("Model Objects", 
                               "mean_rmr.rds"))
# 
# glimpse(ful_rmr)
# glimpse(mean_rmr)
# 
# glimpse(predicts)
# 
# predicts %>% 
#   filter(month %in% "May" & year == 2019) %>% 
#   tail()
# 
# summary(predicts$doy)
# # create month labels 
# month_doy <- predicts %>% 
#   group_by(month_abb) %>% 
#   summarise(first = first(doy),
#             last = last(doy)) %>% 
#   ungroup() %>% 
#   mutate(
#     # month_abb = forcats::fct_relevel(month_abb, "Jan", 
#     #                                  "Feb", "Mar", "Apr", "May", "Jun",
#     #                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   ) %>% 
#   arrange(month_abb) %>% 
#   mutate(
#     # first = case_when(
#       # month_abb %in% "May" ~ 1, 
#       # month_abb %in% "June" ~ 10 + 20, false = first
#     # )
#   ) %>%
#   .$first
# 
# month_doy
# # month_doy <- c(1, 32, 62, 93, 123, 154, 184, 215, 246, 274, 305, 335)
# predicts %>% 
#   filter(doy %in% month_doy) %>%
#   group_by(month_abb) %>% 
#   summarise() %>% 
#   # mutate(
#   #   month_abb = forcats::fct_relevel(month_abb, "Jan", 
#   #                                    "Feb", "Mar", "Apr", "May", "Jun",
#   #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   # ) %>% 
#   arrange(month_abb) %>% 
#   .$month_abb -> month_label 
# month_label
# 
# # plotting prep -------
# 
# # figure out where your shading for summer and winter goes 
# predicts %>% 
#   group_by(season) %>% 
#   summarise(first = first(doy),
#             last = last(doy)) %>% 
#   ungroup()
# 
# 
# rect_summer <- tibble(
#   season = "Summer",
#   xmin = 32,
#   xmax = 124,
#   ymin = -Inf,
#   ymax = Inf
# )
# 
# rect_winter <- tibble(
#   season = "Winter",
#   xmin = 215,
#   xmax = 305,
#   ymin = -Inf,
#   ymax = Inf
# )
# rect_winter_dec <- tibble(
#   season = "Winter",
#   xmin = 335,
#   xmax = 365,
#   ymin = -Inf,
#   ymax = Inf
# )
# 
# predicts %>% 
#   filter(doy == 335)
# # ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
# ggplot(data = predicts) +
#   geom_rect(data = rect_summer, aes(xmin = xmin,
#                                     xmax = xmax,
#                                     ymin = ymin,
#                                     ymax = ymax),
#             fill = "grey80",
#             alpha = 0.75,
#             inherit.aes = FALSE) +
#   geom_rect(data = rect_winter, aes(xmin = xmin,
#                                     xmax = xmax,
#                                     ymin = ymin,
#                                     ymax = ymax),
#             fill ="grey80",
#             alpha = 0.75,
#             inherit.aes = FALSE) +
#   # geom_rect(data = rect_winter_dec, aes(xmin = xmin,
#   #                                       xmax = xmax,
#   #                                       ymin = ymin,
#   #                                       ymax = ymax),
#   #           fill ="grey80",
#   #           alpha = 0.75,
#   #           inherit.aes = FALSE) +
#   # geom_text(
#   #   aes(x = xmin + 30, y = 125, label = season),
#   #   data = rect_summer,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_text(
#   #   aes(x = xmin + 32, y = 125, label = season),
#   #   data = rect_winter,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_point(data = mean_rmr, aes(x = doy, y = mean_rmr,
#   #                                  colour = fish_basin,
#   # ), alpha = 0.25, size = 2.75) +
# geom_point(data = mean_rmr, aes(x = doy, y = rmr,
#                                 # colour = fish_basin,
# ), alpha = 0.5, size = 3) +
#   # geom_errorbar(data = mean_rmr, aes(x = doy, ymin = rmr - sem, 
#   #                                    ymax = rmr + sem)) + 
#   geom_line(
#     aes(x = doy, y = fit, 
#         # colour = fish_basin
#     ), linewidth = 1) +
#   geom_ribbon(
#     aes(ymin = lower,
#         ymax = upper,
#         x = doy, y = fit,
#         # fill = fish_basin
#     ), alpha = 0.25) +
#   # scale_y_continuous(breaks = seq(45, 135, 15)) +
#   scale_x_continuous(breaks = month_doy,
#                      label = month_label) +
#   # scale_colour_viridis_d(name = "Basin",
#   #                        option = "B", begin = 0.35, end = 0.75) +
#   scale_shape_discrete(name = "Basin") +
#   # scale_fill_viridis_d(name = "Basin",
#   #                      option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
#   #                # ncol = 1
#   # ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         legend.position = c(0.95, 0.92),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Date",
#        y = expression(paste("Active Metabolism (mg", 
#                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 
# 
# p
# 
# 
# write_rds(p, here("Plot Objects", 
#                   "daily_rmr_GAMM_plot.rds"))
# 
# 
# ggsave(plot = p, filename = here("plots",
#                                  "Daily GAMM Plots", 
#                                  "gamm_BioE_doy.png"), width = 11,
#        height = 7 )
# 
# ggplot(data = predicts_2) +
#   geom_rect(data = rect_summer, aes(xmin = xmin,
#                                     xmax = xmax,
#                                     ymin = ymin,
#                                     ymax = ymax),
#             fill = "grey80",
#             alpha = 0.75,
#             inherit.aes = FALSE) +
#   geom_rect(data = rect_winter, aes(xmin = xmin,
#                                     xmax = xmax,
#                                     ymin = ymin,
#                                     ymax = ymax),
#             fill ="grey80",
#             alpha = 0.75,
#             inherit.aes = FALSE) +
#   # geom_text(
#   #   aes(x = xmin + 30, y = 125, label = season),
#   #   data = rect_summer,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_text(
#   #   aes(x = xmin + 32, y = 125, label = season),
#   #   data = rect_winter,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_point(data = mean_rmr, aes(x = doy, y = mean_rmr,
#   #                                  colour = fish_basin,
#   # ), alpha = 0.25, size = 2.75) +
# # geom_point(data = mean_rmr, aes(x = doy, y = mean_rmr,
# #                                 # colour = fish_basin,
# # ), alpha = 0.5, size = 3) +
# 
# geom_line(
#   aes(x = doy, y = fit), linewidth = 1) +
#   geom_ribbon(
#     aes(ymin = lower,
#         ymax = upper,
#         x = doy, y = fit,
#         # fill = fish_basin
#     ), alpha = 0.25) +
#   # scale_y_continuous(breaks = seq(45, 135, 15)) +
#   scale_x_continuous(breaks = seq(09, 344, 67),
#                      label = month_label) +
#   scale_colour_viridis_d(name = "Basin",
#                          option = "B", begin = 0.35, end = 0.75) +
#   scale_shape_discrete(name = "Basin") +
#   scale_fill_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   # facet_rep_wrap(.~ floy_tag, repeat.tick.labels = TRUE,
#   #                # ncol = 1
#   # ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         legend.position = c(0.95, 0.92),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Date",
#        y = expression(paste("Active Metabolism (mg", 
#                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1
# 
# p1
# # ------------ plot each fish's profile facted --------
# dat_3 <- ful_rmr 
# 
# glimpse(dat_3)
# 
# # use prediction to get interpolated points 
# fits_id <- predict.bam(m1, newdata = dat_3, 
#                        type = "response", se = TRUE)
# 
# 
# 
# # calcuate lower and upper ci per id add month abb 
# pred_id <- data.frame(dat_3, fits_id) %>% 
#   mutate(lower = fit - 1.96 * se.fit,
#          upper = fit + 1.96 * se.fit,
#          month_abb = month(date, label = TRUE, abbr = TRUE), 
#          month_abb = factor(month_abb, 
#                             levels = c("May", "Jun", "Jul", 
#                                        "Aug", "Sep", "Oct",
#                                        "Nov", "Dec", "Jan",
#                                        "Feb", "Mar", "Apr"))) %>% 
#   arrange(floy_tag, doy)
# 
# 
# 
# # double check that predicts looks correct 
# glimpse(pred_id)
# # create month labels 
# pred_id %>% 
#   filter(doy %in% seq(25, 350, 65)) %>% 
#   group_by(month_abb) %>% 
#   summarise() %>% 
#   .$month_abb -> month_label 
# month_label
# 
# 
# 
# 
# 
# 
# # ---- plot 2017 - 2020 gamm with mean daily temp per ID ------
# ggplot() +
#   geom_point(data = ful_rmr, aes(x = doy, y = mean_rmr,
#                                  colour = fish_basin,
#   ), alpha = 0.25, size = 2) +
#   geom_line(data = pred_id, 
#             aes(x = doy, y = fit, colour = fish_basin), size = 1) +
#   geom_ribbon(data = pred_id, aes(ymin = lower,
#                                   ymax = upper,
#                                   x = doy, y = fit,
#                                   fill = fish_basin), alpha = 0.5) +
#   # scale_y_continuous(breaks = seq(20, 60, 10)
#   # ) +
#   scale_x_continuous(breaks = seq(25, 350, 65), 
#                      label = month_label) +
#   scale_colour_viridis_d(name = "Basin",
#                          option = "B", begin = 0.35, end = 0.75) +
#   scale_shape_discrete(name = "Basin") +
#   scale_fill_viridis_d(name = "Basin",
#                        option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") + 
#   
#   facet_rep_wrap(.~ floy_tag, repeat.tick.labels = FALSE,
#                  # ncol = 1
#   ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         axis.text = element_text(colour = "black"),
#         # axis.text.x = element_text(angle = 45, hjust = 1),
#         # legend.position = c(0.95, 0.14),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Date",
#        y = expression(paste("Active Metabolism (mg", 
#                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1
# 
# p1
# 
# ggsave(plot = p1, filename = here("plots",
#                                   "Individaul Plots",
#                                   "gamm_rmr_2017_2021_id.png"),
#        width = 11 * 2,
#        height = 7 * 2.15)
# 
