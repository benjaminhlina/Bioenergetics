# # load packages ----
# 
# install.package("broom.mixed")
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
  source(here("R",
              "Cleaning and Calculations",
              "julian_date_reorder.r"))
}

# bring in RDS -----
# 
# ful <- read_rds(here("Saved Data",
#                      "BioE_lt.rds"))
# # 
# glimpse(ful)
# # 
# # ful_temp %>%
# #   filter(floy_tag %in% "07478"))
# #
# 
# 
# ##### SUBSET OUT ONLY TEMP DATA and determine daily temp FOR 2017 - 2021-------
# 
# ful_temp <- ful %>%
#   filter(sensor_unit %in% "°C"
#            & !(floy_tag %in% "07478")
#          ) %>%
#   group_by(floy_tag, weight, date, fish_basin,
#            week, month, season, year,
#            sensor_unit, labels) %>%
#   summarise(mean_smr = mean(smr, na.rm = TRUE),
#             sem_smr = sd(smr) / sqrt(n()), 
#             cv_smr = raster::cv(smr), 
#             mean_temp = mean(sensor_value, na.rm = TRUE), 
#             sem_temp = sd(sensor_value) / sqrt(n()), 
#             cv_temp = raster::cv(sensor_value)) %>%
#   ungroup() %>%
#   mutate(date_2 = as.numeric(date),
#          floy_tag = factor(floy_tag)) %>%
#   filter(date_2 <= 18559) %>%
#   mutate(fish_basin = factor(stringr::str_replace(as.character(fish_basin), " Basin",
#                                                   ""),
#                              levels = c("East", "West", "North")),
#          doy = yday(date),
#          month = factor(month,
#                         levels = c("May", "June", "July",
#                                    "August", "September", "October",
#                                    "Novemeber", "December", "January",
#                                    "February", "March", "April"))
#          ) %>%
#   arrange(month) %>%
#   mutate(doy = days(date)) %>%
#   arrange(date)
# 
# # 
# # # remove big objects to free up RAM -----
# # # rm(ful)
# # gc()
# # 
# glimpse(ful_temp)
# 
# 
# # tail(ful_temp)
# # # 2020-10-24
# write_rds(ful_temp, here("Saved Data",
#                          "Daily_SMR.rds"))

ful_temp <- read_rds(here("Saved Data", 
                          "Daily_SMR.rds"))  
glimpse(ful_temp)
# crete day of year (doy) variable and refactor month 
# have doy start in may and end in april... 

# -------------- check distirtubion ---------------
ggplot(data = ful_temp, aes(x = mean_smr)) + 
  geom_histogram()


gammas <- fitdist(ful_temp$mean_smr, distr = "gamma", 
                  method = "mme")

plot(gammas)


ful_temp <-  ful_temp %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_temp)

ful_temp <- ful_temp %>% 
  mutate(
    date_label = as.factor(format(as.Date(date), "%m-%d"))
  )


# coeffiencet of varation -----


coef_v <- ful_temp %>% 
  group_by(fish_basin, doy, date_label, season) %>% 
  summarise(
    mean_cv = mean(cv_smr, na.rm = TRUE), 
    sem_cv = sd(cv_smr, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup() %>% 
  arrange(season, fish_basin)
coef_v %>%
  filter(season == "Winter") %>% 
  mutate(
    date_label = reorder(date_label, doy)
  ) %>% filter(doy %in% seq(230, 290, 20)) %>% 
  distinct(date_label) -> date_labels

coef_v %>%
  filter(season == "Winter") %>% 
  ggplot(aes(x = doy, y = mean_cv)) + 
  geom_point(aes(colour = fish_basin), size = 3) + 
  geom_errorbar(aes(ymin = mean_cv - sem_cv,
                    ymax = mean_cv + sem_cv, 
                    group = fish_basin), 
                width = 0.15) + 
  theme_bw(base_size = 15) + 
  scale_x_continuous(breaks = seq(230, 290, 20), 
                     labels = date_labels$date_label) + 
  theme(panel.grid = element_blank()) + 
  scale_color_viridis_d(name = "Basin", option = "B",
                        alpha = 0.5, begin = 0.25, end = 0.75) + 
  labs(x = "Date", 
       y = "Coefficient of Varation (%)") -> p9



p9
# ggsave(here("Plots", 
#             "coefficient of varation", 
#             "smr_winter_daily_mean_cv.png"), plot = p9, 
#        height = 7, width = 11)



cv_season <- coef_v %>% 
  group_by(season, fish_basin) %>% 
  summarise(
    mean_cvs = mean(mean_cv, na.rm = TRUE), 
    sem_cvs = sd(mean_cv, na.rm = TRUE) / sqrt(n())
  )

cv_season

glimpse(ful_temp) 

ful_temp <- ful_temp %>% 
  mutate(
    year = factor(year)
  )




# possibly add temp used to then model what temps fish may see 
# -----------------------START GAMMS -------------------------------
m <- bam(mean_smr ~ 
           s(doy, bs = "cc", k = 18) +
           s(floy_tag, bs = c( "re"), 
             k = c(20)) +
           s(year, bs = "re", 
             k = c(4)), 
         
         method = "fREML",
         family = Gamma(link = "inverse"),
         data = ful_temp, 
         select = TRUE
)

acf(resid_gam(m))


r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 17)
r1


m1 <- update(m, discrete = TRUE, 
             rho = r1, 
             AR.start = start_event)
m2 <- bam(mean_smr ~ 
           s(doy, bs = "cc", k = 18) +
           s(floy_tag, bs = c( "re"), 
             k = c(20)) +
           s(year, bs = "re"), 
         
         method = "fREML",
         family = Gamma(link = "identity"),
         data = ful_temp, 
         select = TRUE
)

acf(resid_gam(m2))


r1 <- itsadug::start_value_rho(m2, plot = TRUE, lag = 17)
r1


m3 <- update(m2, discrete = TRUE, 
             rho = r1, 
             AR.start = start_event)



summary(m3)
summary(m1)
# check model fit -----
# par(mfrow = c(2, 2))
# gam.check(m1)
# plot(m1)
appraise(m1)
appraise(m3)
# draw(m1)

summary(m1)
summary(m3)
anova(m3)

anova(m1)
anova(m1, m3, test = "Chisq")
AIC(m1)
AIC(m3)



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


# write_rds(m1, file = here("model objects", 
#                           "smr_gamm_model.rds")
# )
# 
# # =---- save summaries 
# 
# overall_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "SMR results",
#                                   "gamm_smr_param_overall.xlsx"))
# ind_parm %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "SMR results",
#                                   "gamm_smr_param_ind.xlsx"))
# 
# smoothers %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "SMR results",
#                                   "gamm_smr_smoothers.xlsx"))
# m_glance %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "SMR results",
#                                   "gamm_smr_model_fit.xlsx"))
# pridicted model --------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_temp %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m3, newdata = dat_2, discrete = FALSE,
                    # type = "response", 
                    se = TRUE, 
                    exclude = c("s(floy_tag)", "s(year)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(
    
    # lower = 1 / (fit - 1.96 * se.fit),
    # upper = 1 / (fit + 1.96 * se.fit),
    # fit = 1 / (fit),
    lower = (fit - 1.96 * se.fit),
    upper = (fit + 1.96 * se.fit),
    # fit = 1 / (fit),
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec", "Jan",
                                  "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy)
# predicts_log <- data.frame(dat_2, fits) %>% 
#   mutate(
#     
#     lower = exp(1) ^ (fit - 1.96 * se.fit),
#     upper = exp(1) ^ (fit + 1.96 * se.fit), 
#     fit = exp(1) ^ (fit),
#     month_abb = month(date, label = TRUE, abbr = TRUE), 
#     month_abb = factor(month_abb, 
#                        levels = c("May", "Jun", "Jul", 
#                                   "Aug", "Sep", "Oct",
#                                   "Nov", "Dec", "Jan",
#                                   "Feb", "Mar", "Apr"))) %>% 
#   arrange(floy_tag, doy)
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
#   arrange(floy_tag, doy)

# double check that predicts looks correct 
glimpse(predicts) 



write_rds(predicts, here("Saved Data",
                         "smr_gamma_predict.rds"))

# calculate daily mean temp by fish basin 
ful_temp %>%
  group_by(doy, 
           # fish_basin
           ) %>% 
  summarise(mean_smr = mean(mean_smr),
            mean_temp = mean(mean_temp)) %>% 
  ungroup() -> mean_smr

write_rds(mean_smr, here::here("model objects", 
                               "mean_smr.rds"))

# # create month labels 
# month_doy <- predicts %>% 
#   group_by(month_abb) %>% 
#   summarise(first = first(doy),
#             last = last(doy)) %>% 
#   ungroup() %>% 
#   # mutate(
#   #   month_abb = forcats::fct_relevel(month_abb, "Jan", 
#   #                                    "Feb", "Mar", "Apr", "May", "Jun",
#   #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   # ) %>% 
#   # arrange(month_abb) %>% 
#   # mutate(
#   #   first = if_else(
#   #     month_abb %in% "May", true = 123, false = first
#   #   )
#   # ) %>%   
#   .$first
# predicts %>% 
#   filter(doy %in% month_doy) %>%
#   group_by(month_abb) %>% 
#   summarise() %>% 
#   # mutate(
#   #   month_abb = forcats::fct_relevel(month_abb, "Jan", 
#   #                                    "Feb", "Mar", "Apr", "May", "Jun",
#   #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   # ) %>% 
#   # arrange(month_abb) %>% 
#   .$month_abb -> month_label 
# month_label
# # plotting prep -------
# 
# # figure out where your shading for summer and winter goes 
# predicts %>% 
#   group_by(season) %>% 
#   summarise(first = first(doy),
#             last = last(doy)) %>% 
#   ungroup()
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
# # ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
# ggplot(predicts) +
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
#   #                                   xmax = xmax,
#   #                                   ymin = ymin,
#   #                                   ymax = ymax),
#   #           fill ="grey80",
#   #           alpha = 0.75,
#   #           inherit.aes = FALSE) +
#   # geom_text(
#   #   aes(x = xmin + 30, y = 64, label = season),
#   #   data = rect_summer,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_text(
#   #   aes(x = xmin + 32, y = 64, label = season),
#   #   data = rect_winter,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   
#   geom_point(data = mean_smr, aes(x = doy, y = mean_smr,
#                                   # colour = fish_basin,
#   ), alpha = 0.5, size = 3) +
#   
#   geom_line(
#     aes(x = doy, y = fit, 
#         # colour = fish_basin
#         ), size = 1) +
#   geom_ribbon( 
#     aes(ymin = lower,
#         ymax = upper,
#         x = doy, y = fit,
#         # fill = fish_basin
#         ), alpha = 0.25) +
#   scale_y_continuous(breaks = seq(20, 60, 10)
#   ) +
#   scale_x_continuous(breaks = month_doy, 
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
#        y = expression(paste("Standard Metabolism (mg", 
#                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 
# 
# p
# write_rds(p, here("Plot Objects", 
#                   "daily_smr_GAMM_plot.rds"))
# 
# 
# ggsave(plot = p, filename = here("plots",
#                                  "Daily GAMM Plots",
#                                  "gamm_smr_doy.png"), width = 11,
#        height = 7 )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# cols <- rev(rainbow(6)[-6])
# 
# ggplot(predicts) +
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
#   #   aes(x = xmin + 25, y = 64, label = season),
#   #   data = rect_summer,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_text(
#   #   aes(x = xmin + 30, y = 64, label = season),
#   #   data = rect_winter,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   
#   geom_point(data = mean_smr, aes(x = doy, y = mean_smr,
#                                   fill = mean_temp,
#                                   # shape = fish_basin
#   ), size = 3, colour = "black", stroke = 0.25, shape = 21) +
#   geom_line(
#     aes(x = doy, y = fit, 
#         # linetype = fish_basin
#     ), colour = "black", 
#     linewidth = 1) +
#   geom_ribbon( 
#     aes(ymin = lower,
#         ymax = upper,
#         x = doy, y = fit, 
#         # group = fish_basin,
#     ), alpha = 0.10) +
#   
#   scale_y_continuous(breaks = seq(20, 60, 10)) +
#   scale_x_continuous(breaks = seq(25, 350, 65), 
#                      label = month_label) +
#   # scale_colour_viridis_c(name = "Temperature (°C)",
#   #                        option = "B", begin = 0.35, end = 0.75) +
#   scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
#                        name = "Temperature (°C)",
#                        breaks = seq(2, 10, 2), 
#                        guide = guide_colorbar(frame.colour = "black", 
#                                               ticks.colour = "black")
#   ) +
#   scale_shape_manual(name = "Basin", values = 21:23 
#   ) +
#   scale_linetype("Basin") + 
#   # scale_fill_viridis_c(name = ,
#   #                      option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   facet_wrap(.~ fish_basin,
#              # repeat.tick.labels = TRUE,
#              ncol = 1
#   ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         # strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         axis.title.y = element_text(hjust = 0.20),
#         legend.position = c(0.89, 0.90),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(
#     x = "", 
#     # x = "Date",
#     y = expression(paste("Standard Metabolism (mg", 
#                          O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2
# 
# # p2
# # ggsave(plot = p2, filename = here("plots",
# #                                   "Daily GAMM Plots",
# #                                   "gamm_smr_doy_temp.png"), width = 7,
# #        height = 11)
# 
# 
# ggplot(predicts) +
#   # geom_rect(data = rect_summer, aes(xmin = xmin,
#   #                                   xmax = xmax,
#   #                                   ymin = ymin,
#   #                                   ymax = ymax),
#   #           fill = "grey80",
#   #           alpha = 0.75,
#   #           inherit.aes = FALSE) +
#   # geom_rect(data = rect_winter, aes(xmin = xmin,
#   #                                   xmax = xmax,
#   #                                   ymin = ymin,
#   #                                   ymax = ymax),
# #           fill ="grey80",
# #           alpha = 0.75,
# #           inherit.aes = FALSE) +
# # geom_text(
# #   aes(x = xmin + 25, y = 64, label = season),
# #   data = rect_summer,
# #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
# # geom_text(
# #   aes(x = xmin + 30, y = 64, label = season),
# #   data = rect_winter,
# #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
# 
# # geom_point(data = mean_smr, aes(x = doy, y = mean_smr,
# #                                 fill = mean_temp,
# #                                 shape = fish_basin
# # ), size = 3, colour = "black", stroke = 0.25) +
# geom_line(
#   aes(x = doy, y = fit, 
#       linetype = fish_basin
#   ), colour = "black", 
#   linewidth = 1) +
#   geom_ribbon( 
#     aes(ymin = lower,
#         ymax = upper,
#         x = doy, y = fit, 
#         group = fish_basin,
#     ), alpha = 0.10) +
#   
#   scale_y_continuous(breaks = seq(20, 60, 10)) +
#   scale_x_continuous(breaks = seq(25, 350, 65), 
#                      label = month_label) +
#   # scale_colour_viridis_c(name = "Temperature (°C)",
#   #                        option = "B", begin = 0.35, end = 0.75) +
#   scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
#                        name = "Temperature (°C)",
#                        breaks = seq(2, 10, 2), 
#                        guide = guide_colorbar(frame.colour = "black", 
#                                               ticks.colour = "black")
#   ) +
#   scale_shape_manual(name = "Basin", values = 21:23 
#   ) +
#   scale_linetype("Basin") + 
#   # scale_fill_viridis_c(name = ,
#   #                      option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   # facet_wrap(.~ fish_basin,
#   # repeat.tick.labels = TRUE,
#   #            ncol = 2
#   # ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         legend.position = c(0.85, 0.87),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(x = "Date",
#        y = ""
#        # y = expression(paste("Standard Metabolism (mg", 
#        # O[2]," ", kg^-1, " ", h^-1, ")"))
#   ) -> p3
# 
# 
# 
# 
# p4 <- p2 / p3 + 
#   plot_layout(heights = c(3, 1))
# 
# # library(gtable)
# # pg <- ggplotGrob(p2)
# # qg <- ggplotGrob(p3)
# # 
# # pl <- gtable_filter(pg, 'panel', trim=F)$layout
# # pg <- gtable_add_grob(pg, qg, t=max(pl$t), l=max(pl$l))
# # 
# # grid.newpage()
# # grid.draw(pg)
# 
# 
# write_rds(p2, here("Plot Objects", 
#                    "daily_smr_GAMM_plot_temp.rds"))
# 
# 
# ggsave(plot = p4, filename = here("plots",
#                                   "Daily GAMM Plots",
#                                   "gamm_smr_doy_temp_a.png"), width = 7,
#        height = 12)
# 
# 
# 
# 
# 
# p5 <-  ggplot(predicts) +
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
#   geom_text(
#     aes(x = xmin + 30, y = 64, label = season),
#     data = rect_summer,
#     size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   geom_text(
#     aes(x = xmin + 32, y = 64, label = season),
#     data = rect_winter,
#     size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_text(
#   #   aes(x = xmin + 25, y = 64, label = season),
#   #   data = rect_summer,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   # geom_text(
#   #   aes(x = xmin + 30, y = 64, label = season),
#   #   data = rect_winter,
#   #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
#   
#   geom_point(data = mean_smr, aes(x = doy, y = mean_smr,
#                                   fill = mean_temp,
#                                   # shape = fish_basin
#   ), size = 3, colour = "black", stroke = 0.25, shape = 21) +
#   geom_line(
#     aes(x = doy, y = fit, 
#         linetype = fish_basin
#     ), colour = "black", 
#     linewidth = 1) +
#   geom_ribbon( 
#     aes(ymin = lower,
#         ymax = upper,
#         x = doy, y = fit, 
#         group = fish_basin,
#     ), alpha = 0.10) +
#   
#   scale_y_continuous(breaks = seq(20, 60, 10)) +
#   scale_x_continuous(breaks = seq(25, 350, 65), 
#                      label = month_label) +
#   # scale_colour_viridis_c(name = "Temperature (°C)",
#   #                        option = "B", begin = 0.35, end = 0.75) +
#   scale_fill_gradientn(colours = alpha(cols, f = 0.35), 
#                        name = "Temperature (°C)",
#                        breaks = seq(2, 10, 2), 
#                        guide = guide_colorbar(frame.colour = "black", 
#                                               ticks.colour = "black")
#   ) +
#   scale_shape_manual(name = "Basin", values = 21:23 
#   ) +
#   scale_linetype("Basin") + 
#   # scale_fill_viridis_c(name = ,
#   #                      option = "B", begin = 0.35, end = 0.75) +
#   # scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
#   
#   # facet_wrap(.~ fish_basin,
#   # repeat.tick.labels = TRUE,
#   # ncol = 1
#   # ) +
#   theme_classic(base_size = 15) +
#   theme(panel.grid = element_blank(),
#         # strip.text = element_blank(),
#         axis.text = element_text(colour = "black"),
#         # axis.title.y = element_text(hjust = 0.20),
#         legend.position = c(0.89, 0.75),
#         legend.background = element_blank(),
#         legend.title = element_text(hjust = 0.5),
#         legend.text = element_text(hjust = 0.5)) +
#   labs(
#     x = "", 
#     # x = "Date",
#     y = expression(paste("Standard Metabolism (mg", 
#                          O[2]," ", kg^-1, " ", h^-1, ")"))) 
# 
# ggsave(plot = p5, filename = here("plots",
#                                   "Daily GAMM Plots",
#                                   "gamm_smr_doy_temp_mike.png"), width = 11,
#        height = 7)
# 
# 
# # ------------ plot each fish's profile facted --------
# dat_3 <- ful_temp 
# 
# glimpse(dat_3)
# 
# # use prediction to get interpolated points 
# fits_id <- predict.bam(m, newdata = dat_3,  
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
# # ---- plot 2017 - 2020 gamm with mean daily temp per ID ------
# ggplot() +
#   geom_point(data = ful_temp, aes(x = doy, y = mean_smr,
#                                   colour = fish_basin,
#   ), alpha = 0.25, size = 2) +
#   geom_line(data = pred_id, 
#             aes(x = doy, y = fit, colour = fish_basin), size = 1) +
#   geom_ribbon(data = pred_id, 
#               aes(ymin = lower,
#                   ymax = upper,
#                   x = doy, y = fit,
#                   fill = fish_basin), alpha = 0.5) +
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
#   labs(
#     x = "Date",
#     y = expression(paste("Standard Metabolism (mg", 
#                          O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1
# 
# p1
# 
# ggsave(plot = p1, filename = here("plots",
#                                   "Individual Plots",
#                                   "gamm_smr_2017_2021_id.png"),
#        width = 11 * 2,
#        height = 7 * 2.15)
# 
