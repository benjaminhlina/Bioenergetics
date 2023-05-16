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
  source(here("R",
              "Cleaning and Calculations",
              "julian_date_reorder.r"))
}

smr <- read_rds(here("Saved Data", 
                     "Daily_SMR.rds"))  

smr <-  smr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)



glimpse(smr)
# possibly add temp used to then model what temps fish may see 
# -----------------------START GAMMS -------------------------------
m <- bam(mean_smr ~ fish_basin +  
           s(doy_id, by = fish_basin, bs = "cc", k = 15) +
           s(mean_temp, by = fish_basin, bs = "cr", k = 12) +
           s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             k = c(20, 4)) +
           ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(15, 3)),
         method = "fREML",
         family = Gamma(link = "log"),
         data = smr, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 16)
r1


m1 <- update(m, discrete = TRUE, 
             rho = r1, 
             AR.start = smr$start_event)


# check model fit -----
par(mfrow = c(2, 2))
gam.check(m)
gam.check(m1)

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


dat_2 <- smr %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  )

glimpse(dat_2)



# use prediction to get interpolated points 
fits <- predict.bam(m1, newdata = dat_2, discrete = FALSE,
                    type = "response", se = TRUE, 
                    exclude = c("s(floy_tag, year)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(
    lower = fit - 1.96 * se.fit,
    upper = fit + 1.96 * se.fit, 
    month_abb = month(date, label = TRUE, abbr = TRUE), 
    month_abb = factor(month_abb, 
                       levels = c("May", "Jun", "Jul", 
                                  "Aug", "Sep", "Oct",
                                  "Nov", "Dec", "Jan",
                                  "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)


predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 

ggplot(predicts) +
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
  #   aes(x = xmin + 30, y = 64, label = season),
  #   data = rect_summer,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_text(
  #   aes(x = xmin + 32, y = 64, label = season),
  #   data = rect_winter,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # 
  # geom_point(data = mean_smr, aes(x = doy_id, y = mean_smr,
  #                                 colour = fish_basin,
  # ), alpha = 0.5, size = 3) +
  # 
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
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
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
       y = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
