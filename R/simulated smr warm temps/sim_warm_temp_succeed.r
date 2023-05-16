# load packages ----

library(broom)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggh4x)
library(gamm4)
library(gratia)
library(here)
library(itsadug)
library(lubridate)
library(janitor)
library(mgcv)
library(readr)
library(tibble)
library(tidymv)
library(tidyr)



temp <- read_rds(here("Saved Data", 
                      "daily_temp_range_measured.rds"))

temp_use_model <- read_rds(here("model objects", 
                                "temp_gamm_model.rds"))
temp_use_pred <- read_rds(here("model objects", 
                               "temp_gamm_predicts.rds"))


glimpse(temp)

sum_temp <- temp %>%
  filter(depth == 2) %>% 
  group_by(doy, basin) %>% 
  summarise(
    min_temps = mean(min_temp),
    max_temps = mean(max_temp)
  ) %>% 
  ungroup()



sim <- crossing(
  sum_temp,
  tibble(wt = seq(0.8, 1.200, 0.1)), 
  tibble(year = seq(2025, 2075, 10))
)


sim_long <- sim %>% 
  pivot_longer(cols = -c(doy,basin, wt, year), 
               names_to = "min_max", 
               values_to = "temp")

sim_long <- sim_long %>% 
  mutate(
    pred_temp = case_when(
      year == 2035 ~ temp + 0.34, 
      year == 2045 ~ temp + 0.34 * 2, 
      year == 2055 ~ temp + 0.34 * 3, 
      year == 2065 ~ temp + 0.34 * 4, 
      year == 2075 ~ temp + 0.34 * 5, 
      TRUE ~ temp
    )
  )
  # mutate(
  #   pred_temp = case_when(
  #     pred_temp > 18 ~ 18, 
  #     TRUE ~ pred_temp
  #   )
  # )


sim_long <- sim_long %>% 
  mutate(
    m_std_stew = 46.072 * (exp (1) ^ (0.0607 * pred_temp)),
    m_std_gbf = 17.099 * (exp (1) ^ (0.0950 * pred_temp)),
    m_act_stew = 127.18 * (exp(1) ^ (0.0785 * pred_temp)),
    m_act_gbf = 105.31 * (exp(1) ^ (0.0847 * pred_temp)),
    m_std_cf = 46.072 * (exp (1) ^ (0.0607 * pred_temp)) * (0.1) *
      (((wt) / 0.1) ^ 0.85)
    
  )





sim_long

sim_sum <- sim_long %>% 
  group_by(doy, basin, year, min_max, temp) %>% 
  summarise(
    mean_smr_cf = mean(m_std_cf), 
    sem_smr_cf = sd(m_std_cf) / sqrt(n())
  ) %>% 
  ungroup()



sim_sum




ggplot(data = sim_sum %>% 
         filter(min_max == "min_temps"), aes(x = doy, y = mean_smr_cf)) +
  geom_line(linewidth = 1, aes(colour = basin)) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.10, 0.90),
        legend.title = element_text(hjust = 0.5)
        # legend.background = element_blank()
  ) +
  facet_wrap(. ~ year) + 
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")")))







# 
# dt_style <- function(){
#   fake_dat_copy <- copy(sim)
#   
#   # Row sum of the two columns, removing NAs
#   setDT(fake_dat_copy)[, V3 := .SD + 0.5, .SDcols = c('min_temp', 
#                                                                     'max_temp')]
#   
#   # by = ...
#   #     Fill NAs by last observation carried forward, turn repeated values into groups
#   #     run-length id (?data.table::rleid)
#   # cumsum...
#   #     Run cumulative sums for each run-length group
#   fake_dat_copy[, V3 := cumsum(V3), by = rleid(nafill(V2, 'locf'))]
# }
# 
# #  pridicted model --------

glimpse(temp_use_pred)

sim_temp_use <- temp_use_pred %>% 
  select(doy_id, doy, season, fish_basin, fit:month_abb) %>% 
  as_tibble() %>% 
  group_by(doy_id, doy, season, fish_basin, month_abb) %>% 
  summarise(
    temp = mean(fit),
    low = mean(lower),
    high = mean(upper)
  ) %>% 
  ungroup()
sim_temp_use


sim_temp <-  crossing(
  sim_temp_use,
  tibble(wt = seq(0.8, 1.200, 0.1)), 
  tibble(year = seq(2025, 2075, 10))
)

sim_temp <- sim_temp %>% 
  mutate(
    pred_temp = case_when(
      year == 2035 ~ temp + 0.34, 
      year == 2045 ~ temp + 0.34 * 2, 
      year == 2055 ~ temp + 0.34 * 3, 
      year == 2065 ~ temp + 0.34 * 4, 
      year == 2075 ~ temp + 0.34 * 5, 
      TRUE ~ temp
    ), 
    pred_temp_low = case_when(
      year == 2035 ~ low + 0.34, 
      year == 2045 ~ low + 0.34 * 2, 
      year == 2055 ~ low + 0.34 * 3, 
      year == 2065 ~ low + 0.34 * 4, 
      year == 2075 ~ low + 0.34 * 5, 
      TRUE ~ temp
    ),
    pred_temp_high = case_when(
      year == 2035 ~ high + 0.34, 
      year == 2045 ~ high + 0.34 * 2, 
      year == 2055 ~ high + 0.34 * 3, 
      year == 2065 ~ high + 0.34 * 4, 
      year == 2075 ~ high + 0.34 * 5, 
      TRUE ~ temp
    )
  ) 


sim_temp <- sim_temp %>% 
  mutate(
    m_std_stew = 46.072 * (exp (1) ^ (0.0607 * pred_temp)),
    m_std_gbf = 17.099 * (exp (1) ^ (0.0950 * pred_temp)),
    m_act_stew = 127.18 * (exp(1) ^ (0.0785 * pred_temp)),
    m_act_gbf = 105.31 * (exp(1) ^ (0.0847 * pred_temp)),
    m_std_cf = 46.072 * (exp (1) ^ (0.0607 * pred_temp)) * (0.1) *
      (((wt) / 0.1) ^ 0.85),
    m_std_cf_l = 46.072 * (exp (1) ^ (0.0607 * pred_temp_low)) * (0.1) *
      (((wt) / 0.1) ^ 0.85),
    m_std_cf_h = 46.072 * (exp (1) ^ (0.0607 * pred_temp_high)) * (0.1) *
      (((wt) / 0.1) ^ 0.85)
    
  )o


temp_use_pred %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(year, month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_label 
month_label

ggplot(data = sim_temp, aes(x = doy_id, y = m_std_cf)) +
  geom_line(linewidth = 1, aes(x = doy_id, y = m_std_cf, 
                               colour = fish_basin)) +
  geom_ribbon(
    aes(ymin = m_std_cf_l,
        ymax = m_std_cf_h,
        x = doy_id, y = m_std_cf, 
        # colour = "black", 
        fill = fish_basin), alpha = 0.5) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_fill_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  theme_bw(base_size = 25) +
  theme(legend.position = c(0.05, 0.96),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_blank(), 
        # panel.grid = element_blank()
  ) +
  facet_grid(wt ~ year) + 
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 


p


ggsave(here("Plots",
            "simulated smr climate change",
            "smr_year_wt_fact.png"),
       plot = p,
       height = 20,
       width = 25)


sim_wt <-  crossing(
  sim_temp_use,
  tibble(wt = 0.9), 
  tibble(year = seq(2025, 2075, 10))
)

sim_wt <- sim_wt %>% 
  mutate(
    pred_temp = case_when(
      year == 2035 ~ temp + 0.34, 
      year == 2045 ~ temp + 0.34 * 2, 
      year == 2055 ~ temp + 0.34 * 3, 
      year == 2065 ~ temp + 0.34 * 4, 
      year == 2075 ~ temp + 0.34 * 5, 
      TRUE ~ temp
    )
  ) 


sim_wt <- sim_wt %>% 
  mutate(
    m_std_stew = 46.072 * (exp (1) ^ (0.0607 * pred_temp)),
    m_std_gbf = 17.099 * (exp (1) ^ (0.0950 * pred_temp)),
    m_act_stew = 127.18 * (exp(1) ^ (0.0785 * pred_temp)),
    m_act_gbf = 105.31 * (exp(1) ^ (0.0847 * pred_temp)),
    m_std_cf = 46.072 * (exp (1) ^ (0.0607 * pred_temp)) * (0.1) *
      (((wt) / 0.1) ^ 0.85)
    
  )

ggplot(data = sim_wt, aes(x = doy_id, y = m_std_cf)) +
  geom_line(linewidth = 1, aes(colour = fish_basin)) +
  # geom_ribbon( 
  #   aes(ymin = low,
  #       ymax = high,
  #       x = doy_id, y = m_std_cf,
  #       fill = fish_basin), alpha = 0.25) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  theme_bw(base_size = 25) +
  theme(legend.position = c(0.05, 0.96),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_blank(), 
        # panel.grid = element_blank()
  ) +
  facet_wrap(. ~ year) + 
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1
p1

# ggsave(here("Plots", 
#             "simulated smr climate change", 
#             "smr_year_fact.png"), 
#        plot = p1, 
#        height = 10, 
#        width = 15)


cols <- rev(rainbow(7)[-7])
alpha()


sim_temp$year_f <- as.factor(sim_temp$year)


ggplot(data = sim_temp, aes(x = doy_id, y = m_std_cf)) +
  geom_line(linewidth = 1, aes(colour = year_f)) +
  # geom_ribbon( 
  #   aes(ymin = low,
  #       ymax = high,
  #       x = doy_id, y = m_std_cf,
  #       fill = fish_basin), alpha = 0.25) +
  scale_colour_viridis_d(name = "Year",
                         option = "B", begin = 0.35, end = 0.75) +
  # scale_colour_manual(values = scales::alpha(cols, alpha = 0.35),
  #                     name = "Year"
  # ) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  theme_bw(base_size = 15) +
  theme(
    # legend.position = c(0.23, 0.9),
    legend.title = element_text(hjust = 0.5),
    legend.background = element_blank(), 
    # panel.grid = element_blank()
  ) +
  facet_grid(wt ~ fish_basin) + 
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2 


p2
# ggsave(here("Plots", 
#             "simulated smr climate change", 
#             "smr_basin_smr_facet.png"), 
#        plot = p2, 
#        height = 10, 
#        width = 15)


sim_temp

sim_temp <-  sim_temp %>%
  group_by(year) %>%
  arrange(year, doy_id) %>%
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE,
                               false = FALSE)) %>%
  ungroup() %>%
  arrange(doy_id, start_event)
sim_temp$year <- as.factor(sim_temp$year)

fitdistrplus::descdist(sim_temp$m_std_cf)

glimpse(sim_temp)
m <- bam(m_std_cf ~ fish_basin * year * wt +
           s(doy_id, by = fish_basin, bs = "cc", k = 15) +
           # s(year, by = fish_basin, bs = c("re"), 
           #   k = c(6)) +
           ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(15, 3)),
         method = "fREML",
         family = gaussian(),
         data = sim_temp, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 16)
r1


m1 <- update(m, discrete = TRUE, 
             rho = r1, 
             AR.start = sim_temp$start_event)


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



# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- sim_temp %>% 
  mutate(
    # floy_tag = "a",
    # year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m1, newdata = dat_2, 
                    type = "response", se = TRUE, discrete = FALSE, 
                    # exclude = c("s(year)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit, 
         # month_abb = month(date, label = TRUE, abbr = TRUE), 
         # month_abb = factor(month_abb, 
         #                    levels = c("May", "Jun", "Jul", 
         #                               "Aug", "Sep", "Oct",
         #                               "Nov", "Dec", "Jan",
         #                               "Feb", "Mar", "Apr"))
  ) %>% 
  arrange(doy_id)

# double check that predicts looks correct 
glimpse(predicts) 
ggplot(predicts) +
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.5) +
  # scale_y_continuous(breaks = seq(0, 12, 2)) +
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
  facet_grid(wt ~ year) + 
  theme_bw(base_size = 25) +
  theme(
    # panel.grid = element_blank(),
    # strip.text = element_blank(),
    axis.text = element_text(colour = "black"),
    # legend.position = c(0.95, 0.96),
    legend.position = c(0.035, 0.96),
    legend.background = element_blank(),
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))
       # y = "Daily Temperature (°C)"
  ) -> p3

# p3
# ggsave(here("Plots", 
#             "simulated smr climate change", 
#             "smr_year_wt_fact_gam.png"), 
#        plot = p3, 
#        height = 20, 
#        width = 25)


glimpse(predicts)

ggplot(data = predicts, aes(x = doy_id, y = fit)) +
  geom_line(linewidth = 1, aes(colour = year)) +
  geom_ribbon(
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = se.fit,
        fill = year), alpha = 0.5) +
  scale_colour_viridis_d(name = "Year",
                         option = "B", begin = 0.25, end = 0.85) +
  scale_fill_viridis_d(name = "Year",
                         option = "B", begin = 0.25, end = 0.85) +
  # scale_colour_manual(values = alpha(cols, f = 0.35), 
  #                     name = "Year"
  #                     # name = "Temperature (°C)",
  #                     # breaks = seq(2, 10, 2),
  # ) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  theme_bw(base_size = 15) +
  theme(
    # legend.position = c(0.23, 0.9),
    legend.title = element_text(hjust = 0.5),
    legend.background = element_blank(), 
    # panel.grid = element_blank()
  ) +
  facet_grid(wt ~ fish_basin) + 
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p4 


p4
ggsave(here("Plots", 
            "simulated smr climate change", 
            "smr_basin_smr_facet__gamm_rainbow.png"), 
       plot = p4, 
       height = 10, 
       width = 15)
ggplot(data = predicts %>% 
         filter(year == "2075"), aes(x = doy_id, y = fit)) +
  geom_line(linewidth = 1, aes(colour = fish_basin, 
                               # linetype = fish_basin
                               )) +
  geom_ribbon(
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.5) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.25, end = 0.85) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.25, end = 0.85) +
  # scale_colour_manual(values = alpha(cols, f = 0.35), 
  #                     name = "Year"
  #                     # name = "Temperature (°C)",
  #                     # breaks = seq(2, 10, 2),
  # ) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = c(0.07, 0.95),
    legend.title = element_text(hjust = 0.5),
    legend.background = element_blank(), 
    # panel.grid = element_blank()
  ) +
  facet_grid(wt ~ .) + 
  labs(x = "Date",
       y = expression(paste("Standard Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p5
# p5
# ggsave(here("Plots", 
#             "simulated smr climate change", 
#             "smr_basin_2075_wt_facet.png"), 
#        plot = p5, 
#        height = 15, 
#        width = 9)
