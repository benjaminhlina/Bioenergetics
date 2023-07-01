{
  library(dplyr)
  library(here)
  library(ggplot2)
  library(lubridate)
  library(readr)
  library(tidyr)
}


# ----- bring in smr and temp data ------

temp <- read_rds(here(
  "Saved Data", 
  "Daily_temp.rds"
))

smr <- read_rds(here(
  "Saved Data", 
  "Daily_smr.rds"
))

rmr <- read_rds(here(
  "Saved Data", 
  "Daily_rmr.rds"
))



glimpse(temp)
glimpse(smr)
glimpse(rmr)





# ----- create daily summarries of temp, smr, and rmr ------
smr_summary <- smr %>% 
  group_by(doy_id, fish_basin) %>% 
  summarise(mean_smrs = mean(mean_smr),
            sem_smr = sd(mean_smr) / sqrt(n()), 
            mean_temps = mean(mean_temp), 
            sem_temp = sd(mean_temp) / sqrt(n()), 
  ) %>% 
  ungroup()

rmr_summary <- rmr %>% 
  group_by(doy_id, fish_basin) %>% 
  summarise(mean_rmrs = mean(mean_rmr),
            sem_rmr = sd(mean_rmr) / sqrt(n()), ) %>% 
  ungroup()




met_summary <- smr_summary %>% 
  left_join(rmr_summary, by = c("doy_id", "fish_basin"))


glimpse(met_summary)
glimpse(smr)

# ---- determine mean weight ------
wt_mean <- bind_rows(smr %>% 
                       group_by(fish_basin) %>% 
                       summarise(
                         wt = mean(weight)
                       ) %>% 
                       ungroup(),
                     temp %>% 
                       group_by(fish_basin) %>% 
                       summarise(
                         wt = mean(weight)
                       ) %>% 
                       ungroup(),
                     # rmr %>% 
                     # group_by(fish_basin) %>% 
                     # summarise(
                     #   wt = mean(weight)
                     # ) %>% 
                     # ungroup()
) %>% 
  group_by(fish_basin) %>% 
  summarise(
    wgt = mean(wt)
  ) %>% 
  ungroup()


wt_mean

wt <- smr %>% 
  group_by(floy_tag, fish_basin, weight) %>% 
  summarise(
  ) %>% 
  ungroup()
wt
smr %>% 
  group_by(fish_basin) %>% 
  summarise(
    n = n_distinct(floy_tag),
    median_wt = median(weight)
  )


ggplot(data = wt, aes(x = weight)) + 
  geom_histogram()

car::leveneTest(weight ~ fish_basin, data = wt)
moments::kurtosis(wt$weight)
moments::skewness(wt$weight)
fitdistrplus::descdist(wt$weight)
lm_fit <- fitdistrplus::fitdist(wt$weight, distr = "norm")
plot(lm_fit)
shapiro.test(wt$weight)
summary(quantreg::rq(weight ~ fish_basin, data = wt), 
        se = "boot")

coin::median_test(weight ~ fish_basin, data = wt, mid.score = "1")
RVAideMemoire::mood.medtest(weight ~ fish_basin, data = wt)

m <- aov(weight ~ fish_basin, data = wt)
car::Anova(m)

kruskal.test(weight ~ fish_basin, data = wt)



# ---- create table of smr and rmr formulas ------
met_form <- crossing(
  tibble(
    temp = seq(0, 20, 0.5)
  ),
  wt_mean, 
) %>%
  mutate(
    m_std_stew = 46.072 * (exp (1) ^ (0.0607 * temp)),
    m_std_gbf = 17.099 * (exp (1) ^ (0.0950 * temp)),
    m_act_stew = 127.18 * (exp(1) ^ (0.0785 * temp)),
    m_act_gbf = 105.31 * (exp(1) ^ (0.0847 * temp)),
    m_std_cf = 46.072 * (exp (1) ^ (0.0607 * temp)) * (0.1) *
      (((wgt) / 0.1) ^ 0.85)
    
  )



glimpse(met_form)




# ----- create labels for months for plotting ------
#
smr %>%   
  mutate(month_abb = month(date, label = TRUE, abbr = TRUE)) %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>%
  arrange(doy_id) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb %>% 
  as.character() %>% 
  
  factor(levels = c("May", "Jul", "Oct", 
                    "Dec", "Feb", "Apr")) %>% 
  sort() -> month_label 
month_label





# ----- Plot temp vs. day with colour being SMR and shape being basin -----
ggplot(data = met_summary, aes(x = doy_id, y = mean_temps)) + 
  
  geom_errorbar(aes(ymin = mean_temps - sem_temp, 
                    ymax = mean_temps + sem_temp, 
                    group = fish_basin), width = 0.1) + 
  geom_point(size = 3, aes(colour = mean_smrs, 
                           shape = fish_basin),
             alpha = 0.65) + 
  scale_colour_viridis_c(
    name = expression(paste("Standard Metabolism (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")),
    option = "B", begin = 0.25, end = 0.75) + 
  scale_shape(name = "Basin") + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.85, 0.85),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label
  ) +
  labs(
    x = "Date", 
    y = "Daily Temperature (°C)"
  )


# ----- plotting temp and SMR as biplot with theroitcal example ------

ms_cf <- met_form %>%
  dplyr::select(temp, fish_basin, wgt, m_std_cf)

glimpse(smr)
ggplot() + 
  geom_point(data = smr, size = 2, 
             aes(x = mean_temp, 
                 y = mean_smr, 
                 colour = fish_basin),
             alpha = 0.08) + 
  geom_line(data = ms_cf, 
            linewidth = 1.5, aes(x = temp, y = m_std_cf, 
                               colour = fish_basin)) +
  scale_colour_viridis_d(
    name = "Basin",
    option = "B", begin = 0.25, end = 0.75) + 
  theme_bw(base_size = 15) +
  theme(
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.08, 0.9),
        # legend.background = element_blank(),
        legend.text = element_text(hjust = 0.5)) +
  # scale_x_continuous(breaks = seq(25, 350, 65), 
  #                    label = month_label) +
  labs(
    x = "Daily Temperature (°C)", 
    
    y = expression(paste("Standard Metabolism (mg", 
                         O[2]," ", kg^-1, " ", h^-1, ")"))
  ) -> p1 


p1


ggsave(p1, filename = here("Plots",
                           "Tag calibration plots",
                           "smr_temp_ID_curve.png"),
       height = 7, width = 11)


ggplot() + 
  geom_point(data = smr, size = 2, 
             aes(x = mean_temp, 
                 y = mean_smr, 
                 colour = fish_basin),
             alpha = 0.08) + 
  # geom_line(data = ms_cf, 
  #           linewidth = 1.5, aes(x = temp, y = m_std_cf, 
  #                                colour = fish_basin)) +
  scale_colour_viridis_d(
    name = "Basin",
    option = "B", begin = 0.25, end = 0.75) + 
  theme_bw(base_size = 15) +
  theme(
    legend.title = element_text(hjust = 0.5),
    axis.text = element_text(colour = "black"),
    legend.position = c(0.08, 0.9),
    # legend.background = element_blank(),
    legend.text = element_text(hjust = 0.5)) +
  # scale_x_continuous(breaks = seq(25, 350, 65), 
  #                    label = month_label) +
  labs(
    x = "Daily Temperature (°C)", 
    
    y = expression(paste("Standard Metabolism (mg", 
                         O[2]," ", kg^-1, " ", h^-1, ")"))
  ) -> p2 


ggsave(p2, filename = here("Plots",
                           "Tag calibration plots",
                           "smr_temp_ID.png"),
       height = 7, width = 11)

ggplot() + 
  # geom_point(data = smr, size = 2, 
  #            aes(x = mean_temp, 
  #                y = mean_smr, 
  #                colour = fish_basin),
  #            alpha = 0.08) + 
  geom_line(data = ms_cf,
            linewidth = 1, aes(x = temp, y = m_std_cf,
                                 colour = fish_basin)) +
  scale_colour_viridis_d(
    name = "Basin",
    option = "B", begin = 0.25, end = 0.75) + 
  theme_bw(base_size = 15) +
  theme(
    legend.title = element_text(hjust = 0.5),
    axis.text = element_text(colour = "black"),
    legend.position = c(0.08, 0.9),
    # legend.background = element_blank(),
    legend.text = element_text(hjust = 0.5)) +
  # scale_x_continuous(breaks = seq(25, 350, 65), 
  #                    label = month_label) +
  labs(
    x = "Daily Temperature (°C)", 
    
    y = expression(paste("Standard Metabolism (mg", 
                         O[2]," ", kg^-1, " ", h^-1, ")"))
  ) -> p3


ggsave(p3, filename = here("Plots",
                           "Tag calibration plots",
                           "smr_temp_curve_basin.png"),
       height = 7, width = 11)





# ----- plotting temp vs. rmr ------- 
ggplot(data = met_summary, aes(x = mean_temps, y = mean_rmrs)) + 
  geom_point(size = 3, aes(colour = fish_basin),
             alpha = 0.65) + 
  scale_colour_viridis_d(
    name = "Basin",
    option = "B", begin = 0.25, end = 0.75) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.15, 0.85),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  # scale_x_continuous(breaks = seq(25, 350, 65), 
  #                    label = month_label) +
  labs(
    x = "Daily Temperature (°C)", 
    y = expression(paste("Active Metabolism (mg", 
                         O[2]," ", kg^-1, " ", h^-1, ")")))

  
  
  