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
         # doy_id = days(date), 
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
  filter(doy %in% seq(25, 350, 65)) %>%
  group_by(month_abb) %>%
  summarise() %>%
  .$month_abb -> month_labels


# glimpse(fs)
month_labels
# look at distibution ------
ggplot(data = fs, aes(x = fs)) + 
  geom_histogram() 

fs_scope <- fs %>% 
  filter(fs != is.na(fs)) %>% 
  .$fs

descdist(fs_scope)


fit_gamma <- fitdist(fs_scope, distr = "gamma", method = "mle")
plot(fit_gamma)

fs <- fs %>% 
  arrange(year, doy) %>% 
  group_by(year) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(year, doy) %>% 
  mutate(
    year = as.factor(year)
  )

fs
# run models ------




m <- bam(fs ~ fish_basin  + 
           s(doy, by = fish_basin, bs = "cc", k = 15) +
           s(year, by = fish_basin, bs = c("re"), 
             k = c(2)
           ), 
         method = "fREML",
         family = gaussian(link = "inverse"),
         data = fs, 
         select = TRUE
)


m1 <- update(m, . ~ 
               fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 15)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             # k = c(20, 4))
)


m2 <- update(m, . ~ 
               fish_basin  
             # s(doy, by = fish_basin, bs = "cc", k = 15) +
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             #   k = c(20, 4))
)

m3 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 15) +
               s(year, by = fish_basin, bs = c("re"), 
                 k = c(2)
               )
)

m4 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 15)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             # k = c(20, 4))
)

m5 <- update(m, . ~ 
               # fish_basin  + 
               # s(doy, by = fish_basin, bs = "cc", k = 15) +
               s(year, by = fish_basin, bs = c("re"), 
                 k = c(2)
               )
)

m6 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, bs = "cc", k = 15)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             #   k = c(20, 4)) + 
)

m7 <- update(m, . ~ 
               fish_basin  + 
               s(doy, bs = "cc", k = 15) +
               s(year, by = fish_basin, bs = c("re"), 
                 k = c(2)
               )
)
m8 <- update(m, . ~ 
               fish_basin  + 
               s(doy, bs = "cc", k = 15) +
               s(year, 
                 # by = fish_basin, 
                 bs = c("re"), 
                 k = c(2)
               )
)
m9 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, bs = "cc", k = 15) +
               s(year, 
                 # by = fish_basin, 
                 bs = c("re"), 
                 k = c(2)
               )
)










# m and m18 are the same m19 invlovles an autocorelation structure 
m18 <- bam(fs ~ fish_basin  + 
             s(doy, by = fish_basin, bs = "cc", k = 15) +
             s(year, by = fish_basin, bs = c("re"), 
               k = c(2)
             ),
           method = "fREML",
           family = gaussian(link = "inverse"),
           data = fs, 
           select = TRUE
)

acf(resid_gam(m18))

r1 <- itsadug::start_value_rho(m18, plot = TRUE, lag = 4)
r1




m19 <- update(m18, 
              discrete = TRUE,
              rho = r1, 
              AR.start = start_event
              
)





# anova(m18, m19, test = "F")
AIC(m18)
AIC(m19)


# create model list for model selection ------
model_list <- list(m, m1, m2, 
                   m3, m4, m5, m6, m7,
                   m8, m9, m19
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2",
                       "m3", "m4", "m5", "m6", "m7",
                       "m8", "m9",
                       "m19"
)
glance(m)

# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)

glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>% 
  mutate(model = lapply(model_list, formula) %>%
           as.character() 
  ) %>% 
  dplyr::select(model, id:df.residual) %>% 
  arrange(AIC)


glance_summary

glance_summary <- glance_summary %>% 
  mutate(model = case_when(id %in% c("m19") ~ paste(model, 
                                                    "ACF", 
                                                    sep = " + "),
                           TRUE ~ model), 
         delta_AIC = AIC - first(AIC), 
         AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>% 
  dplyr::select(model:AIC, delta_AIC, AIC_weight, BIC:df.residual)


glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "SOA results",
                                  "gamm_SOA_model_selection.xlsx"))
# # pridicted model --------


# ------------- plot -------

# create new fsafreame with dummmy variables for RE for plotting 
fs_2 <- fs %>% 
  mutate(
    year = 0, 
  )

glimpse(fs_2)

# use prediction to get interpolated points 
fits <- predict.bam(m19, newdata = fs_2, se = TRUE, discrete = FALSE,
                    exclude = c("s(year)"),
                    newdata.guaranteed = TRUE)



# combine fits with fsaframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(fs_2, fits) %>% 
  mutate(lower = 1 / (fit - 1.96 * se.fit),
         upper = 1 / (fit + 1.96 * se.fit), 
         fit = 1 / fit, 
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("Jan",
                                       "Feb", "Mar", "Apr",
                                       "May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec")))

# double check that predicts looks correct 
glimpse(predicts) 

# calculate daily mean temp by fish basin 
fs %>%
  group_by(doy, fish_basin) %>% 
  summarise(fs = mean(fs)) %>% 
  ungroup() -> fs_mean

# # create month labels 
# predicts %>% 
#   filter(doy %in% seq(09, 344, 67)) %>%
#   group_by(month_abb) %>% 
#   summarise() %>% 
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
# rect_summer <- tibble(
#   season = "Summer",
#   xmin = 10,
#   xmax = 101,
#   ymin = -Inf,
#   ymax = Inf
# )
# 
# rect_winter <- tibble(
#   season = "Winter",
#   xmin = 193,
#   xmax = 283,
#   ymin = -Inf,
#   ymax = Inf
# )

# ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
ggplot(predicts) +
  # geom_rect(fsa = rect_summer, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
  #           fill = "grey80",
  #           alpha = 0.75,
  #           inherit.aes = FALSE) +
  # geom_rect(fsa = rect_winter, aes(xmin = xmin,
  #                                   xmax = xmax,
  #                                   ymin = ymin,
  #                                   ymax = ymax),
#           fill ="grey80",
#           alpha = 0.75,
#           inherit.aes = FALSE) +
# geom_text(
#   aes(x = xmin + 25, y = 125, label = season),
#   fsa = rect_summer,
#   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
# geom_text(
#   aes(x = xmin + 30, y = 125, label = season),
#   fsa = rect_winter,
#   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
geom_point(data = fs_mean, aes(x = doy, y = fs,
                              colour = fish_basin,
), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy, y = fit,
        fill = fish_basin), alpha = 0.25) +
  # scale_y_continuous(breaks = seq(45, 135, 15)) +
  # scale_x_continuous(breaks = seq(10, 350, 65), 
  #                    label = month_label
  # ) +
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
  labs(x = "date",
       y =  expression(paste("Aerobic Scope  (mg", 
                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "gamm_SOA_doy_id_ACF.png"), width = 11,
       height = 7 )
