

# load packages ----

library(broom.mixed)
library(dplyr)
library(fitdistrplus)
library(ggplot2)
# library(gamm4)
library(gratia)
library(here)
library(itsadug)
library(lubridate)
# library(lme4)
library(mgcv)
library(multcomp)
library(openxlsx)
library(purrr)
library(readr)
library(tibble)

# bring temp data for model selection ------

ful_mmr <- read_rds(here("Saved Data", 
                         "Daily_MMR.rds")) 



glimpse(ful_mmr)


ful_mmr <-  ful_mmr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_mmr)



# run models ------




m <- bam(mean_mmr ~ fish_basin  + 
           s(doy, by = fish_basin, bs = "cc", k = 17) +
           s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             k = c(20, 4)), method = "fREML",
         family = Gamma(link = "identity"),
         data = ful_mmr, 
         select = TRUE
)


m1 <- update(m, . ~ 
               fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 17)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             # k = c(20, 4))
)


m2 <- update(m, . ~ 
               fish_basin  
             # s(doy, by = fish_basin, bs = "cc", k = 17) +
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             #   k = c(20, 4))
)

m3 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 17) +
               s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
                 k = c(20, 4))
)

m4 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 17)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             # k = c(20, 4))
)

m5 <- update(m, . ~ 
               # fish_basin  + 
               # s(doy, by = fish_basin, bs = "cc", k = 17) +
               s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
                 k = c(20, 4))
)

m6 <- update(m, . ~ 
               # fish_basin  + 
               s(doy, bs = "cc", k = 17)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             #   k = c(20, 4)) + 
)

m7 <- update(m, . ~ 
               fish_basin  + 
               s(doy, bs = "cc", k = 17) +
               s(floy_tag, year, bs = c("re", "re"), 
                 k = c(20, 4))
)


m8 <- update(m, . ~ 
               fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 17) + 
               s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
                 k = c(20, 4)) +
               ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
             
)

m9 <- update(m, . ~ 
               fish_basin  + 
               s(doy, by = fish_basin, bs = "cc", k = 17) + 
               # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
               #   k = c(20, 4)) +
               ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
             
)

m10 <- update(m, . ~ 
                fish_basin  + 
                # s(doy, by = fish_basin, bs = "cc", k = 17) + 
                # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
                #   k = c(20, 4)) +
                ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
              
)

m11 <- update(m, . ~ 
                # fish_basin  + 
                s(doy, by = fish_basin, bs = "cc", k = 17) +
                # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
                #   k = c(20, 4)) +
                ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
              
)
m12 <- update(m, . ~ 
                # fish_basin  + 
                s(doy, 
                  # by = fish_basin, 
                  bs = "cc", k = 17) +
                # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
                #   k = c(20, 4)) +
                ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
              
)
m13 <- update(m, . ~ 
                # fish_basin  + 
                s(doy, 
                  # by = fish_basin, 
                  bs = "cc", k = 17) 
              # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
              #   k = c(20, 4)) +
              # ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
              
)









# m and m18 are the same m19 invlovles an autocorelation structure 
m18 <- bam(mean_mmr ~ fish_basin  + 
             s(doy, by = fish_basin, bs = "cc", k = 17) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)),
           method = "fREML",
           family = Gamma(link = "log"),
           data = ful_mmr, 
           select = TRUE
)

acf(resid_gam(m18))

r1 <- itsadug::start_value_rho(m18, plot = TRUE, lag = 11)
r1




m19 <- bam(mean_mmr ~ fish_basin  + 
             s(doy, by = fish_basin, bs = "cc", k = 17) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)), method = "fREML",
           family = Gamma(link = "identity"),
           data = ful_mmr, 
           select = TRUE, 
           discrete = TRUE,
           rho = r1, 
           AR.start = ful_mmr$start_event
           
)


m20 <- bam(mean_mmr ~ fish_basin  + 
             s(doy, by = fish_basin, bs = "cc", k = 17) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)) +
             ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3)),
           method = "fREML",
           family = Gamma(link = "identity"),
           data = ful_mmr, 
           select = TRUE, 
           discrete = TRUE,
           rho = r1, 
           AR.start = ful_mmr$start_event
           
)

m21 <- bam(mean_mmr ~ 
             s(doy, by = fish_basin, bs = "cc", k = 17) +
             s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
               k = c(20, 4)), 
           method = "fREML",
           family = Gamma(link = "identity"),
           data = ful_mmr, 
           select = TRUE, 
           discrete = TRUE,
           rho = r1, 
           AR.start = ful_mmr$start_event
           
)


acf(resid_gam(m19))
acf(resid_gam(m20))


# anova(m18, m19, test = "F")
AIC(m18)
AIC(m19)


# create model list for model selection ------
model_list <- list(m, m1, m2, 
                   m3, m4, m5, m6, m7,
                   m8, m9, m10, m11, m12, m13, m19, m20, m21
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2",
                       "m3", "m4", "m5", "m6", "m7",
                       "m8", "m9", "m10", "m11", "m12", "m13",
                       "m19", "m20", "m21"
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
  mutate(model = case_when(id %in% c("m20", "m19", "m21") ~ paste(model, 
                                                                  "ACF", 
                                                                  sep = " + "),
                           TRUE ~ model), 
         delta_AIC = AIC - first(AIC), 
         AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>% 
  dplyr::select(model:AIC, delta_AIC, AIC_weight, BIC:df.residual)


glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "MMR results",
                                  "gamm_MMR_model_selection.xlsx"))
# # pridicted model --------


# ------------- plot -------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_mmr %>% 
  mutate(
    floy_tag = "a",
    year = 0, 
  )

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m20, newdata = dat_2, 
                    type = "response", se = TRUE, discrete = FALSE,
                    exclude = c("s(floy_tag, year)"),
                    newdata.guaranteed = TRUE)



# combine fits with dataframe for plotting and calc upper and lower 
# add in month abb for plotting 
predicts <- data.frame(dat_2, fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit, 
         month_abb = month(date, label = TRUE, abbr = TRUE), 
         month_abb = factor(month_abb, 
                            levels = c("Jan",
                                       "Feb", "Mar", "Apr",
                                       "May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec"))) %>% 
  arrange(floy_tag, doy)

# double check that predicts looks correct 
glimpse(predicts) 

# calculate daily mean temp by fish basin 
ful_mmr %>%
  group_by(doy, fish_basin) %>% 
  summarise(mean_mmr = mean(mean_mmr)) %>% 
  ungroup() -> mmr_mean

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
  #   aes(x = xmin + 25, y = 125, label = season),
  #   data = rect_summer,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  # geom_text(
  #   aes(x = xmin + 30, y = 125, label = season),
  #   data = rect_winter,
  #   size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = mmr_mean, aes(x = doy, y = mean_mmr,
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
  labs(x = "Date",
       y =  expression(paste("Maximum Metabolism (mg", 
                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "gamm_MMR_doy_ACF.png"), width = 11,
       height = 7 )
