

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

ful_smr <- read_rds(here("Saved Data", 
                         "Daily_SMR.rds"))



glimpse(ful_smr)


ful_smr <-  ful_smr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE), 
         year = factor(year)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_smr)



# run models ------




m <- bam(mean_smr ~ 
           s(doy, bs = "cc", k = 18) +
           s(floy_tag,  bs = c("re"), 
             k = c(20)) + 
           s(year, bs = c("re"), 
             k = c(4)),
         family = Gamma(link = "inverse"),
         data = ful_smr, 
         select = TRUE
)


m1 <- update(m, . ~ 
               s(doy, bs = "cc", k = 18) +
               s(floy_tag,  bs = c("re"), 
                 k = c(20)) 
             # s(year, bs = c("re"), 
             #   k = c(4)),
)


m2 <- update(m, . ~ 
               s(doy, bs = "cc", k = 18) +
               # s(floy_tag,  bs = c("re"), 
               #   k = c(20)) + 
               s(year, bs = c("re"), 
                 k = c(4)),
)

m3 <- update(m, . ~ 
               # fish_basin  + 
               # s(doy, bs = "cc", k = 17) +
               s(floy_tag,  bs = c("re"), 
                 k = c(20)) + 
               s(year, bs = c("re"), 
                 k = c(4))
)
m4 <- update(m, . ~ 
               # fish_basin  + 
               # s(doy, bs = "cc", k = 17) +
               # s(floy_tag,  bs = c("re"), 
               #   k = c(20)) + 
               s(year, bs = c("re"), 
                 k = c(4))
)
m5 <- update(m, . ~ 
               # fish_basin  + 
               # s(doy, bs = "cc", k = 17) +
               s(floy_tag,  bs = c("re"),
                 k = c(20)) 
             # s(year, bs = c("re"), 
             #   k = c(4))
)




# m and m18 are the same m19 invlovles an autocorelation structure 
# m18 <- bam(mean_mmr ~ fish_basin  + 
#              s(doy, bs = "cc", k = 17) +
#              s(floy_tag,  bs = c("re"), 
#                k = c(20)) + 
#              s(year, bs = c("re"), 
#                k = c(4)),
#            method = "fREML",
#            family = Gamma(link = "inverse"),
#            data = ful_mmr, 
#            select = TRUE
# )

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 17)
r1




m6 <- update(m, 
             discrete = TRUE,
             rho = r1, 
             AR.start = start_event
             
)

# 
# m20 <- bam(mean_mmr ~ fish_basin  + 
#              s(doy, bs = "cc", k = 17) +
#              s(floy_tag,  bs = c("re"), 
#                k = c(20)) + 
#              s(year, bs = c("re"), 
#                k = c(4)),
#              ti(doy, fish_basin, bs = c("cc", "fs"), k = c(20, 3)),
#            method = "fREML",
#            family = Gamma(link = "inverse"),
#            data = ful_mmr, 
#            select = TRUE, 
#            discrete = TRUE,
#            rho = r1, 
#            AR.start = $start_event
#            
# )
# 
# m21 <- bam(mean_mmr ~ 
#              s(doy, bs = "cc", k = 17) +
#              s(floy_tag,  bs = c("re"), 
#                k = c(20)) + 
#              s(year, bs = c("re"), 
#                k = c(4)),
#            method = "fREML",
#            family = Gamma(link = "inverse"),
#            data = ful_mmr, 
#            select = TRUE, 
#            discrete = TRUE,
#            rho = r1, 
#            AR.start = start_event
#            
# )


# acf(resid_gam(m19))
# acf(resid_gam(m20))
# 
# 
# # anova(m18, m19, test = "F")
# AIC(m18)
# AIC(m19)


# create model list for model selection ------
model_list <- list(m, m1, m2, 
                   m3, m4, m5, m6
                   # m8, m9, m10, m11, m12, m13, m19, m20, m21
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2",
                       "m3", "m4", "m5", "m6"
                       # "m8", "m9", "m10", "m11", "m12", "m13", 
                       # "m19", "m20", "m21"
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
  mutate(model = case_when(id %in% c("m6") ~ paste(model, 
                                                  "ACF", 
                                                  sep = " + "),
                           TRUE ~ model), 
         delta_AIC = AIC - first(AIC), 
         AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>% 
  dplyr::select(model:AIC, delta_AIC, AIC_weight, BIC:df.residual)




glance_summary

glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "smr results",
                                  "gamm_smr_model_selection.xlsx"))

gam.check(m20)
# # pridicted model --------


# ------------- plot -------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- ful_smr %>% 
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
                            levels = c("May", "Jun", "Jul", 
                                       "Aug", "Sep", "Oct",
                                       "Nov", "Dec", "Jan",
                                       "Feb", "Mar", "Apr"))) %>% 
  arrange(floy_tag, doy_id)

# double check that predicts looks correct 
glimpse(predicts) 

# calculate daily mean temp by fish basin 
ful_smr %>%
  group_by(doy_id, fish_basin) %>% 
  summarise(mean_smr = mean(mean_smr)) %>% 
  ungroup() -> temp_mean

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

# ---------- plot doy gamm for 2017 - 2020 with mean daily temp ------
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
    aes(x = xmin + 25, y = 56, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 30, y = 56, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = temp_mean, aes(x = doy_id, y = mean_smr,
                                   colour = fish_basin,
  ), alpha = 0.5, size = 3) +
  
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_continuous(breaks = seq(20, 60, 10)
  ) +
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
ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots", 
                                 "gamm_smr_doy_ACF.png"), width = 11,
       height = 7 )

