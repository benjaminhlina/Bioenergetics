

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

soa <- read_rds(here("Saved Data", 
                          "Daily_SoA.rds"))

glimpse(soa)




# soa <-  soa %>% 
#   group_by(year) %>% 
#   arrange(year, doy_id) %>% 
#   mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
#                                false = FALSE)) %>% 
#   ungroup() %>% 
#   arrange(date, start_event)

glimpse(soa)

# run models ------




m <- bam(fs ~ fish_basin + 
           s(doy_id, by = fish_basin, bs = "cc", k = 20),
         method = "fREML",
         data = soa, 
         select = TRUE
)




m1 <- update(m, . ~ 
               fish_basin  
             # s(doy_id, by = fish_basin, bs = "cc", k = 20) +
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             #   k = c(20, 4))
)

m2 <- update(m, . ~ 
               # fish_basin  + 
               s(doy_id, by = fish_basin, bs = "cc", k = 20) 
              
)

m3 <- update(m, . ~ 
               # fish_basin  + 
               s(doy_id, bs = "cc", k = 20)
             # s(floy_tag, year, by = fish_basin, bs = c("re", "re"), 
             #   k = c(20, 4)) + 
)

m4 <- update(m, . ~ 
               fish_basin  + 
               s(doy_id, by = fish_basin, bs = "cc", k = 20) + 
               ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
             
)


m5 <- update(m, . ~ 
                fish_basin  + 
                # s(doy_id, by = fish_basin, bs = "cc", k = 20) + 
                # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
                #   k = c(20, 4)) +
                ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
              
)

m6 <- update(m, . ~ 
                # fish_basin  + 
                s(doy_id, by = fish_basin, bs = "cc", k = 20) +
                # s(floy_tag, year, by = fish_basin, bs = c("re", "re"),
                #   k = c(20, 4)) +
                ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3))
              
)









# m and m18 are the same m19 invlovles an autocorelation structure 
m18 <- bam(fs ~ 
             fish_basin + 
             s(doy_id, by = fish_basin, bs = "cc", k = 20) + 
             ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3)), 
           data = soa, 
           select = TRUE
)

acf(resid_gam(m18))

r1 <- itsadug::start_value_rho(m18, plot = TRUE, lag = 4)
r1




m19 <- bam(fs ~ fish_basin  + 
             s(doy_id, by = fish_basin, bs = "cc", k = 20),
        
           method = "fREML",
           data = soa, 
           select = TRUE, 
           discrete = TRUE,
           rho = r1, 
           AR.start = soa$start_event
           
)


m20 <- bam(fs ~ fish_basin  + 
             s(doy_id, by = fish_basin, bs = "cc", k = 20) +
             ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(20, 3)),
           method = "fREML",
           data = soa, 
           select = TRUE, 
           discrete = TRUE,
           rho = r1, 
           AR.start = soa$start_event
           
)

m21 <- bam(fs ~ 
             s(doy_id, by = fish_basin, bs = "cc", k = 20), 
           method = "fREML",
           data = soa, 
           select = TRUE, 
           discrete = TRUE,
           rho = r1, 
           AR.start = soa$start_event
           
)


acf(resid_gam(m19))
acf(resid_gam(m20))


# anova(m18, m19, test = "F")
AIC(m18)
AIC(m19)


# create model list for model selection ------
model_list <- list(m, m1, m2, 
                   m3, m4, m5, m6,
                   m19, m20, m21
)
# give the elements useful names
names(model_list) <- c("m", 
                       "m1", "m2",
                       "m3", "m4", "m5", "m6", 
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
de <- paste("\u394", "AIC", sep = " ")
glance_summary <- glance_summary %>% 
  mutate(model = case_when(id %in% c("m20", "m19", "m21") ~ paste(model, 
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
                                  "Activity Scope",
                                  "gamm_soa_model_selection.xlsx"))
# # pridicted model --------
par(mfrow = c(2, 2))
gam.check(m20)
# ------------- plot -------

# create new datafreame with dummmy variables for RE for plotting 
dat_2 <- soa

glimpse(dat_2)

# use prediction to get interpolated points 
fits <- predict.bam(m20, newdata = dat_2, 
                    type = "response", se = TRUE, discrete = FALSE,
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
  arrange(doy_id)

# double check that predicts looks correct 
glimpse(predicts) 



# create month labels 
predicts %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  group_by(month_abb) %>% 
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
  xmin = 220,
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
    aes(x = xmin + 25, y = 91.25, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 30, y = 91.25, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_point(data = soa, aes(x = doy_id, y = fs, 
                            colour = fish_basin), 
             size = 3, alpha = 0.5) + 
  geom_line(
    aes(x = doy_id, y = fit, colour = fish_basin), size = 1) +
  geom_ribbon( 
    aes(ymin = lower,
        ymax = upper,
        x = doy_id, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  scale_shape_discrete(name = "Basin") +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_x_continuous(breaks = seq(25, 350, 65), 
                     label = month_label) +
  scale_y_continuous(breaks = seq(12.5, 100, 12.5)) + 
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.92),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y =  expression(paste("Swimming Activity (mg", 
                             O[2]," ", kg^-1, " ", h^-1, ")"))) -> p 

p
ggsave(plot = p, filename = here("plots",
                                 "Daily GAMM Plots",
                                 "factoral_scope_doy_gamm_acf.png"), width = 11,
       height = 7 )
