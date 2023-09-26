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

# ---- bring in to calcuate daily theroritcal scope ---- 


dat <- read_rds(here("Saved Data",
                     "Daily_theoratical_smr_mmr.rds")) 

glimpse(dat)



# ---- model ----

aec <- dat$aerobic_scope

descdist(aec)

ggplot(data = dat, aes(x = aerobic_scope)) + 
  geom_histogram()

fit_gamma <- fitdist(aec, distr = "gamma", method = "mme")
fit_norm <- fitdist(aec, distr = "norm", method = "mle")

plot(fit_gamma)
plot(fit_norm)

dat <- dat %>% 
  group_by(year) %>% 
  arrange(year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE), 
         year = factor(year)) %>% 
  ungroup() %>% 
  arrange(date, start_event)



dat %>% 
  filter(doy_id %in% seq(25, 350, 65)) %>% 
  mutate(
    month_abb = month(date, label = TRUE)
  ) %>% 
  group_by(month_abb) %>% 
  summarise() %>% 
  .$month_abb -> month_labels




#  --------- start GAMM--------
m <- bam(aerobic_scope ~ 
           fish_basin + 
           s(doy_id, by = fish_basin, bs = "cc", k = 14) + 
           # ti(doy_id, fish_basin, bs = c("cc", "fs"), k = c(14, 3)) + 
           s(floy_tag, by = fish_basin, bs = "re") + 
           s(year, by = fish_basin, bs = "re"), 
         family = Gamma(link = "inverse"), 
         method = "fREML",
         data = dat, 
         select = TRUE
)

acf(resid_gam(m))

r1 <- itsadug::start_value_rho(m, plot = TRUE, lag = 17)
r1

m1 <- update(m,
             discrete = TRUE,
             rho = r1, 
             AR.start = start_event
             
)


par(mfrow = c(2, 2))
gam.check(m)
gam.check(m1)
plot(m)


# -------- predict from model ---------
# look at overall effect terms -----
m_overall <- anova.gam(m1, freq = FALSE)

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
