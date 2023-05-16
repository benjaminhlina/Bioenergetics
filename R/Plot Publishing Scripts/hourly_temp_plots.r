# load packages ----

library(dplyr)
library(ggplot2)
library(ggh4x)
library(here)
library(lemon)
library(patchwork)
library(readr)


# bring in both plots -----

gam <- read_rds(here("Plot Objects", 
                     "hourly_temp_GAMM_plots.rds"))


vi <- read_rds(here("Plot Objects", 
                    "hourly_temp_GLMM_violin_plot.rds"))





p3 <- gam / vi
p3

ggsave(plot = p3, filename = here("plots",
                                  "gamm_violin_temp_hourly.png"), 
       width = 11 * 2,
       height = 7 * 2.5)
