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
                     "daily_rmr_GAMM_plot.rds"))


vi <- read_rds(here("Plot Objects", 
                    "daily_rmr_GLMM_violin_plot.rds"))


gam$layers
vi

gam$layers[[3]] <- NULL
gam$layers[[3]] <- NULL

vi_2 <- vi + 
  theme(legend.position = "none")


p3 <- gam / vi_2
p3

ggsave(plot = p3, filename = here("plots",
                                  "Combined GAMM and Violin", 
                                  "gamm_violin_rmr_doy.png"), 
       width = 11,
       height = 7 * 2)
