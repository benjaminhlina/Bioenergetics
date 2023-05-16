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
                     "scope_for_activity_GAMM_plot.rds"))


vi <- read_rds(here("Plot Objects", 
                    "scope_for_activity_lm_plot.rds"))


gam$layers
vi

gam$layers[[3]] <- NULL
gam$layers[[3]] <- NULL

gam <- gam + 
  labs(y = expression(paste("Swimming Activity (mg", 
                  O[2]," ", kg^-1, " ", h^-1, ")")))


vi_2 <- vi + 
  theme(legend.position = "none") + 
  labs(y = expression(paste("Swimming Activity (mg", 
                            O[2]," ", kg^-1, " ", h^-1, ")")))


p3 <- gam / vi_2
p3

ggsave(plot = p3, filename = here("plots",
                                  "Combined GAMM and Violin",
                                  "gamm_violin_scope_for_activity.png"), 
       width = 11,
       height = 7 * 2)
