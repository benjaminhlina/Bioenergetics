# load packages ----

library(dplyr)
library(ggplot2)
library(ggh4x)
library(here)
library(lemon)
library(patchwork)
library(readr)

# --- bring in plots ---- 
a <- read_rds(here("Plot Objects", 
                    "mmr_rmr_smr_jan.rds"))


b <- read_rds(here("Plot Objects", 
                  "Scope_of_activity_and_AS_gamm_plot_jan.rds"))
c <- a

d <- b
# glimpse(a)


a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
b$layers[[1]] <- NULL
b$layers[[1]] <- NULL
b$layers[[1]] <- NULL
b$layers[[1]] <- NULL
b$layers[[1]] <- NULL
# a$layers
# a$layers[[4]] <- NULL
# a$layers[[3]] <- NULL
# a$layers[[3]] <- NULL

p <- a / b + 
  plot_annotation(
    tag_levels = "a",
    tag_suffix = ")"
  )

p
ggsave(plot = p, filename = here("plots",
                                  "Combined metabolic rates",
                                  "metabolism_and_scope_plot_wo_grey_CORRECT_pt_jan.png"), 
       width = 11,
       height = 7 * 2)
p1 <- c / d + 
  plot_annotation(
    tag_levels = "a",
    tag_suffix = ")"
  )

p1
ggsave(plot = p1, filename = here("plots",
                                  "Combined metabolic rates",
                                  "metabolism_and_scope_plot_CORRECT_pt_jan.png"), 
       width = 11,
       height = 7 * 2)

a$layers[[1]] <- NULL
b$layers[[1]] <- NULL

p2 <- a / b + 
  plot_annotation(
    tag_levels = "a",
    tag_suffix = ")"
  )

# p2
ggsave(plot = p2, filename = here("plots",
                                 "Combined metabolic rates",
                                 "metabolism_and_scope_plot_wo_grey_CORRECT_jan.png"), 
       width = 11,
       height = 7 * 2)

# c$layers[[4]] <- NULL
# c$layers[[4]] <- NULL
# c$layers[[4]] <- NULL
# d$layers[[3]] <- NULL
# d$layers[[3]] <- NULL


p3 <- c / d + 
  plot_annotation(
    tag_levels = "a",
    tag_suffix = ")"
  )

# p3
ggsave(plot = p3, filename = here("plots",
                                  "Combined metabolic rates",
                                  "metabolism_and_scope_plot_CORRECT_jan.png"), 
       width = 11,
       height = 7 * 2)
ggsave(plot = p3, filename = here("plots",
                                  "Combined metabolic rates",
                                  "metabolism_and_scope_plot_CORRECT_jan.pdf"), 
       width = 11,
       height = 7 * 2)
