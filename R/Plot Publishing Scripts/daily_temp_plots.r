# load packages ----

library(ggplot2)
library(here)
library(patchwork)
library(readr)


# bring in both plots -----

gam <- read_rds(here("Plot Objects", 
                     "daily_temp_GAMM_plot.rds"))


vi <- read_rds(here("Plot Objects", 
                    "daily_temp_GLMM_violin_plot.rds"))

gam <- gam + 
  theme(
    plot.tag.position = c(0.075, 0.97),
    plot.tag = element_text(face = "bold")
  )
# gam$layers[[3]] <- NULL
# gam$layers[[3]] <- NULL

vi_2 <- vi + 
  theme(
    legend.position = "none",
    plot.tag.position = c(0.075, 0.97),
    plot.tag = element_text(face = "bold")
  )


p3 <- gam / vi_2 + 
  plot_annotation(tag_suffix = ")", 
                  tag_levels = "a")

p3

ggsave(plot = p3, filename = here("plots",
                                  "Combined GAMM and Violin",
                                  "gamm_violin_temp_doy.png"), 
       width = 11,
       height = 7 * 2)
