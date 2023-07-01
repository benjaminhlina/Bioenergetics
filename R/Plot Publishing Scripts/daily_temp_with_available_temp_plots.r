# load packages ----

library(dplyr)
library(ggplot2)
# library(ggh4x)
library(here)
# library(lemon)
library(patchwork)
library(readr)


# bring in both plots -----

gam <- read_rds(here("Plot Objects", 
                     "mdt_used_GAMM_available_13.rds"))


vi <- read_rds(here("Plot Objects", 
                    "daily_temp_GLMM_violin_plot.rds"))

# gam$layers[[3]] <- NULL
# gam$layers[[3]] <- NULL

gam <- gam + 
  theme(
    plot.tag.position = c(0.09, 0.97),
    plot.tag = element_text(face = "bold")
  )
max_temp <- vi$data %>% 
  group_by(fish_basin, season) %>% 
  summarise(
    max = max(mean_temp)
  ) %>% 
  ungroup() %>% 
  arrange(season, fish_basin)
sig_let <- tibble(
  letter = c(rep("A", 3), rep("B", 2), "BD", 
             rep("C", 2), "CD", rep("E", 2), 
             "F"),
  x = c(0.7, 1, 1.3, 1.7, 2, 2.31, 
        2.7, 3, 3.31, 3.7, 4, 4.3),
  y = rep(0.5, 12)
)

# max(vi$data$mean_temp)
# glimpse(vi)
vi_2 <- vi + 
  geom_text(data = sig_let, aes(x = x, 
                                y = y + max_temp$max, 
                                label = letter), 
            size = 5) + 
  theme(
    legend.position = "none", 
    plot.tag.position = c(0.09, 0.97),
    plot.tag = element_text(face = "bold")
  )

# vi_2
p3 <- gam / vi_2 + 
  plot_annotation(tag_suffix = ")", 
                  tag_levels = "a")
# p3

ggsave(plot = p3, filename = here("plots",
                                  "Combined GAMM and Violin",
                                  "gamm_violin_temp_doy_avaialable_temp.png"), 
       width = 11,
       height = 7 * 2)

