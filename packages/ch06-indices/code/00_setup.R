# Shared setup: packages, chart theme and colours.
library(dplyr)
library(ggplot2)
library(scales)
library(moments)
library(knitr)
options(width = 72)
book_blue <- "#1F66A8"
book_orange <- "#C2571A"
book_gray <- "#AEB6C1"
theme_set(
  theme_minimal(base_size = 9.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.margin = margin(4, 8, 4, 4))
)

