############### This is data for Fig 3I ###############
library(ggplot2)
library(ggpubr)
load("Fig3I.RData")
make_fig3I <- function(fig3I_data) {
  cols <- c(Ct = "#4D4D4D",EF = "#D95F02",M6 = "#1B9E77")
  ggplot(
    fig3I_data,
    aes(percent, burden, colour = sample)) +
    geom_point(
      position = position_dodge(width = 0.52),
      size = 2.1) +
    geom_text(
      aes(label = sprintf("%.1f", percent)),
      position = position_dodge(width = 0.52),
      hjust = -0.35,
      size = 2.5,
      show.legend = FALSE) +
    scale_colour_manual(
      values = cols,
      drop = FALSE) +
    scale_x_continuous(
      limits = c(0, 68),
      breaks = c(0, 20, 40, 60),
      expand = expansion(mult = c(0, 0))) +
    labs(
      title = "Cell-state burden distinguishes severe groups from Ct",
      x = "% of curated immune cells",
      y = NULL,
      colour = "Group") +
    theme_pubr(base_size = 7) +
    theme(
      legend.position = "top")}

# figure generation
Fig3I <- make_fig3I(fig3I_data)
Fig3I