############### This is data for Fig 3J ###############
library(ggplot2)
library(ggpubr)
load("Fig3J.RData")
make_fig3J <- function(state_delta) {
  comparison_cols <- c("EF vs Ct" = "#D95F02","M6 vs Ct" = "#1B9E77")
  ggplot(
    state_delta,
    aes(delta_pp, state_label, colour = comparison)) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.35,
      colour = "#777777") +
    geom_segment(
      aes(
        x = 0,
        xend = delta_pp,
        yend = state_label),
      linewidth = 0.45,
      alpha = 0.7) +
    geom_point(size = 1.8) +
    facet_grid(
      logic_class ~ .,
      scales = "free_y",
      space = "free_y") +
    scale_colour_manual(
      values = comparison_cols) +
    labs(
      title = "Convergent and branch-specific immune-state shifts",
      subtitle = "Positive values indicate higher abundance than Ct",
      x = "Percentage-point change vs Ct",
      y = NULL,
      colour = NULL) +
    theme_pubr(base_size = 6.5) +
    theme(
      legend.position = "top",
      strip.text.y = element_text(
        angle = 0,
        size = 5.7))}

# figure generation
Fig3J <- make_fig3J(state_delta)
Fig3J
