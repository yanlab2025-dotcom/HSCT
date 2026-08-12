###############This is data for Fig 3I#######
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
load("fig3I.Rdata")
##############################################################################

groups <- c("Ct", "EF", "M6")
group_cols <- c(Ct = "#4D4D4D", EF = "#D95F02", M6 = "#1B9E77")


# fig3I_left panel plot
fig3I_left <- ggplot(fig3I_data, aes(percent, burden, colour = sample)) +
  geom_point(position = position_dodge(width = 0.52), size = 2.1) +
  geom_text(
    aes(label = sprintf("%.1f", percent)),
    position = position_dodge(width = 0.52),
    hjust = -0.35,
    size = 2.5,
    family = "Arial",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = group_cols, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, 68),
    breaks = c(0, 20, 40, 60),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Cell-state burden distinguishes severe groups from Ct",
    x = "% of curated immune cells",
    y = NULL,
    colour = "Group"
  ) +
  theme_pubr(base_size = 7) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.box.just = "left",
    plot.tag.position = c(0, 1)
  )

fig3I_left



# fig3I_right panel plot
comparison_cols <- c(
  "EF vs Ct" = group_cols[["EF"]],
  "M6 vs Ct" = group_cols[["M6"]]
)

fig3I_right <- ggplot(
  state_delta,
  aes(x = delta_pp, y = state_label, colour = comparison)
) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "#777777") +
  geom_segment(
    aes(x = 0, xend = delta_pp, yend = state_label),
    linewidth = 0.45,
    alpha = 0.7
  ) +
  geom_point(size = 1.8) +
  facet_grid(logic_class ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = comparison_cols) +
  labs(
    title = "Convergent and branch-specific immune-state shifts",
    subtitle = "Positive values indicate higher abundance than Ct",
    x = "Percentage-point change vs Ct",
    y = NULL,
    colour = NULL
  ) +
  theme_pubr(base_size = 6.5) +
  theme(
    legend.position = "top",
    strip.text.y = element_text(angle = 0, size = 5.7),
    plot.tag.position = c(0, 1)
  )

fig3I_right


