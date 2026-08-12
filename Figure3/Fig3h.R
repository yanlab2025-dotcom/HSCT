###############This is data for Fig 3H####
rm(list = ls())
library(data.table)
library(ggplot2)
library(ggpubr)

load("Fig3h.RData")
#################################################################################
################ Fig3H left panel ################

merged_levels <- c(
  "CD4 T", "CD8 T", "NK", "NKT", "B cell", "Monocyte", "Macrophage",
  "DC", "Neutrophil", "Basophil", "Eosinophil", "mast cell", "Cycling myeloid")

merged_cols <- c(
  "CD4 T" = "#1F78B4",
  "CD8 T" = "#08306B",
  "NK" = "#E6550D",
  "NKT" = "#F16913",
  "B cell" = "#6A3D9A",
  "Monocyte" = "#006D2C",
  "Macrophage" = "#31A354",
  "DC" = "#008B8B",
  "Neutrophil" = "#4D4D4D",
  "Basophil" = "#C51B8A",
  "Eosinophil" = "#D95F0E",
  "mast cell" = "#8C6D31",
  "Cycling myeloid" = "#7F2704"
)

present_levels <- c("CD4 T","CD8 T","NK","B cell","Monocyte","Macrophage","DC","Neutrophil","Basophil","Eosinophil","Cycling myeloid")

theme_merged <- function(base_size = 6.6) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line = element_line(linewidth = 0.34, colour = "black"),
      axis.ticks = element_line(linewidth = 0.34, colour = "black"),
      axis.text = element_text(colour = "black"),
      legend.title = element_text(size = base_size - 0.3),
      legend.text = element_text(size = base_size - 0.6),
      strip.text = element_text(size = base_size - 0.2, face = "bold"),
      plot.title = element_text(size = base_size + 0.8, face = "bold"),
      panel.grid = element_blank()
    )
}

plot_embedding <- function(
    data, x_column, y_column, title, point_size = 0.20) {
  label_data <- data[
    , .(
      x = median(get(x_column), na.rm = TRUE),
      y = median(get(y_column), na.rm = TRUE),
      n = .N
    ),
    by = merged_cell_type
  ][n >= 50]
  data.table::setorder(label_data, merged_cell_type)
  
  ggplot(
    data,
    aes(
      x = .data[[x_column]], y = .data[[y_column]],
      colour = merged_cell_type
    )
  ) +
    geom_point(size = point_size, alpha = 0.78, stroke = 0) +
    scale_colour_manual(
      values = merged_cols[present_levels],
      drop = FALSE,
      name = "Cell type"
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(size = 2.2, alpha = 1), ncol = 1
      )
    ) +
    labs(
      title = title,
      x = sub("_", " ", x_column),
      y = sub("_", " ", y_column)
    ) +
    theme_merged() +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    ggrepel::geom_label_repel(
      data = label_data,
      aes(x = x, y = y, label = merged_cell_type),
      inherit.aes = FALSE,
      size = 2.2,
      label.size = 0.15,
      fill = "white",
      alpha = 0.90,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.25,
      seed = 20260704
    )
}

### Fig3H-left panel plot
fig3h_left <- plot_embedding(
  umap_plot_data,
  "UMAP_1",
  "UMAP_2",
  "Merged immune cell types: UMAP",
  point_size = 0.20
)

fig3h_left


################ Fig3H right panel ################

composition_plot_data[, merged_cell_type := factor(
  as.character(merged_cell_type), levels = rev(present_levels)
)]

### Fig3H-left panel plot
fig3h_right <- ggplot(
  composition_plot_data,
  aes(x = sample, y = percent, fill = merged_cell_type)
) +
  geom_col(width = 0.78, colour = "white", linewidth = 0.12) +
  scale_fill_manual(
    values = merged_cols[present_levels],
    breaks = present_levels,
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = "Merged immune cells (%)",
    fill = "Cell type",
    title = "Merged cell-type composition"
  ) +
  theme_merged() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.height = grid::unit(3.2, "mm"),
    legend.position = "right"
  )

fig3h_right


