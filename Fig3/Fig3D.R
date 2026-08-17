############### This is data for Fig 3D ###############
library(dplyr)
library(ggplot2)
load("Fig3D.RData")
make_fig3D <- function(data) {ggplot(data$sig_abundance_top9, aes(Study, Species)) +
    geom_point(
      data = ~ filter(.x, Abundance > 0),
      aes(size = Abundance, color = Abundance)) +
    scale_size_continuous(
      name = "Mean abundance",
      range = c(2, 6),
      breaks = c(0, 0.025, 0.05, 0.075, 0.1)) +
    scale_color_gradientn(
      colours = c("#BFDBFE", "#FFE0B2", "#FFCC80", "#FFB74D", "#F57C00"),
      name = "Relative\nabundance") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(face = "italic"),
      axis.title = element_blank())}

# figure generation
Fig3D <- make_fig3D(list(sig_abundance_top9 = sig_abundance_top9))
Fig3D