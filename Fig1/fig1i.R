###############This is data for Fig 1i#######
rm(list = ls())
library(dplyr)
library(ggplot2)
load("Fig1.Rdata")
#####################################################################################
#data input preparation
    marker_names <- c(
      "IL6" = "IL-6",
      "CRP" = "C-Reactive Protein",
      "IL10" = "IL-10",
      "TNF-a" = "TNF-α",
      "IL4" = "IL-4",
      "IL2" = "IL-2",
      "PCT" = "Procalcitonin"
    )
    df_all$Marker <- marker_names[df_all$Marker]
    df_all$Marker <- factor(df_all$Marker, levels = rev(marker_names))
    df_all$Variable <- factor(df_all$Variable, levels = c("AUS", "Shannon"))

#figure generation
#q_value < 0.05 ~ "*"
#q_value < 0.25  ~ "."
    Fig1i <- ggplot(df_all, aes(x = Variable, y = Marker)) +
      geom_tile(aes(fill = Correlation), color = "white", width = 1, height = 1) +
      geom_text(aes(label = Signif), size = 5, color = "black") + 
      scale_fill_gradient2(
        low = "#1F8F99", mid = "#FFE6CC", high = "#994000", midpoint = 0
      ) +
      scale_size(range = c(3, 8)) +
      theme_bw() +
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        panel.grid = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.66),
        axis.ticks.x = element_line(size = 0.33, colour = "black"),  
        axis.ticks.y = element_line(size = 0.33, colour = "black"),   
        axis.ticks.length = unit(0.1, "cm"),
        legend.position = "right"
      ) 

    Fig1i
