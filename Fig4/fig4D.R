###############This is data for Fig 4D#######
rm(list=ls()) 
library(ggplot2)
library(readxl)
library(scales)
library(patchwork)
library(grid) 
load( "Fig4.RData")
#####################################################################################
#figure generation
    p1 <- ggplot(data, aes(x = coef1, y = FDV,color = region)) + 
      annotate(
        "rect",
        xmin = -Inf, xmax = -0.05,
        ymin = 1, ymax = Inf,
        alpha = 0.2,
        fill = "#74add1"
      ) +
      annotate(
        "rect",
        xmin = 0.05, xmax = Inf,
        ymin = -Inf, ymax = 1,
        alpha = 0.2,
        fill = "#f46d43"
      ) +
      scale_color_manual(
        values = c(
          "response" = "#4575b4",
          "resistant" = "#d73027",
          "other" = "gray"
        ))+
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.33) +
      geom_vline(xintercept = 0.05, linetype = "dashed", color = "black", linewidth = 0.33) +
      geom_vline(xintercept = -0.05, linetype = "dashed", color = "black", linewidth = 0.33) +
      geom_point(size = 1.5, alpha = 1,shape=16) +
      labs(x = "Maaslin coefficient", y = "FDV") +
      xlim(-0.15, 0.15) +
      theme_minimal() +
      annotate("segment", 
               x = -0.05, xend = -0.12, 
               y = 0.2, yend = 0.2, 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), 
               color = "#4575b4", size = 0.6) +
      annotate("text", 
               x = -0.1, y = 0.0,  
               label = "Response", 
               color = "black", hjust = 1, size = 3) + 
      annotate("segment", 
               x = 0.05, xend = 0.12, 
               y = 0.2, yend = 0.2, 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), 
               color = "#ff4757", size = 0.6) +
      annotate("text", 
               x = 0.1, y = 0.0,  
               label = "Resistant", 
               color = "black", hjust = 0, size = 3) + 
      theme(
        plot.title = element_text(size = 9, face = "bold", hjust = 0),
        axis.title.x = element_text(size = 9, color = "black"),
        axis.title.y = element_text(size = 9, color = "black"),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.text.y = element_text(size = 9, color = "black"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.66),
        axis.ticks.x = element_line(size = 0.33, colour = "black"),
        axis.ticks.y = element_line(size = 0.33, colour = "black"),
        axis.ticks.length = unit(0.1, "cm")
      )
    p1
###################################################################################
# response species abundance plot
    aresponse <- ggplot(merged_abP_long, aes(x = Value, y = colname, color = Group)) +
      geom_point(size = 2) +
      geom_segment(data = merged_abP, 
                   aes(x = Total.x, xend = Total.y, y = colname, yend = colname), 
                   arrow = arrow(length = unit(0.1, "cm"), type = "closed"), 
                   color = "black", linewidth = 0.33) +
      scale_x_continuous(trans = "log10", name = "Relative Abundance (log scale)", limits = c(0.001, 12)) +
      scale_color_manual(name = "Group", values = c("In" = "#9CD2FB", "Out" = "#55B0FA")) +
      labs(y = "Response species") +
      theme_minimal() +
      scale_y_discrete(position = "right")+
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=8,color = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=8,color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.line = element_line(colour = 'black', linewidth = 0.33),
        panel.border = element_rect(color = "black", fill = NA, size = 0.66),
        axis.ticks = element_line(size = 0.33, colour = "black"),
        axis.ticks.length = unit(0.1, "cm")
      )

# resistant species abundance plot
    aresistant <- ggplot(merged_abN_long, aes(x = Value, y = colname, color = Group)) +
      geom_point(size = 2) +
      geom_segment(data = merged_abN, 
                   aes(x = Total.x, xend = Total.y, y = colname, yend = colname), 
                   arrow = arrow(length = unit(0.1, "cm"), type = "closed"), 
                   color = "black", linewidth = 0.33) +
      scale_x_continuous(
        trans = "log10",
        name = "Log10(Relative abundance)",
        limits = c(0.001, 12),
        labels = label_number(accuracy = 0.001, trim = TRUE)
      )+
      scale_color_manual(name = "Group", values = c("In" = "#9CD2FB", "Out" = "#55B0FA")) +
      labs(y = "Resistant species") +
      scale_y_discrete(position = "right")+
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title.x = element_text(size=9,color = "black"),
        axis.title.y = element_text(size=9,color = "black"),
        axis.text.x = element_text(size=8,angle = 45, hjust = 1,color = "black"),
        axis.text.y = element_text(size=9,color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.66),
        axis.ticks.x = element_line(size = 0.33, colour = "black"),
        axis.ticks.y = element_line(size = 0.33, colour = "black"),
        axis.ticks.length = unit(0.1, "cm")
      )
    p2 <- aresponse/aresistant+plot_layout(heights = c(10, 6))
    p2

# Combine Plots 
    Fig4D <- (p1+p2)+plot_layout(widths = c(2, 1.5))
    Fig4D