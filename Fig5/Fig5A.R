###########This is data for Fig5A###############
rm(list = ls())
library(ggplot2)   
library(dplyr)      
library(vegan)     
library(ggpubr)     
library(patchwork)  
library(scales)  
load("Fig5.RData")
############################################################################
# data input preparation 
    filtered_AUS <- filtered_AUS[order(abs(filtered_AUS[, 3]), decreasing = FALSE), ]
    filtered_AUS <- filtered_AUS %>%
      mutate(
        species_label = paste(dt_species)
      )
    filtered_AUS$species_label <- factor(filtered_AUS$species_label, levels = rev(unique(filtered_AUS$species_label)))
    filtered_AUS <- filtered_AUS[filtered_AUS$outcome == "PFS", ]
    filtered_AUS$Pval_mediate
############################################################################
# mediation coefficient plot
    p_med <- ggplot(filtered_AUS, aes(x = coef_mediate, 
                                      y = species_label, 
                                      xmin = coefCI_mediate_low, 
                                      xmax = coefCI_mediate_high)) +
      geom_point(shape = 18, size = 8,color="#808080ff") +
      geom_errorbarh(height = 0.2,color="#808080ff") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#808080ff") +   
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black", size = 0.8),  
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),
        axis.text.x = element_text(size = 6),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black", size = 0.5),    
        axis.ticks.length = unit(0.2, "cm")                       
      ) +
      labs(
        x = "Mediation coefficient",
        y = NULL
      ) +
      scale_y_discrete(position = "left") 
    p_med

# ridge plot
    p <- ggplot(data=zll$nz, aes(group=factor(y), fill=factor(AUS_group_numeric), color=factor(AUS_group_numeric))) + 
      geom_ridgeline(aes(height=height, x=x, y=y), alpha = 0.75) +
      zeroplots+
      scale_fill_manual(values = c("NO" = "#0072B2", "Yes" = "#D55E00"), name="PFS") +
      scale_color_manual(values = c("NO" = "#0072B2", "Yes" = "#D55E00"), name="PFS")+
      labs(
        x = "Log10 (relative abundance to non-PFS median)",
        y = NULL,
        title = NULL
      ) +
      scale_y_continuous(expand=c(0, 0),
                         limits=c(0.5, 1+length(levels(df$feature)))) +
      scale_x_continuous(expand=c(0, 0), 
                         limits=c(min(limits[1], suppressWarnings(min(zll$z$xmin)-zw*0.35)), limits[2])) +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.6)
      ) 
    p

# combine plots
    Fig5A <- grid.arrange(p_med, p, ncol = 2, widths = c(1, 0.6))  
    Fig5A
