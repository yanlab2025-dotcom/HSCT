###############This is data for Fig 3e############### 
library(tidyverse)
library(cowplot)
library(smplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
load("Fig3e.RData")
#####################################################################################
#data input preparation
    theme_classic <- function(base_size = 11, base_family = "", 
          base_line_size = base_size/22, 
          base_rect_size = base_size/22) 
    {theme_bw(base_size = base_size, base_family = base_family, 
          base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.border = element_blank(),                    
          panel.grid.major = element_blank(),           
          panel.grid.minor = element_blank(),              
          axis.line = element_line(colour = "black", linewidth = rel(1)),
          strip.background = element_rect(fill = "grey85", colour = "black", linewidth = 0.66),
          complete = TRUE)}

#figure generation
    p_combined <- ggplot(combined_data, aes(x = cluster, y = value, fill = cluster)) +
    geom_jitter(alpha = 0.7, size = 1.5, color = "grey80") +  
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    facet_wrap(~ Time_Period, scales = "free_x", ncol = 2) +
    labs(x = "Cluster",
          y = "T cells count",
          fill = "Cluster") +
    theme_classic() +
    geom_signif(data = signif_data_pre,
          aes(xmin = group1, xmax = group2, annotations = p.signif, y_position = y.position),  
          manual = TRUE,
          tip_length = 0.01,
          vjust = 0.5,
          textsize = 4,
          inherit.aes = FALSE ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          strip.background = element_blank(),  
          strip.text = element_blank() ,
          legend.position = "none") +
    scale_fill_manual(values = 
          c("f__Micrococcaceae"  = "#549ec7", 
          "f__Prevotellaceae"    = "#44644a",
          "f__Streptococcaceae"  = "#5454c7",
          "f__Veillonellaceae"   = "#08306b",
          "f__Burkholderiaceae"  = "#d62728"))
    p_combined

#####################################################################################
#data input preparation
    theme_classic <- function(base_size = 11, base_family = "", 
           base_line_size = base_size/22, 
           base_rect_size = base_size/22) 
    {theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.border = element_blank(),                    
           panel.grid.major = element_blank(),           
           panel.grid.minor = element_blank(),              
           axis.line = element_line(colour = "black", linewidth = rel(1)),
           strip.background = element_rect(fill = "grey85", colour = "black", linewidth = 0.66),
           complete = TRUE)}

#figure generation
    p1 <- ggplot(pre, aes(x = cluster, y = value, fill = cluster)) +
    geom_jitter(alpha = 0.7, size = 1.5, color = "grey80") +  
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +  
    facet_wrap(~ immune_marker, nrow = 1, scales = "free_y") + 
    labs(x = "Cluster", y = "Cell counts") +
    theme_classic() +
    scale_fill_manual(values =
          c("Micrococcaceae" = "#549ec7", 
          "Prevotellaceae" = "#44644a",
          "Streptococcaceae" = "#5454c7",
          "Veillonellaceae" = "#08306b",
          "Burkholderiaceae" = "#d62728")) +
    geom_signif(
          data = signif_data_Post,
          aes(xmin = group1, xmax = group2, annotations = label, y_position = y_position),  
          manual = TRUE,
          tip_length = 0.01,
          vjust = 0.5,
          textsize = 4,
          inherit.aes = FALSE ) +
    theme(axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.text.y = element_text(size = 12, color = "black", angle = 60, hjust = 1),  
           axis.title.y = element_text(size = 12, color = "black"),
           legend.position = "none",
           axis.line = element_line(size = 0.33, color = "black"),
           strip.text = element_text(size = 12, color = "black"))
    p1
    