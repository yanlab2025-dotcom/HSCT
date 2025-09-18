###############This is data for Fig 3b###############
rm(list = ls())
library(ggplot2)
library(grid)
library(gridExtra)
library(codyn)
library(dplyr)
library(tidyr)
library(readxl)
load("Fig3.RData")
##################################################################################
#data input preparation
    stab_rbind$group <- factor(stab_rbind$group, levels = c("g__Rothia", "g__Prevotella", "g__Streptococcus",
                                                            "g__Lautropia","g__Burkholderia"))
    stab_rbind$log_stability <- log10(stab_rbind$stability)
    col_group = c(
            "g__Rothia" = alpha("#ff9f42", 0.75),
            "g__Prevotella" = alpha("#44644a", 0.75),
            "g__Streptococcus" = alpha("#5454c7", 0.75),
            "g__Lautropia" = alpha("#4e9e9e", 0.75),
            "g__Burkholderia" = alpha("#ff1f00", 0.75)
    )

#figure generation
    Fig3b <- ggplot(stab_rbind, aes(x = group, y = log_stability, fill = group)) +
      geom_jitter(color = "#A9A9A9",size=1,shape=16,alpha=0.75) +
      geom_boxplot(color = "black",outlier.shape = NA,lwd=0.5,fatten = 1) +  
      labs(x = "Cluster", y = "log(community stability)") +
      theme_minimal() +
      scale_fill_manual(values = col_group) + 
      theme(
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.title = element_text(size = 10),  
            legend.text = element_text(size = 10),
            legend.key.size = unit(0.2, "cm"),
            panel.grid = element_blank(), 
            panel.border = element_rect(color = "black", fill = NA, size = 0.66),  
            axis.ticks = element_line(color = "black", size = 0.33),  
            axis.ticks.length = unit(0.1, "cm") ,
            legend.position = "none")

    Fig3b
