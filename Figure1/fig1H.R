###############This is data for Fig 1H######
rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx) 
library(ggpubr)
library(survival)
library(survminer)
library(cowplot)
load("Fig1.RData")
#######################################################################################
#data input preparation
    surv_object <- Surv(time = summarized_data$death_time, event = summarized_data$death)
    km_fit <- survfit(surv_object ~ AUS_group, data = summarized_data)
    
#figure generation
    km_plot <- ggsurvplot(
      km_fit,
      data = summarized_data,
      pval = TRUE,  
      pval.size = 3.5,
      conf.int = TRUE, 
      risk.table.title = "",
      risk.table = TRUE,  
      risk.table.fontsize = 3.5,         
      legend.labs = c(  "High_Score","Low_Score"),  
      legend.title = "AUS_death",  
      xlab = "Time (days)",          
      ylab = "Survival Probability", 
      ggtheme = theme_classic(base_size = 12) +
        theme(
          axis.line.x = element_line(size = 0.33),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 0),
          axis.line.y = element_line(size = 0.33),
          axis.ticks.length = unit(0.1, "cm")
        ),
      tables.theme = theme_classic(base_size = 1) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.ticks = element_line(color = "black", size = 0.33),
          axis.ticks.length = unit(0.1, "cm"),
          axis.text.x = element_text(size = 10),
          axis.line.x = element_line(size = 0.33),  
          axis.line.y = element_line(size = 0.33)
        ),
      palette = c("#08306b","#93C6E1FF")   
    )
    Fig1H <- plot_grid(km_plot$plot, km_plot$table, ncol = 1, align = "v", rel_heights = c(2, 1))
    Fig1H