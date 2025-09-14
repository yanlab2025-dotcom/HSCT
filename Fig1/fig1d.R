###############This is data for Fig 1d####
rm(list = ls())
library(vegan)
library(survival)   
library(survminer)  
library(tidyr)
library(dplyr)
library(tibble)
library(cowplot)
load("Fig1.RData")
#################################################################################
#data input preparation
    surv_object <- Surv(time = merged_data$death_time, event = merged_data$death)
    km_fit <- survfit(surv_object ~ Infection_in, data = merged_data)

#figure generation
    km_plot <- ggsurvplot(
      km_fit,
      data = merged_data,
      pval = TRUE,  
      pval.size = 3.5,
      conf.int = TRUE, 
      risk.table.title = "",
      risk.table = TRUE,  
      risk.table.fontsize = 3.5,
      legend.labs = c("No","Yes"),  
      legend.title = "Infection",  
      xlab = "Time(days)",  
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
      ylab = "Survival probability", 
      palette = c( "#A4BED5FF", "#453947FF"))
      Fig1D <- plot_grid(
        km_plot$plot, 
        km_plot$table, 
        ncol = 1, 
        align = "v", 
        rel_heights = c(3, 1))

      Fig1D
