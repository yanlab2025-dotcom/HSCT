###############This is data for Fig 3F##########
rm(list = ls())
library(survival)
library(survminer)
library(cowplot)
load("Fig3.RData")
#########################################################################
#data input preparation
    surv_obj <- Surv(metadata_cox_In$death_time, metadata_cox_In$death)
    fit <- survfit(surv_obj ~ In_cluster, data = metadata_cox_In)
    pairwise_survdiff(Surv(death_time, death) ~ In_cluster, data = metadata_cox_In,
                      p.adjust.method = "BH")

#figure generation
    km_plot <- ggsurvplot(
      fit,
      data = metadata_cox_In,
      pval = TRUE,  
      pval.size = 4,
      conf.int = TRUE, 
      risk.table.title = "",
      risk.table = TRUE,  
      risk.table.fontsize = 4,
      size=0.66,
      legend.title = "In cluster", 
      xlab = "Time(Days)",  
      ggtheme = theme_classic(base_size = 12) +
        theme(
          axis.line.x = element_line(size = 0.33),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 0),
          axis.line.y = element_line(size = 0.33),
          axis.ticks = element_line(size = 0.33),
          axis.ticks.length = unit(0.1, "cm")
        ),
      tables.theme = theme_classic(base_size = 1) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 12,color = "black"),
          axis.title.x = element_text(size = 12,color = "black"),
          axis.text.x = element_text(size = 12,color = "black"),
          axis.line.x = element_line(size = 0.33),  
          axis.line.y = element_line(size = 0.33),
          axis.ticks = element_line(size = 0.33),
          axis.ticks.length = unit(0.1, "cm")
        ),
      ylab = "Survival probability", 
      palette = c("Rothia" = "#ff9f42","Prevotella" = "#44644a","Streptococcus"="#5454c7", "Lautropia" = "#4e9e9e"),
      legend.labs = levels(metadata_cox_In$In_cluster)
    )
    km_plot
    combined_plot <- plot_grid(
      km_plot$plot,
      km_plot$table,
      ncol = 1,
      align = "v",
      rel_heights = c(3, 1.4)
    )
    combined_plot