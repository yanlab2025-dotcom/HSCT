###############This is data for Fig 3g##################
rm(list = ls())
library(survival)
library(survminer)
library(cowplot)
load("Fig3.RData")
#########################################################################
#data input preparation
    metadata_cox_OUt$Out_cluster <- factor(
      metadata_cox_OUt$Out_cluster,
      levels = c("g__Rothia", "g__Lautropia", "g__Burkholderia")
    )
    surv_obj <- Surv(metadata_cox_OUt$death_time, metadata_cox_OUt$death)
    fit <- survfit(surv_obj ~ Out_cluster, data = metadata_cox_OUt)
    pairwise_survdiff(Surv(death_time, death) ~ Out_cluster, data = metadata_cox_OUt)

#figure generation
    km_plot <- ggsurvplot(
      fit,
      data = metadata_cox_OUt,
      pval = TRUE,  
      pval.size = 3.5,
      conf.int = TRUE, 
      risk.table.title = "",
      risk.table = TRUE,  
      risk.table.fontsize = 4,
      size=0.66,
      legend.title = "Out cluster",  
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
      palette = c("g__Rothia" = "#ff9f42","g__Lautropia" = "#4e9e9e","g__Burkholderia" = "#ff1f00"),
      legend.labs = levels(metadata_cox_OUt$Out_cluster)
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
