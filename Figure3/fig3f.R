###############This is data for Fig 3f##########
library(survival)   
library(survminer)     
library(ggplot2)     
library(cowplot)      
library(dplyr) 
load("Fig3f.RData")
#####################################################################################
#define plot colors
    cluster_palette <- c("f__Burkholderiaceae" = "#E41A1C",
                         "f__Micrococcaceae"   = "#549ec7",
                         "f__Prevotellaceae"   = "#44644a",
                         "f__Streptococcaceae" = "#5454c7",
                         "f__Veillonellaceae"  = "#08306b")

#data input preparation
    data_list <- list(Pre = metadata_pre, Post = metadata_post)
    survival_plots <- list()
    for (cluster_type in names(data_list)) {
    metadata <- data_list[[cluster_type]]
    metadata$cluster <- as.factor(metadata$cluster)
    surv_obj <- Surv(metadata$aGvHD_time, metadata$aGVHD)
    fit <- survfit(surv_obj ~ cluster, data = metadata)
    
#figure generation
    km_plot <- ggsurvplot(fit,
                          data = metadata,
                          pval = TRUE,  
                          pval.size = 3.5,
                          conf.int = TRUE, 
                          risk.table.title = "",
                          risk.table = TRUE,  
                          risk.table.fontsize = 4,
                          size = 0.66,
                          legend.title = paste0(cluster_type, " cluster"),  
                          xlab = "Time(Days)",  
    ggtheme = theme_classic(base_size = 12) +
    theme(axis.line.x = element_line(size = 0.33),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 0),
          axis.line.y = element_line(size = 0.33),
          axis.ticks = element_line(size = 0.33),
          axis.ticks.length = unit(0.1, "cm")),
    tables.theme = theme_classic(base_size = 1) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12, color = "black"),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.line.x = element_line(size = 0.33),  
          axis.line.y = element_line(size = 0.33),
          axis.ticks = element_line(size = 0.33),
          axis.ticks.length = unit(0.1, "cm")),
    ylab = "Survival probability",  
    palette = cluster_palette,  
    legend.labs = levels(metadata$cluster))
    survival_plots[[cluster_type]] <- plot_grid(
                          km_plot$plot,
                          km_plot$table,
                          ncol = 1,
                          align = "v",
                          rel_heights = c(3, 1.4))}
    pre_agvhd_cox <- survival_plots[["Pre"]]
    post_cox_agvhd <- survival_plots[["Post"]]
    pre_agvhd_cox+post_cox_agvhd
