###########This is data for Fig 6F#####################
rm(list=ls()) 
library(ggplot2)    
library(dplyr)      
library(ggpubr)     
library(patchwork) 
library(grid)      
load("Fig6.RData")
#########################################################################
#data input preparation
      plot_list <- list()  # 存储图形的列表
      for (i in seq_along(feature)) {
      bug <- feature[i]
      x1 <- data_boxplot[, "AUS_group"]
      x2 <- data_boxplot[, bug]
      x3 <- data_boxplot[, "PFS"]
      data <- data.frame(
        dt_grouped  = x1,
        metagenome = as.numeric(x2),
        PFS = x3
      )
      low_data <- data[data$dt_grouped  == "low_score", ]
      high_data <- data[data$dt_grouped  == "high_score", ]
      data <- rbind(low_data, high_data)
      data$Group <- with(data, paste0(
        ifelse(dt_grouped == "low_score", "Low", "High"),
        "\n",
        ifelse(PFS == 1, "+", "-")
      )) 
      data$Group <- factor(data$Group)
      custom_colors <- c(
        "#6696BF",  
        "#EB5C53",  
        "#CCE0F0",  
        "#FFE5DF"  
      )
      data$metagenome[data$metagenome == 0] <- 1e-6

# figure generation
        plot_list[[i]] <- ggplot(data, aes(x = Group, y = metagenome, fill = Group)) +
        geom_boxplot(outlier.shape = NA, color = "black") +
        geom_jitter(width = 0.2, size = 0.5, alpha = 0.5,shape=16) +
        coord_cartesian(clip = "off") + 
        scale_y_log10() +
        scale_fill_manual(
          values = custom_colors,
          name = "Group",  
          labels = c("High\n-" = "AUS_High/PFS_NO", "High\n+" = "AUS_High/PFS_Yes",
                     "Low\n-" = "AUS_Low/PFS_NO", "Low\n+" = "AUS_Low/PFS_Yes")
        ) +
        labs(
          y = "Relative abundance",
          x = NULL,
          title = bug
        ) +
        theme_classic() +
        theme(
          legend.position = "right",
          axis.line = element_line(color = "black", linewidth = 0.33),
          axis.ticks = element_line(size = 0.33, colour = "black"),
          axis.ticks.length = unit(0.1, "cm"),
          axis.text.x = element_text(size = 10, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "italic")
        ) +
        stat_compare_means(
          comparisons = list(c("High\n-", "High\n+"),
                             c("Low\n-", "Low\n+")),
          method = "wilcox.test",
          label = "p.signif"
        )
      grid.text("AUS\nPFS", x = unit(0.08, "npc"), y = unit(0.031, "npc"),
                just = "left", gp = gpar(fontface = "bold", fontsize = 9))
      }
# combine plots
      Fig6F <- wrap_plots(plot_list, nrow = 1) + 
      plot_layout(guides = "collect")  # 统一收集图例
      print(Fig6F)