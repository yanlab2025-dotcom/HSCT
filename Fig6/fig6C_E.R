###########This is data for Fig 6C_E#####################
rm(list=ls()) 
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
load("Fig6.RData")
########################################################################################
#effect size plot
    aGVHD_brm$X <- factor(aGVHD_brm$X, levels = aGVHD_brm$X[order(aGVHD_brm$Estimate, decreasing = FALSE)])
    Fig6C_1 <- ggplot(aGVHD_brm, aes(x = Estimate, y = X)) +
      geom_point(size = 2) +  
      geom_errorbarh(aes(xmin = l.95..CI, xmax = u.95..CI), 
                     height = 0.2, color = "black", size = unit(0.2, "mm")) +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
      coord_cartesian(clip = "off") +  
      theme_classic() +
      labs(x = "Effect Size (Estimate)", y = "") +
      theme(
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.border = element_rect(fill = NA, color = "black", size = unit(0.2, "mm")),  
        plot.title = element_blank()
      )
    Fig6C_1
########################################################################################
#boxplot of relative abundance
    dt.response.boxplot_long$variable <- as.character(dt.response.boxplot_long$variable)
    sorted_levels <- aGVHD_brm$X[order(aGVHD_brm$Estimate, decreasing = FALSE)]
    dt.response.boxplot_long$variable <- factor(dt.response.boxplot_long$variable, levels = sorted_levels)
    dt.response.boxplot_long$Target<-as.factor(dt.response.boxplot_long$Target)
    Fig6D_1 <- ggplot(dt.response.boxplot_long, 
                       aes(x = variable, 
                           y = sqrtAbundance, 
                           color = Target)) +
      coord_flip() +
      geom_boxplot(outlier.shape = NA, linewidth = 0.4, alpha = 0.5,
                   position = position_dodge(width = 0.75)) +
      geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
                  size = 0.5, alpha = 0.6) +
      stat_compare_means(method = "wilcox.test", 
                         label = "p.signif", 
                         #hide.ns = TRUE, 
                         size = 3.5) +
      scale_color_manual(values = c("0" = "#51C3CC", "1" = "#CC5800")) + #"0" represents "No", "1" represents "Yes" 
      labs(x =NULL, y =  "Relative abundance
           (sqrt transformation)") +
      theme_minimal(base_size = 8) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.line = element_line(color = "black", size = unit(0.2, "mm")),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    Fig6D_1
##############################################################################
#Hazard ratio plot
    aGVHD_brm$X <- factor(aGVHD_brm$X, levels = aGVHD_brm$X[order(aGVHD_brm$Estimate, decreasing = FALSE)])
    cox_table_subset$bug <- factor(cox_table_subset$bug, levels = levels(aGVHD_brm$X))
    cox_table_subset <- cox_table_subset[order(cox_table_subset$bug), ]
    Fig6E <- ggplot(cox_table_subset, aes(x = logHR, y = bug, fill = logHR)) +
      geom_segment(
        aes(x = logHR, xend = logHR, y = bug),
        linetype = "dotted", color = "gray30", linewidth = 0.4
      ) +
      geom_col(width = 0.6) +  
      scale_fill_gradientn(
        colors = c("#0F2C59", "#1A4E8B", "#2E75B7", "#4789E2", "#bababa", "#F4A1A1", "#F46D43", "#D73027", "#B22222"),
        values = scales::rescale(c(-0.4,-0.3,-0.2,-0.1, 0, 0.1,0.2,0.3, 0.4)), 
        limits = c(-0.4, 0.4),                                            
        name = "log10(HR)",
        na.value = "grey50"  
      ) +
      geom_text(
        data = subset(cox_table_subset, significant == TRUE),
        aes(x = logHR, y = bug),
        label = "*", color = "black", size = 4
      ) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
      theme_classic() +
      labs(
        x = "Log10(Hazard ratio)",
        y = NULL,
        title = NULL
      ) +
      scale_x_continuous(  
        limits = c(-0.4, 0.4),  
        breaks = seq(-0.4, 0.4, by = 0.1)  
      ) +
      theme(
        legend.position = "none",
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.line = element_line(color = "black", size = unit(0.2, "mm")),
        axis.text.y = element_blank(),
        
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    Fig6E
######################################################################################## 
#effect size plot for immune cell 
    aGVHD_brm_im$Y <- factor(aGVHD_brm_im$Y, levels = aGVHD_brm_im$Y[order(aGVHD_brm_im$Estimate, decreasing = FALSE)])
    Fig6C_2<-ggplot(aGVHD_brm_im, aes(x = Estimate, y = Y)) +
      geom_point(size = 2) +  
      geom_errorbarh(aes(xmin = l.95..CI, xmax = u.95..CI), height = 0.2, color = "black") +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  
      coord_cartesian(clip = "off") +  
      theme_classic() +
      labs(x = "Effect Size (Estimate)", y = "") +
      theme(
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.border = element_rect(fill = NA, color = "black", size = unit(0.2, "mm")),  
        plot.title = element_blank()
      )
    Fig6C_2
######################################################################################## 
#boxplot for immune cell 
    sorted_levels <- aGVHD_brm_im$X[order(aGVHD_brm_im$Estimate, decreasing = FALSE)]
    dt.response.boxplot_long_im$variable <- factor(dt.response.boxplot_long_im$variable, levels = sorted_levels)
    dt.response.boxplot_long_im$Target<-as.factor(dt.response.boxplot_long_im$Target)
    Fig6D_2 <- ggplot(dt.response.boxplot_long_im, 
                          aes(x = variable, 
                              y = value, 
                              color = Target)) +
      coord_flip() +
      geom_boxplot(outlier.shape = NA, linewidth = 0.4, alpha = 0.5,
                   position = position_dodge(width = 0.75)) +
      geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
                  size = 0.5, alpha = 0.6) +
      stat_compare_means(method = "wilcox.test", 
                         label = "p.signif", 
                         hide.ns = TRUE, 
                         size = 3.5) +
      scale_color_manual(values = c("0" = "#51C3CC", "1" = "#CC5800")) + # "0" represents "NO" "1" represents "Yes"
      labs(x =NULL, y =  "Immune cell counts") +
      theme_minimal(base_size = 8) +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.line = element_line(color = "black", size = unit(0.2, "mm")),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    Fig6D_2
######################################################################################## 
#combine all plots
    Fig6C_1 <- Fig6C_1 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
    Fig6E <- Fig6E + theme(plot.margin = unit(rep(0, 4), "cm"))
    Fig6D_1 <- Fig6D_1 + theme(plot.margin = unit(rep(0, 4), "cm"))
    g1 <- ggplotGrob(Fig6C_1)
    g2 <- ggplotGrob(Fig6E)
    g3 <- ggplotGrob(Fig6D_1)
    max_height <- unit.pmax(g1$heights, g2$heights, g3$heights)
    g1$heights <- max_height
    g2$heights <- max_height
    g3$heights <- max_height
    Fig6C_2 <- Fig6C_2 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
    Fig6D_2 <- Fig6D_2 + theme(plot.margin = unit(rep(0, 4), "cm"))
    g1_im <- ggplotGrob(Fig6C_2)
    g3_im <- ggplotGrob(Fig6D_2)
    max_height_im <- unit.pmax(g1_im$heights, g3_im$heights)
    g1_im$heights <- max_height_im
    g3_im$heights <- max_height_im
    max_widths <- unit.pmax(g1$widths, g1_im$widths)
    g1$widths <-  g1_im$widths <-  max_widths
    combined_plot <- arrangeGrob(g1, g3, g2,
                                 ncol = 3,
                                 widths = c(4.1, 1.1, 1.4),
                                 layout_matrix = rbind(c(1, 2, 3)))
    combined_plot_im <- arrangeGrob(g1_im, g3_im,
                                    ncol = 3,
                                    widths = c(4.1, 1.1, 1.4),
                                    layout_matrix = rbind(c(1, 2, 3)))
    combined_plot_all <- grid.arrange(combined_plot, combined_plot_im,
                                      nrow = 2,
                                      heights = c(2, 0.8))