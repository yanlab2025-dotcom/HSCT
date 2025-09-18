###########This is data for Fig c_d#####################
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
aGVHD_brm$Feature <- factor(aGVHD_brm$Feature, levels = aGVHD_brm$Feature[order(aGVHD_brm$Estimate, decreasing = FALSE)])
Fig6c_1 <- ggplot(aGVHD_brm, aes(x = Estimate, y = Feature)) +
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
    Fig6c_1
########################################################################################
dt.response.boxplot_long$PFS <- ifelse(dt.response.boxplot_long$PFS == "NO", "0",
                                       ifelse(dt.response.boxplot_long$PFS == "YES", "1",
                                              dt.response.boxplot_long$PFS))    
    
dt.response.boxplot_long$variable <- as.character(dt.response.boxplot_long$variable)
sorted_levels <- aGVHD_brm$Feature[order(aGVHD_brm$Estimate, decreasing = FALSE)]
dt.response.boxplot_long$variable <- factor(dt.response.boxplot_long$variable, levels = sorted_levels)
dt.response.boxplot_long$PFS<-as.factor(dt.response.boxplot_long$PFS)
dt.response.boxplot_long <- dt.response.boxplot_long %>%
    mutate(rel_abundance_for_FC = if ("sqrtAbundance" %in% names(.)) sqrtAbundance else (sqrtAbundance)^2)
fc_tbl <- dt.response.boxplot_long %>%
      group_by(variable, PFS) %>%
      summarize(med = median(rel_abundance_for_FC, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = PFS, values_from = med, names_prefix = "grp_") %>%
      mutate(grp_0 = ifelse(is.na(grp_0), 0, grp_0),
           grp_1 = ifelse(is.na(grp_1), 0, grp_1)) %>%
      mutate(FC = ifelse(grp_0 == 0 & grp_1 == 0, 1,
                       ifelse(grp_0 == 0, Inf, grp_1 / grp_0)),
           log2FC = ifelse(is.finite(FC) & FC > 0, log2(FC),
                           NA_real_))
  
fc_tbl<-as.data.frame(fc_tbl)
fc_tbl <- fc_tbl %>%
      mutate(fc_centered = FC - 1)  
    fc_tbl$variable <- factor(fc_tbl$variable, levels = sorted_levels)
    
wilcox_test_results <- dt.response.boxplot_long %>%
      group_by(variable) %>%
      summarise(
        p_value = wilcox.test(rel_abundance_for_FC ~ PFS)$p.value
      ) %>%
      mutate(
        significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
fc_tbl <- fc_tbl %>%
      left_join(wilcox_test_results, by = "variable")
Fig6d_1 <- ggplot(fc_tbl, aes(x = fc_centered, y = variable, fill = FC)) +
      geom_segment(aes(x = fc_centered, xend = fc_centered, y = variable),
                   linetype = "dotted", color = "gray30", linewidth = 0.4) +
      geom_col(width = 0.6) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
      
      geom_text(aes(x = ifelse(fc_centered >= 0, fc_centered + 0.1, fc_centered - 0.1),
                    label = significance),
                size = 4, color = "black", vjust = 0.7) +
      
      scale_x_continuous(
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = c("0", "0.5", "1", "1.5", "2")
      ) +
      scale_fill_gradientn(
        colors = c("#51C3CC", "#bababa", "#CC5800"),
        limits = c(0, 2),
        breaks = c(0, 0.5, 1, 1.5, 2)
      ) +
      theme_classic() +
      labs(
        x = "Fold Change", 
        y = NULL, 
        title = NULL
      ) +
      theme(
        legend.position = "none",
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.line = element_line(color = "black", size = unit(0.2, "mm")),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    
    Fig6d_1

######################################################################################## 
#combine all plots

 Fig6c_1 <- Fig6c_1 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
 Fig6d_1 <- Fig6d_1 + theme(plot.margin = unit(rep(0, 4), "cm"))
 g1 <- ggplotGrob(Fig6c_1)
 g2 <- ggplotGrob(Fig6d_1)
 max_height <- unit.pmax(g1$heights,g2$heights)
 g1$heights <- max_height
 g2$heights <- max_height
 Fig6c_1 <- Fig6c_1 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
 Fig6d_1 <- Fig6d_1 + theme(plot.margin = unit(rep(0, 4), "cm"))
 combined_plot <- grid.arrange(g1, g2,
                             ncol = 2,
                             widths = c(4.1, 1.1),
                             layout_matrix = rbind(c(1, 2)))






           
