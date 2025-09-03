######This plot shows the expression of B cells across Pre-HSCT clusters####
rm(list = ls())
library(tidyverse)
library(cowplot)
library(smplot2)
load("Fig3.RData")
###############################################################################
#data input preparation
    custom_colors <- c(
      "Streptococcus" = alpha("#5454c7", 0.75),
      "Lautropia" = alpha("#4e9e9e", 0.75),
      "Rothia" = alpha("#ff9f42", 0.75),
      "Prevotella" = alpha("#44644a", 0.75)
    )
    theme_bw <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                          base_rect_size = base_size/22) 
    {
      theme_grey(base_size = base_size, base_family = base_family, 
                 base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
        theme(panel.background = element_rect(fill = "white", 
                                              colour = NA), 
              panel.border = element_rect(fill = NA, colour = "black"), 
              panel.grid = element_line(colour = "grey92"), 
              panel.grid.minor = element_line(linewidth = rel(0.66)), 
              strip.background = element_rect(fill = "grey85", 
                                              colour = "black",
                                              size = 0.66), complete = TRUE)
    }
    b_data <- immune_long %>% 
      filter(Cell_Type == "B_cells")
    b_data$Cluster <- factor(b_data$Cluster, levels = c("Rothia", "Prevotella","Streptococcus", "Lautropia"))
    clusters <- c("Rothia", "Prevotella", "Streptococcus", "Lautropia")
    comparisons <- list(
      c("Rothia", "Prevotella"), 
      c("Prevotella", "Streptococcus") ,
      c("Prevotella", "Lautropia")
    )

#figure generation
    Fig3E_B_cell <- ggplot(b_data, aes(x = Cluster, y = Count, fill = Cluster)) +
      sm_raincloud(
        boxplot.params = list(outlier.shape = NA, width = 0.7),
        point.params = list(alpha = 0.8, size = 3, shape = 21,stroke = 0.1),  
        violin.params = list(alpha = 0, color = NA, fill = NA),  
        sep_level = 4
      ) +
      stat_compare_means(comparisons = comparisons, label = "p.signif", method = "wilcox.test",size=2)+
      scale_fill_manual(values = custom_colors) +  
      theme_bw() +
      theme(
        axis.text.x  = element_text(size=12,color="black",angle = 30,hjust = 1),  
        legend.position = "none",
        axis.text.y = element_text(size=12,color="black"),  
        strip.text = element_text(size=12,color = "black"), 
        axis.title.x = element_text(size=12,color="black"), 
        axis.title.y = element_text(size=12,color="black"), 
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 0.66),
        axis.ticks = element_line(color = "black", size = 0.33),  
        axis.ticks.length = unit(0.1, "cm")
      )+labs(y = "Cell count") +   
      facet_wrap(~ Cell_Type, scales = "free_y") 
    Fig3E_B_cell
###############This plot shows the expression of CD8 effector T cells across Pre-HSCT clusters####
#data input preparation
    CD8_effector_T_cells <- immune_long %>% 
      filter(Cell_Type == "CD8_effector_T_cells")
    CD8_effector_T_cells$Cluster <- factor(CD8_effector_T_cells$Cluster, levels = c("Rothia", "Prevotella", "Streptococcus", "Lautropia"))
    clusters <- c("Rothia", "Prevotella", "Streptococcus", "Lautropia")
    comparisons <- list(
      c("Prevotella", "Streptococcus") ,
      c("Streptococcus", "Lautropia")
    )

#figure generation
    Fig3E_CD8_effector_T_cells <- ggplot(CD8_effector_T_cells, aes(x = Cluster, y = Count, fill = Cluster)) +
      sm_raincloud(
        boxplot.params = list(outlier.shape = NA, width = 0.7),
        point.params = list(alpha = 0.8, size = 3, shape = 21,stroke = 0.1),  
        violin.params = list(alpha = 0, color = NA, fill = NA),  
        sep_level = 4
      ) +
      stat_compare_means(comparisons = comparisons, label = "p.signif", method = "wilcox.test",size=2)+
      scale_fill_manual(values = custom_colors) + 
      theme_bw() +
      theme(
        axis.text.x  = element_text(size=12,color="black",angle = 30,hjust = 1),  
        legend.position = "none",
        axis.text.y = element_text(size=12,color="black"), 
        strip.text = element_text(size=12,color = "black"), 
        axis.title.x = element_text(size=12,color="black"), 
        axis.title.y = element_text(size=12,color="black"), 
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 0.66),
        axis.ticks = element_line(color = "black", size = 0.33),  
        axis.ticks.length = unit(0.1, "cm")
      )+labs(y = "Cell count") +   
      facet_wrap(~ Cell_Type, scales = "free_y") 
    Fig3E_CD8_effector_T_cells