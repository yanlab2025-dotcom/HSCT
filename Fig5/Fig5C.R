############### This is data for Fig 5C ###############

library(ggplot2)
library(showtext)
library(patchwork)
load("Fig5C.RData")

##################################################
for (species in species_subset) {
  for (cell in cell_subset) {
    df <- data.frame(
      Abundance = ALL[[species]],
      CellValue = ALL[[cell]],
      death =ALL$death
    )
    cor_test_0 <- cor.test(
      subset(df, death == 0)$Abundance,
      subset(df, death == 0)$CellValue,
      method = "spearman",
      exact = FALSE
    )
    cor_test_1 <- cor.test(
      subset(df, death == 1)$Abundance,
      subset(df, death == 1)$CellValue,
      method = "spearman",
      exact = FALSE
    )
    R_val_0 <- round(cor_test_0$estimate, 2)
    P_val_0 <- signif(cor_test_0$p.value, 3)
    R_val_1 <- round(cor_test_1$estimate, 2)
    P_val_1 <- signif(cor_test_1$p.value, 3)
    #if (cor_test$p.value < 0.05 && abs(cor_test$estimate) > 0.25) {
    if (cor_test_0$p.value < 1) {
      df$death<-as.factor(df$death)
      p <- ggplot(df, aes(x = Abundance, y = CellValue, colour = death)) +
        scale_y_log10() +
        geom_smooth(method = "lm", se = TRUE) +
        scale_color_manual(values = c("#5254a3bf", "#ad494abf")) +
        annotate(
          "text", x = Inf, y = Inf, 
          label = paste0("PFS:No R = ", R_val_0," P = ",P_val_0),
          hjust = 1.1, vjust = 1.5, 
          size = 3.2, fontface = "italic", family = "Arial" 
        ) +
        annotate(
          "text", x = Inf,y = max(df$CellValue)-1000, 
          label = paste0("PFS:Yes R = ", R_val_1," P = ",P_val_1),
          hjust = 1.1, vjust = 1.5, 
          size = 3.2, fontface = "italic", family = "Arial" 
        ) +
        geom_point(
          alpha = 0.75, size = 3.5, 
          aes(shape = ifelse(Abundance == 0 | CellValue == 0, "zero", "nonzero"))
        ) +
        scale_shape_manual(values = c(zero = 3, nonzero = 16)) +
        guides(shape = "none") +
        labs(
          y = NULL,
          title = unique(cell)
        ) +
        theme_classic() +
        theme(
          text = element_text(family = "Arial", colour = "black"),  
          plot.title = element_text(size = 10),  
          axis.text  = element_text(size = 8, colour = "black"),
          axis.line.x = element_line(colour = "black", size = 0.72),
          axis.line.y = element_line(colour = "black", size = 0.72),
          legend.position = "none"
        )
      plot_list[[paste(species, cell, sep = "_")]] <- p
    }
  }
}

p<-wrap_plots(plot_list, ncol = 3)
p
