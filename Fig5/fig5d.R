#############This is data for Fig 5d###############
rm(list = ls())
library(ggplot2)
library(showtext)
library(patchwork)
load("Fig5.RData")
#############################################################################
#data input preparation
      font_add("Arial", "arial.ttf")  
      showtext_auto()
      plot_list <- list()
      species <- "Prevotella_melaninogenica"
      cell_subset <- c("T_cells","CD4_central_memory_T_cells",
                       "CD8_central_memory_T_cells","CD8_effector_memory_T_cells")

#############################################################################
# generate correlation plots
      for (cell in cell_subset) {
      df <- data.frame(
        Abundance = LOW_AUS_patients[[species]],
        CellValue = LOW_AUS_patients[[cell]],
        death =LOW_AUS_patients$PFS
      )
      cor_test <- cor.test(df$Abundance, df$CellValue, method = "spearman", exact = FALSE)
      R_val <- round(cor_test$estimate, 2)
      P_val <- signif(cor_test$p.value, 3)
      if (cor_test$p.value < 1) {
        df$death<-as.factor(df$death)
        Fig5D <- ggplot(df, aes(x = Abundance, y = CellValue, colour = death)) +
          scale_y_log10() +
          geom_smooth(method = "lm", se = TRUE) +
          scale_color_manual(values = c("#0072B2", "#D55E00")) +
          annotate(
            "text", x = Inf, y = Inf, 
            label = paste0("R = ", R_val),
            hjust = 1.1, vjust = 1.2, 
            size = 3.2, fontface = "italic", family = "Arial" 
          ) +
          geom_point(
            alpha = 0.75, size = 2.5, 
            aes(shape = ifelse(Abundance == 0 | CellValue == 0, "zero", "nonzero"))
          ) +
          scale_shape_manual(values = c(zero = 3, nonzero = 16)) +
          guides(shape = "none") +
          labs(
            y = NULL,
            x=" Relative abundance of P. melaninogenica",
            title = unique(cell)
          ) +
          theme_classic() +
          theme(
            text = element_text(family = "Arial",colour = "black"),  
            plot.title = element_text(size = 10),  
            axis.text  = element_text(size = 8,colour = "black"),
            axis.line.x = element_line(colour = "black", size = 0.72),
            axis.line.y = element_line(colour = "black", size = 0.72),
            legend.position = "none"
          )
        plot_list[[paste(species, cell, sep = "_")]] <- Fig5D
         }
      }
      Fig5d <- wrap_plots(plot_list, ncol = 2)

      Fig5d
