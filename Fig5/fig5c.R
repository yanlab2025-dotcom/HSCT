###########This is data for Fig 5c###############
rm(list = ls())
library(ggplot2)   
library(dplyr)      
library(vegan)     
library(ggpubr)     
library(patchwork)  
library(scales)     
load("Fig5.RData")
############################################################################
# pick a color palette
    gradient_colors <- colorRampPalette(c("#232268", "#218029", "yellow"))

# PCoA plot
    pcoa_plot <- ggplot(mp4.score, aes(x = Dim1, y = Dim2,fill=Prevotella_melaninogenica)) +
      geom_point(size =3.5, alpha = 0.75,shape =21) +
      scale_fill_viridis(name = "P_M") +
      ggtitle(paste0("PCOA")) +
      theme_classic() +
      xlab(paste0("PCo1 (", pco1, "%)")) +  
      ylab(paste0("PCo2 (", pco2, "%)"))+
      theme()
    pcoa_plot
#############################################################################
# stacked bar plots 
    filtered_data$Category <- factor(filtered_data[[3]], 
                                     levels = c("AUS", "f__Prevotellaceae",
                                                "Prevotella_melaninogenica",
                                                "f__Micrococcaceae", "f__Burkholderiaceae"))
    plot_list <- filtered_data %>%
      group_split(Category) %>%
      lapply(function(df) {
        cat_name <- unique(df$Category)
        p <- ggplot(df, aes(x = as.factor(Dim1), y = Abundance, fill = Category)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = c(
            "f__Prevotellaceae" = "#4d7558",
            "Prevotella_melaninogenica"="#88AC90",
            "f__Burkholderiaceae" = "#ffbb28",
            "f__Micrococcaceae" = "#6c6cb0"
          )) +
          theme_minimal() +
          theme(
            panel.grid = element_blank(),
            axis.line = element_line(colour = "black", size =0.25),  
            axis.title.y = element_text(angle = 0),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            strip.text = element_text(face = "bold", hjust = 0.5),
            plot.title = element_text(hjust = 1, face = "bold", size = 10)
          ) +
          labs(x = NULL, y = cat_name, title = NULL)
        if (cat_name == "AUS") {
          p <- p + ylim(0, 3000)
        } else if 
        (cat_name == "Prevotella_melaninogenica") {
          p <- p + ylim(0, 0.1)
        } else
        {
          p <- p + ylim(0, 1)
        }
        return(p)
      })

# combine stacked bar plots
    p <- wrap_plots(plotlist = plot_list, ncol = 1)
    print(p)

   Fig5c<- pcoa_plot / p  
   Fig5c
