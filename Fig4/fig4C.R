###############This is data for Fig 4C#######
rm(list=ls()) 
library(dplyr)
library(reshape2)
library(ggplot2)
load("Fig4.RData")
#####################################################################################
# pick a nice color palette
    my_col <- colorRampPalette(c("white","#ffffe0", "#c7e9b9","#c7e9b1", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494"))(100)

#figure generation
    Fig4C <- ggplot(dat_upper, aes(x = Var2, y = Var1, fill = Correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
      geom_text(aes(label = sig_label), vjust = -0.2, color = "black", size = 3) +
      scale_fill_gradientn(colors = my_col, limits = c(0, 0.8), name = "Correlation") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 14, face = "bold")
      ) +
      coord_fixed()
    Fig4C