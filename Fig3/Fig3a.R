###############This is data for Fig 3a###################
rm(list = ls())
library(ggplot2)
library(vegan)
library(tidyr)
library(ggpubr)
library(ggalt)
library(gridExtra)
library(philentropy)
library(fpc)
library(ggsankey)
load("Fig3.RData")
#########################################################################
#data input preparation
    desired_order <- c("Rothia", "Prevotella", "Streptococcus", "Lautropia","Burkholderia")
    desired_order_1 <- c("Rothia","Lautropia","Burkholderia")
    qqq$node <- factor(qqq$node, levels = rev(desired_order))
    qqq$next_node <- factor(qqq$next_node, levels = rev(desired_order_1))
    qqq <- qqq %>%
      mutate(
        x = factor(x, levels = c("Pre-HSCT", "Post-HSCT")),
        next_x = factor(next_x, levels = c("Pre-HSCT", "Post-HSCT"))
      )

#figure generation
    Fig3a <- ggplot(qqq, aes(x = x, 
                    next_x = next_x, 
                    node = node, 
                    next_node = next_node,  
                    fill = factor(node), 
                    value = value)) +
    geom_sankey(flow.alpha = 0.7, node.color = "gray") +
    geom_sankey_text(aes(label = node), size = 3, color = "black") +
    theme_minimal() +
    scale_fill_manual(values = c(
        "Streptococcus" = "#5454c7", #cluster label
        "Rothia" = "#ff9f42", #cluster label
        "Lautropia" = "#4e9e9e", #cluster label
        "Prevotella" = "#44644a", #cluster label
        "Burkholderia" = "#ff1f00" #cluster label
    )) +
    labs(fill = "Cluster") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          legend.key.size = unit(0.5, "cm")) +
    theme(panel.grid = element_blank()) +
    theme(legend.justification = "left", legend.position = "right")
    Fig3a
