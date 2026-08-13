###############This is data for Fig 2F####
rm(list = ls()) 
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(readxl)
library(tidyverse)
library(patchwork)
load("fig2F.RData")
#####################RNA SGB
plot_df_top_pos_SGB <- plot_df_top_pos %>%
  mutate(stars = case_when(
    padj < 0.1  ~ "*",   
    padj < 0.25 ~ ".",  
    TRUE        ~ ""    
  ))
color_palette <- c(
  "Control" = "#9E9E9F",
  "Levofloxacin" = "#ADD8E6",
  "Meropenem" = "#E19576",
  "Tigecycline" = "#FFBFCA",
  "None" = "grey95"
)
##################################################
#figure generation
p1 <- ggplot(plot_df_top_pos_SGB, aes(x = Type, y = Feature)) +
  geom_hline(yintercept = seq_along(levels(plot_df_top_pos_SGB$Feature)), 
             color = "gray95", linewidth = 0.4) +
  geom_point(aes(size = abs_log2FC, color = Direction)) +
  geom_text(aes(label = stars), color = "white", size = 10, vjust =  0.4, nudge_y = 0.05) +
  scale_color_manual(values = color_palette) +
  scale_size_continuous(range = c(5, 12)) + 
  theme_bw() +
  labs(
    x = NULL,
    y = NULL,
    size = "|log2FC|",
    color = "Enrichment"
  ) +
  theme(
    text = element_text(color = "black", size = 17),
    axis.text.x = element_text(color = "black", size = 17,  angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 17),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
print(p1)
#####################RNA PWY
plot_df_top_pos_PWY <- plot_df_top_pos_PWY %>%
  mutate(stars = case_when(
    padj < 0.1  ~ "*",  
    padj < 0.25 ~ ".",   
    TRUE        ~ ""    
  ))
##################################################
#figure generation
p2 <- ggplot(plot_df_top_pos_PWY, aes(x = Type, y = Feature)) +
  geom_hline(yintercept = seq_along(levels(plot_df_top_pos_PWY$Feature)), 
             color = "gray95", linewidth = 0.4) +
  geom_point(aes(size = abs_log2FC, color = Direction)) +
  geom_text(aes(label = stars), color = "white", size = 10, vjust = 0.4, nudge_y = 0.05) +
  scale_color_manual(values = color_palette) +
  scale_size_continuous(
    breaks = c(0, 4,8,12, 16),  
    limits = c(0, 17),         
    range = c(5, 12),           
    guide = guide_legend(override.aes = list(color = "black")) 
  ) +  
  theme_bw() +
  labs(
    x = NULL,
    y = NULL,
    size = "|log2FC|",
    color = "Enrichment"
  ) +
  theme(
    text = element_text(color = "black", size = 17),
    axis.text.x = element_text(color = "black", size = 17,  angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 17),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
print(p2)

p<- p1+p2
p

