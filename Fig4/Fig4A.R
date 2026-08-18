###############This is data for Fig 4A####
rm(list=ls()) 
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)
library(readxl)
load("fig4A.RData")
##################################################
#calculate mean R² per clinical group
mean_order_data <- combined_data %>%
  group_by(clinical) %>%
  summarise(mean_R2 = mean(R2, na.rm = TRUE)) %>%
  arrange(desc(mean_R2)) 
combined_data$clinical <- factor(combined_data$clinical, levels = rev(ordered_clinical))
##################################################
#figure generation
p_bar <- ggplot(data = combined_data, aes(x = clinical, y = R2, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + 
  geom_text(
    aes(
      label = stars, 
      vjust = ifelse(Group == "Pre", 0.65, 0.9)),
    position = position_dodge(width = 0.8), 
    hjust = -0.2,
    size = 3.5) +
  theme_bw() + 
  labs(x = NULL, y = "Variation explained (%)") + 
  coord_flip() + 
  scale_fill_manual(values = c("Pre_immune" = "#C77D88", "Pre" = "#9CD2FB", "Post" = "#55B0FA"),
                    breaks = c("Pre_immune","Pre","Post")) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    text = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black"),
    axis.line = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.background = element_blank()
  )
print(p_bar)
