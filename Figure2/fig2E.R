###############This is data for Fig 2E####
rm(list = ls()) 
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(patchwork)
load("fig2E.RData")
#####################DNA  SGB
centroids_1 <- plotdata_1 %>%
  group_by(Group) %>%
  summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop")
col_palette <- c(
  "Control" = "#9E9E9F",
  "Levofloxacin" = "#ADD8E6",
  "Meropenem" = "#E19576",
  "Tigecycline" = "#FFBFCA"
)
##################################################
#figure generation
p1 <- ggplot(plotdata_1, aes(x = dim1, y = dim2, colour = Group)) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +
  geom_point(data = centroids_1, aes(x = dim1, y = dim2, fill = Group), 
             size = 3.5, shape = 25, colour = "black", stroke = 0.8) +
  scale_colour_manual(values = col_palette) +
  scale_fill_manual(values = col_palette) +
  xlab(paste0("PCo1 (", pc1, "%)")) +
  ylab(paste0("PCo2 (", pc2, "%)")) +
  ggtitle("MGX-SGB") + 
  theme_classic() +
  theme(
    axis.line.x = element_line(colour = 'black', linewidth = 0.5),
    axis.line.y = element_line(colour = 'black', linewidth = 0.5),
    plot.title = element_text(size = 12, hjust = 0, face = "plain"),
    axis.title = element_text(size = 12),
    legend.position = "none", 
    axis.ticks = element_blank(), 
    axis.text = element_blank()  
  ) +
  annotate("label", 
           x = min(plotdata_1$dim1), 
           y = min(plotdata_1$dim2), 
           label = paste0("p = ", p_value, "\nR² = ", round(r2_value, 2)),
           size = 3.5, 
           hjust = 0, 
           vjust = 0,
           label.size = 0.5, 
           fill = "white",
           family = "sans") 
print(p1)

###################################DNA  PWY
centroids_2 <- plotdata_2 %>%
  group_by(Group) %>%
  summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop")
##################################################
#figure generation
p2 <- ggplot(plotdata_2, aes(x = dim1, y = dim2, colour = Group)) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +
  geom_point(data = centroids_2, aes(x = dim1, y = dim2, fill = Group), 
             size = 3.5, shape = 25, colour = "black", stroke = 0.8) +
  scale_colour_manual(values = col_palette) +
  scale_fill_manual(values = col_palette) +
  xlab(paste0("PCo1 (", pc1, "%)")) +
  ylab(paste0("PCo2 (", pc2, "%)")) +
  ggtitle("MGX-PWY") + 
  theme_classic() +
  theme(
    axis.line.x = element_line(colour = 'black', linewidth = 0.5),
    axis.line.y = element_line(colour = 'black', linewidth = 0.5),
    plot.title = element_text(size = 12, hjust = 0, face = "plain"),
    axis.title = element_text(size = 12),
    legend.position = "none",
    axis.ticks = element_blank(), 
    axis.text = element_blank()  
  ) +
  annotate("label", 
           x = min(plotdata_2$dim1), 
           y = min(plotdata_2$dim2), 
           label = paste0("p = ", p_value, "\nR² = ", round(r2_value, 2)),
           size = 3.5, 
           hjust = 0, 
           vjust = 0,
           label.size = 0.5, 
           fill = "white",
           family = "sans") 
print(p2)

################################RNA  SGB
centroids_3 <- plotdata_3 %>%
  group_by(Group) %>%
  summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop")
##################################################
#figure generation
p3 <- ggplot(plotdata_3, aes(x = dim1, y = dim2, colour = Group)) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +
  geom_point(data = centroids_3, aes(x = dim1, y = dim2, fill = Group), 
             size = 3.5, shape = 25, colour = "black", stroke = 0.8) +
  scale_colour_manual(values = col_palette) +
  scale_fill_manual(values = col_palette) +
  xlab(paste0("PCo1 (", pc1, "%)")) +
  ylab(paste0("PCo2 (", pc2, "%)")) +
  ggtitle("MTX-SGB") + 
  theme_classic() +
  theme(
    axis.line.x = element_line(colour = 'black', linewidth = 0.5),
    axis.line.y = element_line(colour = 'black', linewidth = 0.5),
    plot.title = element_text(size = 12, hjust = 0, face = "plain"),
    axis.title = element_text(size = 12),
    legend.position = "none", 
    axis.ticks = element_blank(), 
    axis.text = element_blank()  
  ) +
  annotate("label", 
           x = min(plotdata_3$dim1), 
           y = min(plotdata_3$dim2), 
           label = paste0("p = ", p_value, "\nR² = ", round(r2_value, 2)),
           size = 3.5, 
           hjust = 0, 
           vjust = 0,
           label.size = 0.5,
           fill = "white",
           family = "sans") 
print(p3)

##############################RNA PWY
centroids_4 <- plotdata_4 %>%
  group_by(Group) %>%
  summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop")
##################################################
#figure generation
p4 <- ggplot(plotdata_4, aes(x = dim1, y = dim2, colour = Group)) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +
  geom_point(data = centroids_4, aes(x = dim1, y = dim2, fill = Group), 
             size = 3.5, shape = 25, colour = "black", stroke = 0.8) +
  scale_colour_manual(values = col_palette) +
  scale_fill_manual(values = col_palette) +
  xlab(paste0("PCo1 (", pc1, "%)")) +
  ylab(paste0("PCo2 (", pc2, "%)")) +
  ggtitle("MTX-PWY") + 
  theme_classic() +
  theme(
    axis.line.x = element_line(colour = 'black', linewidth = 0.5),
    axis.line.y = element_line(colour = 'black', linewidth = 0.5),
    plot.title = element_text(size = 12, hjust = 0, face = "plain"),
    axis.title = element_text(size = 12),
    legend.position = "none", 
    axis.ticks = element_blank(), 
    axis.text = element_blank() 
  ) +
  annotate("label", 
           x = min(plotdata_4$dim1), 
           y = min(plotdata_4$dim2), 
           label = paste0("p = ", p_value, "\nR² = ", round(r2_value, 2)),
           size = 3.5, 
           hjust = 0, 
           vjust = 0,
           label.size = 0.5, 
           fill = "white",
           family = "sans") 
print(p4)

p<- p1+p2+p3+p4
p

