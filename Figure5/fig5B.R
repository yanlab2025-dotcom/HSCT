###############This is data for Fig 5B####
rm(list = ls()) 
library(ggplot2)
library(ggh4x)
library(scales)
library(dplyr)
library(patchwork)  
load("fig5B.RData")
##################################################
# Set species order for plotting
target_species_order <- c(
  "Prevotella_melaninogenica",
  "Fusobacterium_pseudoperiodonticum",
  "Prevotella_scopos",
  "Prevotella_jejuni",
  "Leptotrichia_wadei"
)
##################################################
# Split data into ALL and Low subsets
plot_data$Species <- factor(plot_data$Species, levels = rev(target_species_order))
plot_data_all <-  plot_data[plot_data$sample == "ALL", ]
plot_data_low <- plot_data[plot_data$sample == "Low", ]
paired_df <- merge(plot_data_all, plot_data_low, 
                   by = c("Species", "Cell"), suffixes = c("_all", "_low"))
paired_df$opposite_signif <- with(paired_df, 
                                  Signif_all %in% c("Positive", "Negative") &
                                    Signif_low %in% c("Positive", "Negative"))
highlight_opposite <- paired_df %>% filter(opposite_signif) %>% select(Species, Cell)

plot_data_all$SizeGroup <- cut(abs(plot_data_all$Cor),
                               breaks = c(-Inf, 0.1, 0.2, Inf),
                               labels = c("0.1", "0.2", "0.3"))

plot_data_low$SizeGroup <- cut(abs(plot_data_low$Cor),
                               breaks = c(-Inf, 0.1, 0.2, Inf),
                               labels = c("0.1", "0.2", "0.3"))

plot_data_all$PLabel <- ifelse(plot_data_all$P < 0.01, "*",
                              ifelse(plot_data_all$P < 0.05, ".", ""))
plot_data_low$PLabel <- ifelse(plot_data_low$P < 0.01, "*",
                              ifelse(plot_data_low$P < 0.05, ".", ""))
inflammation_markers <- c("IL_6", "IL_10", "IL_4", "CRP", "IgM")
plot_data_all$Group <- ifelse(plot_data_all$Cell %in% inflammation_markers, "ALL Inflammatory", "ALL Immune")
plot_data_low$Group <- ifelse(plot_data_low$Cell %in% inflammation_markers, "Low Inflammatory", "Low Immune")
plot_data_all$Group <- factor(plot_data_all$Group, levels = c("ALL Inflammatory", "ALL Immune"))
plot_data_low$Group <- factor(plot_data_low$Group, levels = c("Low Inflammatory", "Low Immune"))
plot_data_all$Cor <- pmax(pmin(plot_data_all$Cor, 0.25), -0.25)
plot_data_low$Cor <- pmax(pmin(plot_data_low$Cor, 0.25), -0.25)
##################################################
#figure generation
p1 <- plot_dot_effectsize(plot_data_all) + 
  theme(axis.text.x = element_blank())
p2 <- plot_dot_effectsize(plot_data_low) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
combined_plot <- (p1 / p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")
combined_plot
