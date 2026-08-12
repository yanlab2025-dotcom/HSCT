###############This is data for Fig 1E####
rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggridges)
load("Fig1.RData")
##############################################################################
#data input preparation
    colors <- c(
      "Meropenem" = "#86b3d1",
      "Caspofungin" = "#4e79a7",
      "Tigecycline" = "#8db8a5",
      "Teicoplanin" = "#668B8B",
      "Cefoperazone-Sulbactam" = "#66c2a5",
      "Cefotaxime-Sulbactam" = "#abdda4",
      "Ampicillin-Sulbactam" = "#edc948",
      "Posaconazole" = "#8B658B",
      "Polymixin E" = "#fee08b",
      "Amphotericin B"= "#D55E00"
    )
    usage_counts_filtered$Antibiotic_1 <- factor(usage_counts_filtered$Antibiotic_1, levels = names(colors))
    data <- usage_counts_filtered %>%
      group_by(Antibiotic_1) %>%
      mutate(density = count / max(count))
    data <- usage_counts_filtered %>%
      arrange(Antibiotic_1, time)

#figure generation
    Fig1E <- ggplot(data, aes(x = time, y = Antibiotic_1, height = count, fill = Antibiotic_1)) +
      geom_density_ridges(stat = "identity", scale = 1.5, alpha = 0.7) +
      scale_fill_manual(values = colors) + 
      labs(x = "Time(days)") +
      theme_classic() +
      theme(legend.position = "none",
            axis.title.x = element_text(size = 8,color="black"),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 8,color="black"),
            axis.text.y = element_text(size = 8,color="black",angle = 50,hjust = 1),
            axis.line = element_line(size = 0.33),
            axis.ticks.length = unit(0.1, "cm"),
            axis.ticks = element_line(color = "black", size = 0.33))
    Fig1E