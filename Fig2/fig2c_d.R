###############This is data for fig 2c_d####
rm(list = ls())
library(Rtsne)
library(ggplot2)
library(vegan)  
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(ggsignif)
library(ggpubr)
library(ggbreak)
library(ggalt)
library(gridExtra)
library(cowplot) 
load("fig2.RData")
#######################################################################################
#data input preparation and transformation
    metadata <- metadata[metadata$Sampling == "pre-HSCT", ] 
    rownames(metadata)<-metadata$SampleID
    metadata$Infection<-metadata$infection_all
    metagenome <- metagenome[rownames(metadata), ]
    
    dist.metagenome <- vegdist(metagenome , method="bray")
    mp4_trans.pcoa <- cmdscale(dist.metagenome, eig = TRUE, k = 2) 
    variance <- head(eigenvals(mp4_trans.pcoa)/sum(eigenvals(mp4_trans.pcoa)))
    pco1 <- as.integer(variance[1] * 100 )
    pco2 <- as.integer(variance[2] * 100 )
    mp4_trans.pcoa.scores <- as.data.frame( scores(mp4_trans.pcoa, display = "sites") )
    mp4.score <- merge(mp4_trans.pcoa.scores, metadata, by.x="row.names",by.y="row.names") 
    row.names(mp4.score) <- mp4.score[,1]
    mp4.score <- mp4.score[,-1]
    n <- ncol(t(mp4.score))
    otus <- nrow(t(metagenome))
##################################################
#adding metadata
    metadata$AUS_group <- factor(metadata$AUS_group, levels = c("low_score", "high_score"))
    metadata$Infection <- factor(metadata$Infection, levels = c("No", "Yes"))
    col_AUS <- setNames(c("#93C6E1FF","#08306b"),c("low_score", "high_score"))
    col_infection <- setNames(c("#A4BED5FF","#453947FF"),c("No", "Yes"))
#PCoA generation
    variables <- c("Infection","AUS_group")
    color_list <- list(
      Infection = col_infection,
      AUS_group = col_AUS
    )
    pcoa_plots <- list()
    for (var in variables) {
      formula_str <- as.formula(paste0("dist.metagenome ~ ", var))
      adonis_result <- adonis2(formula_str, data = metadata, permutations = 999)
      R2 <- round(adonis_result$R2[1], 4)
      pvalue <- formatC(adonis_result$`Pr(>F)`[1])
      centroids <- mp4.score %>%
        group_by(.data[[var]]) %>%
        summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
        rename(group = 1)
      mp4_with_centroids <- mp4.score %>%
        rename(group = all_of(var)) %>%
        left_join(centroids, by = "group", suffix = c("_sample", "_centroid"))
      p <- ggplot(mp4.score, aes(x = Dim1, y = Dim2, colour = .data[[var]])) +
        scale_colour_manual(values = color_list[[var]]) +
        theme_classic() +
        theme(
          axis.line.x = element_line(colour = 'black', linewidth = 0.25),
          axis.line.y = element_line(colour = 'black', linewidth = 0.25),
          axis.title = element_text(size = 10), 
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.position = "top",                  
          legend.direction = "horizontal",         
          legend.justification = "center",          
          legend.box = "horizontal",
          axis.ticks = element_blank(), 
          axis.text = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5)
        ) +
        xlab(paste0("PCo1 (", pco1, "%)")) +
        ylab(paste0("PCo2 (", pco2, "%)")) +
        guides(colour = guide_legend(
          title = paste0("R² = ", round(R2, 4),
                         "   |   P = ", pvalue),
          title.position = "top",    
          title.theme = element_text(size = 9, face = "plain", hjust = 0.5)
        )) +
        geom_point(size = 2.5, alpha = 0.75,shape=16) +
        geom_point(data = centroids, 
                   aes(x = Dim1, y = Dim2), 
                   size = 2.5, shape = 21, 
                   fill = color_list[[var]][centroids$group], 
                   color = "black", stroke = 0.8) +
        ggtitle(var)
      pcoa_plots[[var]] <- p
    }
    fig2c_d <- plot_grid(plotlist = pcoa_plots, ncol = 3, align = "hv")

    fig2c_d
