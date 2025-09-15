###############This is data for Fig 2e_h####
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
load("Fig2.RData")
#######################################################################################
#data input preparation and transformation
    metadata$aGVHD[metadata$aGVHD == 2] <- 1 #1 and 2 represent grade I and grade II of aGvHD
    metadata <- metadata[metadata$In_out == "Out", ] #"Out" represents "Post-HSCT"
    rownames(metadata)<-metadata$number
    metadata$Infection<-metadata$infection_all
    
    metagenome_OUT <- metagenome_OUT[rownames(metadata), ]
    
    dist.metagenome_OUT <- vegdist(metagenome_OUT , method="bray")
    mp4_trans.pcoa <- cmdscale(dist.metagenome_OUT, eig = TRUE, k = 2) 
    variance <- head(eigenvals(mp4_trans.pcoa)/sum(eigenvals(mp4_trans.pcoa)))
    pco1 <- as.integer(variance[1] * 100 )
    pco2 <- as.integer(variance[2] * 100 )
    mp4_trans.pcoa.scores <- as.data.frame( scores(mp4_trans.pcoa, display = "sites") )
    mp4.score <- merge(mp4_trans.pcoa.scores, metadata, by.x="row.names",by.y="row.names") 
    row.names(mp4.score) <- mp4.score[,1]
    mp4.score <- mp4.score[,-1]
    n <- ncol(t(mp4.score))
    otus <- nrow(t(metagenome_OUT))
##################################################
#adding metadata
#1 and 2 represent grade I and grade II of EBV/TMA/CMV
    binary_vars <- c("EBV", "TMA", "CMV")
    for (var in binary_vars) {
      metadata[[var]][metadata[[var]] == 2] <- 1
      mp4.score[[var]][mp4.score[[var]] == 2] <- 1
    }
    yes_no_vars <- c("Infection", "TMA", "CMV", "EBV", "aGVHD", "PFS", "death")
    for (var in yes_no_vars) {
      mp4.score[[var]] <- factor(ifelse(mp4.score[[var]] == 1, "Yes", "No"),
                                 levels = c("No", "Yes"))
      metadata[[var]] <- factor(ifelse(metadata[[var]] == 1, "Yes", "No"),
                                levels = c("No", "Yes"))
    }
    metadata$AUS_group <- factor(metadata$AUS_group, levels = c("low_score", "high_score"))
    
    col_AUS <- setNames(c("#93C6E1FF","#08306b"),c("low_score", "high_score"))
    col_infection <- setNames(c("#A4BED5FF","#453947FF"),c("No", "Yes"))
    col_TMA <- setNames(c("#C5EFE8","#5E8F91"), c("No", "Yes"))
    col_CMV <- setNames(c("#FFC4B8","#D4785F"),c("No", "Yes"))
    col_EBV <- setNames(c("#E7BCA4","#9A756D"), c("No", "Yes"))
    col_aGVHD <- setNames(c("#9ecae1","#ec7014"), c("No", "Yes"))
    col_PFS <- setNames(c("#51C3CC","#CC5800"),c("No", "Yes"))
    col_Death <- setNames(c("#5254A3","#AD494A"), c("No", "Yes"))
    
    variables <- c("AUS_group","aGVHD","EBV","TMA")
    color_list <- list(
      AUS_group = col_AUS,
      aGVHD = col_aGVHD,
      EBV = col_EBV,
      TMA = col_TMA
    )

#PCoA generation    
    pcoa_plots <- list()
    for (var in variables) {
      formula_str <- as.formula(paste0("dist.metagenome_OUT ~ ", var))
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
    Fig2e_h <- plot_grid(plotlist = pcoa_plots, ncol = 2, align = "hv")

    Fig2e_h
