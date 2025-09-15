###############This is data for Fig 2i####
rm(list = ls())
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(ggsignif)
library(ggpubr)
library(ggalt)
library(gridExtra)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(shadowtext)
library(purrr)
library(stringr)
library(ggnewscale)
load("Fig2.RData")
#######################################################################################
#data input preparation and transformation
    sparse_labeling <- function(x, y, minRadius) {
      stopifnot(length(x) == length(y))
      minRadiusSq <- minRadius^2
      label <- rep(F, length(x))
      for (i in seq_along(x)) {
        dSq <- (x[label] - x[i])^2 + (y[label] - y[i])^2
        label[i] <- !any(dSq < minRadiusSq)
      }
      return (label)
    }
    card_gene_annotation$AMR.Gene.Family <- as.factor(card_gene_annotation$AMR.Gene.Family)
    card_gene_annotation$Drug.Class <- as.factor(card_gene_annotation$Drug.Class)
    scores.bug.DNA$gene_id <- row.names(scores.bug.DNA)
    
    bugs2plot_annotation <- merge(scores.bug.DNA, card_gene_annotation, by.x= "gene_id", by.y= colnames(card_gene_annotation)[1])
    bugs2plot_annotation$shouldlbl <- sparse_labeling(bugs2plot_annotation$Dim1, bugs2plot_annotation$Dim2, 0.02)
    bugs2plot_annotation <- bugs2plot_annotation[
    bugs2plot_annotation$gene_id %in% rownames(bugs2plot),
    ]
##################################################
#adding metadata
    metadata <- left_join(metadata, IN_shannon, by = c("Subject_ID" = "PID"))
    rownames(metadata) <- metadata$number
    metadata <- metadata[rownames(card_matrix), ]
    adonis_result <- adonis2(
      card_matrix ~ AUS_group,
      data = metadata,
      method = "canberra",
      permutations = 999
    )
    metadata$Infection <- as.factor(metadata$infection_all)
    metadata$AUS_group <- as.factor(metadata$AUS_group)
    
    r2 <- round(adonis_result$R2[1], 3)
    pval <- adonis_result$`Pr(>F)`[1]
    annot_text <- paste0("PERMANOVA: R² = ", r2, ", p = ", pval)
    x_min <- min(scores.pwy.DNA_meta_top25_shannon$PCo1, na.rm = TRUE)
    y_min <- min(scores.pwy.DNA_meta_top25_shannon$PCo2, na.rm = TRUE)
    
    family_levels <- unique(bugs2plot_annotation$AMR.Gene.Family)
    bugs2plot_annotation$AMR.Gene.Family <- factor(bugs2plot_annotation$AMR.Gene.Family, 
                                                   levels = family_levels)
    family_colors <- c(
      "resistance-nodulation-cell division (RND) antibiotic efflux pump" = "#E74C3C",      
      "Erm 23S ribosomal RNA methyltransferase" = "#4168F9",                               
      "ATP-binding cassette (ABC) antibiotic efflux pump" = "#009E79",                    
      "lincosamide nucleotidyltransferase (LNU)" = "#D8BFD8",                              
      "tetracycline-resistant ribosomal protection protein" = "#F5F5DC",               
      "23S rRNA with mutation conferring resistance to macrolide antibiotics" = "#DEB887",
      "Penicillin-binding protein mutations conferring resistance to beta-lactam antibiotics" = "#CC79A0",  
      "fluoroquinolone resistant parC" = "#96CDCD",                                       
      "antibiotic resistant fusA" = "#F0E442",                                            
      "CfxA beta-lactamase" = "#332288",                                                  
      "major facilitator superfamily (MFS) antibiotic efflux pump" = "#117733",           
      "lsa-type ABC-F protein" = "#FFA07A",                                               
      "elfamycin resistant EF-Tu" = "#8B0000",                                            
      "Others" = "#A9A9B1",                                                                
      "pyrazinamide resistant rpsA" =  "#4682B4" ,
      "fluoroquinolone resistant gyrA" = "#FF7F0E",  
      "antibiotic resistant ndh" = "#2CA02C",  
      "rifamycin-resistant beta-subunit of RNA polymerase (rpoB)" = "#9467BE",
      "16S rRNA with mutation conferring resistance to aminoglycoside antibiotics" = "#FFD700",
      "fluoroquinolone resistant gyrB" = "#8A2BE1",         
      "Miscellaneous ABC-F subfamily ATP-binding cassette ribosomal protection proteins" = "#8B4513"  
    )
    dna.pco1 <- as.integer( pcoa.pwy.bug.DNA$eig/sum(pcoa.pwy.bug.DNA$eig) * 100 )[1]
    dna.pco2 <- as.integer( pcoa.pwy.bug.DNA$eig/sum(pcoa.pwy.bug.DNA$eig) * 100 )[2]

#PCoA generation
    Fig2i <- ggplot(scores.pwy.DNA_meta_top25_shannon, aes(PCo1, PCo2)) +
      geom_point(aes(fill = IN_Shannon, shape = factor(AUS_group)), size = 2.5, color = "black") +
      geom_label_repel(
        data = vec.sp.df,
        aes(x = MDS1, y = MDS2, label = species),
        fill = NA,
        label.size = NA,
        segment.color = NA,
        color = NA,
        box.padding = 0.3,
        point.padding = 0.2,
        force = 1.2,           
        max.overlaps = Inf,
        show.legend = FALSE
      ) +
      geom_shadowtext(
        data = vec.sp.df,
        aes(x = MDS1, y = MDS2, label = species),
        size = 2,
        color = 'steelblue',
        bg.color = 'white',
        fontface = 'italic',
        show.legend = FALSE
      )+
      scale_shape_manual(name = "AUS", values = c(22, 24)) +
      scale_fill_gradientn(
        name = "IN_Shannon",
        colors = colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(24)
      ) +
      geom_point(
        data = bugs2plot_annotation,
        aes(x = Dim1, y = Dim2, color = AMR.Gene.Family),
        size = 1,
        shape = 4,
        position = position_jitter(width = 0.01, height = 0.01),
        show.legend = FALSE
      )+
      scale_color_manual(
        name = "AMR.Gene.Family",
        values = family_colors
      ) +
      annotate("text", x = x_min + 0.02, y = y_min + 0.02, 
               label = annot_text, size = 2.5, hjust = 0, vjust = 0,
               fontface = "italic", color = "gray30", alpha = 0.9) +
      theme_classic() +
      theme(
        axis.line.x = element_line(colour = "black", size = 0.5),
        axis.line.y = element_line(colour = "black", size = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, "cm")
      ) +
        guides(
        shape = guide_legend(order = 1, override.aes = list(size = 1.5)),     
        fill  = guide_colorbar(order = 2, barwidth = 6, barheight = 0.3) ,    
        color = guide_legend(order = 3, ncol = 2, override.aes = list(size = 1.5))  
      )+
      xlab(paste0("PCo1 (", dna.pco1, "%)")) +
      ylab(paste0("PCo2 (", dna.pco2, "%)"))
     print(Fig2i)

