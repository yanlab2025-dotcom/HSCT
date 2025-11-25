###############This is data for Fig 3a###################
library(vegan)
library(ggplot2)
load("Fig3a_pre.RData")
##############################################################################
#data input preparation
    metadata <- as.data.frame(metadata)
    rownames(metadata) <- metadata$ID 
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
    adonis_result_dis <- adonis2(dist.metagenome ~ cluster,  data= metadata, permutations = 999)
    Rvalue = adonis_result_dis$R2[1]
    pvalue = adonis_result_dis$`Pr(>F)`[1]
    adonis <- paste("PERMANOVA:\nR2 = ", round(Rvalue,4), "\nP-value = ", pvalue)
    centroids <- mp4.score %>%
          group_by(cluster) %>%
          summarise(Dim1 = mean(Dim1), Dim2 = mean(Dim2))
    mp4_with_centroids <- mp4.score %>%
          left_join(centroids, by = "cluster", suffix = c("_sample", "_centroid"))
    sha <- c("Discovery"         = 16,
             "Faraci_2024"       = 15, 
             "Gem_2024"          = 17,
             "Heidrich_2023"     = 18,
             "Ingham_2021"       = 19,
             "Kambara_2025"      = 8,
             "Oku_2020"          = 0,
             "Rashidi_2025"      = 1,
             "Raychaudhuri_2025" = 2,
             "Shouval_2020"      = 3,
             "Shtossel_2025"     = 4)
    
#define plot colors
    col <- c("f__Micrococcaceae" = "#549ec7",
             "f__Prevotellaceae" = "#44644a",
             "f__Streptococcaceae" = "#5454c7")
    
#figure generation
    p1 <- ggplot(mp4.score, aes(x = Dim1, y = Dim2, colour = cluster, shape = Study)) +
          scale_colour_manual(values = col) +
          scale_shape_manual(values = sha) +
          theme_classic() +
          theme(axis.line.x = element_line(colour = 'black', linewidth = 0.33),
                axis.line.y = element_line(colour = 'black', linewidth = 0.33),
                plot.title = element_text(size=12),
                axis.title = element_text(size=12), 
                legend.text = element_text(size=12),        
                legend.title= element_blank(),       
                legend.position = "right",                    
                legend.margin = margin(t = 10, b = 10),       
                legend.spacing.y = unit(0.4, "cm"),           
                axis.ticks = element_blank(), 
                axis.text = element_blank() )+
          xlab(paste0("PCo1 (", pco1, "%)")) +
          ylab(paste0("PCo2 (", pco2, "%)")) +
          geom_point(size = 1.5, alpha = 0.6) +
          geom_point(data = centroids, aes(x = Dim1, y = Dim2), 
                size = 3, shape = 25, fill = col, color = "black", stroke = 0.8) 
    p1
