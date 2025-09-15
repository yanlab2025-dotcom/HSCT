###############This t-SNE plot shows Pre-HSCT clusters########
rm(list = ls())
library(Rtsne)
library(ggplot2)
library(scales)
library(gridExtra)
load("Fig3.RData")
##############################This is data for Fig 3c#################################################
#data input preparation
    col <- c(
          "g__Burkholderia" = alpha("#ff1f00", 0.75),  
          "g__Lautropia" = alpha("#4e9e9e", 0.75),    
          "g__Prevotella" = alpha("#44644a", 0.75),     
          "g__Rothia" = alpha("#ff9f42", 0.75),    
          "g__Streptococcus" = alpha("#5454c7", 0.75)
    )
    set.seed(123)
    p <- 75
    tsne_result <- Rtsne(as.matrix(metagenome), perplexity = p, verbose = FALSE)
    tsne_df <- data.frame(
          Sample = rownames(metagenome),
          TSNE1 = tsne_result$Y[, 1],
          TSNE2 = tsne_result$Y[, 2]
    )
    tsne_df$cluster <- metadata$cluster[match(tsne_df$Sample, metadata$ID)]
    tsne_df$Source <- metadata$Source[match(tsne_df$Sample, metadata$ID)]
    tsne_df$Source <- gsub("zz", "Discovery", tsne_df$Source)
    tsne_df$Source <- gsub("paper1", "Ingham2021", tsne_df$Source)
    tsne_df$Source <- gsub("paper2", "Heidrich2023", tsne_df$Source)
    tsne_df$Source <- gsub("paper3", "Faraci2024", tsne_df$Source)
    centroids <- tsne_df %>%
      group_by(cluster) %>%
      summarise(
          TSNE1 = median(TSNE1, na.rm = TRUE),
          TSNE2 = median(TSNE2, na.rm = TRUE)
    )
    tsne_df$cluster <- factor(tsne_df$cluster, levels = names(col))
    centroids$cluster <- factor(centroids$cluster, levels = names(col))

#figure generation
    Pre_HSCT_tsne <- ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = cluster
    )) +
      geom_point(size = 2.5,shape=16) +
      geom_point(data = centroids, aes(x = TSNE1, y = TSNE2, fill = cluster), 
                 size = 3, shape = 21, color = "black", stroke = 0.8) +
      scale_color_manual(values = col) +
      scale_fill_manual(values = col) + 
      theme_classic() +
      theme(
            plot.title = element_text(size = 12, hjust = 0.5),
            legend.text = element_text(size = 10),
            axis.title = element_text(size = 12,color = "black"),
            axis.line = element_line(size = 0.33,color = "black"),
            legend.position = "right",
            axis.text = element_blank(),
            axis.ticks = element_blank()
      )
    print(Pre_HSCT_tsne)
###############This t-SNE plot shows Post-HSCT clusters########
#data input preparation
    col <- c(
          "Burkholderia" = alpha("#ff1f00", 0.75),  
          "Lautropia" = alpha("#4e9e9e", 0.75),    
          "Prevotella" = alpha("#44644a", 0.75),     
          "Rothia" = alpha("#ff9f42", 0.75),    
          "Streptococcus" = alpha("#5454c7", 0.75)
    )
    set.seed(123)
    p <- 35
    tsne_result <- Rtsne(as.matrix(metagenome_out), perplexity = p, verbose = FALSE)
    tsne_df <- data.frame(
          Sample = rownames(metagenome_out),
          TSNE1 = tsne_result$Y[, 1],
          TSNE2 = tsne_result$Y[, 2]
    )
    tsne_df$cluster <- metadata_out$cluster[match(tsne_df$Sample, metadata_out$ID)]
    tsne_df$cohort <- metadata_out$cohort[match(tsne_df$Sample, metadata_out$ID)]
    tsne_df$cohort <- gsub("HSCT_out", "Discovery", tsne_df$cohort)
    tsne_df$cohort <- gsub("paper1", "Ingham2021", tsne_df$cohort)
    tsne_df$cohort <- gsub("paper2", "Heidrich2023", tsne_df$cohort)
    tsne_df$cohort <- gsub("paper3", "Faraci2024", tsne_df$cohort)
    centroids <- tsne_df %>%
      group_by(cluster) %>%
      summarise(
            TSNE1 = mean(TSNE1, na.rm = TRUE),
            TSNE2 = mean(TSNE2, na.rm = TRUE)
      )
    tsne_df$cluster <- factor(tsne_df$cluster, levels = names(col))
    centroids$cluster <- factor(centroids$cluster, levels = names(col))

#figure generation
    Post_HSCT_tsne <- ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = cluster)) +
      geom_point(size = 2.5,shape=16) +
      geom_point(data = centroids, aes(x = TSNE1, y = TSNE2, fill = cluster), 
                 size = 3, shape = 21, color = "black", stroke = 0.8) +
      scale_color_manual(values = col) +
      scale_fill_manual(values = col) + 
      theme_classic() +
      theme(
            plot.title = element_text(size = 12, hjust = 0.5),
            legend.text = element_text(size = 10),
            axis.title = element_text(size = 12,color = "black"),
            axis.line = element_line(size = 0.33,color = "black"),
            legend.position = "right",
            axis.text = element_blank(),
            axis.ticks = element_blank()
      )

     print(Post_HSCT_tsne)
