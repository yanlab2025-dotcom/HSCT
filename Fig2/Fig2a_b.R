###############This is data for fig 2a_b####
rm(list = ls())
library(vegan)      
library(survival)  
library(survminer)  
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate) 
library(readxl)
library(openxlsx)
library(circlize)
library(vegan)
library(ComplexHeatmap)
library(grid)
set.seed(1000)
load("fig2.RData")
#######################################################################################
#data transformation
    log10_top25 <- log10(top25_data)
    log10_top25[log10_top25 == '-Inf'] <- 1000
    log10_top25 <- as.matrix(log10_top25)
    min_value <- min(na.omit(log10_top25))
    log10_top25[log10_top25 <= -5.5] <- -5.5  
    log10_top25[log10_top25 == 1000] <- -5.5  
    matched_rows <- rownames(log10_top25) %in% metadata$SampleID
    log10_top25_matched <- log10_top25[matched_rows, ]
    metadata_matched <- metadata[metadata$SampleID %in% rownames(log10_top25), ]
    log10_top25_transposed <- t(log10_top25_matched)
#cluster samples by sampling time
    dist_matrix <- vegdist(log10_top25_transposed, method = "euclidean")
    hc <- hclust(dist_matrix, method = "ward.D")
    log10_top25_clustered <- log10_top25_transposed[hc$order, ]
    
    in_group <- metadata_matched[metadata_matched$Sampling == "pre-HSCT", ] 
    out_group <- metadata_matched[metadata_matched$Sampling == "post-HSCT", ] 
    rownames(in_group) <- in_group$SampleID
    rownames(out_group) <- out_group$SampleID
    in_samples <- rownames(in_group)  
    out_samples <- rownames(out_group) 
    
    dist_in <- dist(t(log10_top25_clustered[, in_samples]))
    hc_in <- hclust(dist_in, method = "ward.D")
    in_sample_order <- colnames(log10_top25_clustered[, in_samples])[hc_in$order]
    
    dist_out <- dist(t(log10_top25_clustered[, out_samples]))
    hc_out <- hclust(dist_out, method = "ward.D")
    out_sample_order <- colnames(log10_top25_clustered[, out_samples])[hc_out$order]
    
    combined_sample_order <- c(in_sample_order, out_sample_order)
    log10_top25_clustered <- log10_top25_clustered[, combined_sample_order]
    in_group <- in_group[match(in_sample_order, in_group$SampleID), ]
    out_group <- out_group[match(out_sample_order, out_group$SampleID), ]
    merge_metadata <- rbind(in_group,out_group)
##################################################
#add prevalence annotation for species
    merge_metadata$Age <- as.numeric(merge_metadata$Age)
    merge_metadata$BMI <- as.numeric(merge_metadata$BMI)
    calculate_prevalence <- function(df, metadata) {
      prevalence_in <- numeric(nrow(df))
      prevalence_out <- numeric(nrow(df))
      for (i in 1:nrow(df)) {
        abundance <- df[i, ]
        in_samples <- metadata$SampleID[metadata$Sampling == "pre-HSCT"]
        out_samples <- metadata$SampleID[metadata$Sampling == "post-HSCT"]
        prevalence_in[i] <- sum(abundance[in_samples] > -5.5) / length(in_samples)
        prevalence_out[i] <- sum(abundance[out_samples] > -5.5) / length(out_samples)
      }
      return(data.frame(
        Gene = rownames(df),
        Prevalence_In = prevalence_in,
        Prevalence_Out = prevalence_out
      ))
    }
    prevalence_data <- calculate_prevalence(log10_top25_clustered, metadata)
    prevalence_in <- prevalence_data$Prevalence_In
    prevalence_out <- prevalence_data$Prevalence_Out
    prevalence_annotation <- rowAnnotation(
      "In" = anno_barplot(
        x = prevalence_in,
        height = unit(5, "cm"),  
        gp = gpar(fill = "#9CD2FB"),  
        border = TRUE,
        ylim = c(0, 1),
        bar_width = 0.55
      ),
      "Out" = anno_barplot(
        x = prevalence_out,
        height = unit(5, "cm"),
        gp = gpar(fill = "#55B0FA"),
        border = TRUE,
        ylim = c(0, 1),
        bar_width = 0.55
      ),gap = unit(3, "mm"),annotation_name_gp = gpar(fontsize = c(2, 2))
    )

#add metadata top annotation
    log10_top25_clustered_matrix <- as.matrix(log10_top25_clustered)  
    heatmap_colors <- colorRamp2(
      c(-5, -4, -3, -2,-1),  
      c("#000000", "#08306b", "#2171b5", "#6baed6", "#ffffcc"))
    ht_annotation <- HeatmapAnnotation(
      Sampling  = rep(c("pre-HSCT", "post-HSCT"), times = c(nrow(in_group), nrow(out_group))),
      infection_all = merge_metadata$infection_all,
      AUS_group = merge_metadata$AUS_group,
      Age = merge_metadata$Age,
      Gender = merge_metadata$Gender,
      BMI = merge_metadata$BMI,
      Disease_group = merge_metadata$Disease_group,
      aGVHD = merge_metadata$aGVHD,
      TMA = merge_metadata$TMA,
      CMV = merge_metadata$CMV,
      EBV = merge_metadata$EBV,
      PFS = merge_metadata$PFS,
      death = merge_metadata$death,
      col = list(
        Sampling = c("pre-HSCT" = "#9CD2FB", "post-HSCT" = "#55B0FA"),  
        infection_all = c("Yes" = "#453947FF", "No" = "#A4BED5FF"),  
        Gender = c("Male" = "#FFE69E", "Female" = "#FFC784"), 
        aGVHD = c("0" = "#FFE8B5", #"0" represents no aGvHD
                  "1" = "#EECB85", #"1" represents grade I aGvHD
                  "2" = "#E0A65E"), #"2" represents grade II aGvHD
        Disease_group = c("benign" = "#C1D9E5", "malignant_lymphatic" = "#6FAAC8", "malignant_myeloid" = "#025F8B"), 
        AUS_group = c("low_score"="#93C6E1FF","high_score"="#08306b","NA" = "#D3D3D3"),
        PFS = c("No" = "#51C3CC", "Yes" = "#D68572"), 
        death = c("No"= "#5254A3","Yes" = "#AD494A"),
        TMA = c("0" = "#C5EFE8", #"0" represents no TMA
                "1" = "#7FAFB0", #"1" represents grade I TMA
                "2" = "#5E8F91"), #"2" represents grade II TMA
        
        CMV = c("0" = "#FFC4B8", #"0" represents no CMV
                "1" = "#EFA292", #"1" represents grade I CMV
                "2" = "#D4785F"), #"2" represents grade II CMV
        
        EBV = c("0" = "#E7BCA4", #"0" represents no EBV
                "1" = "#C09B8B", #"1" represents grade I EBV
                "2" = "#9A756D"), #"2" represents grade II EBV
        BMI = colorRamp2(
          c(min(merge_metadata$BMI, na.rm = TRUE), max(merge_metadata$BMI, na.rm = TRUE)),
          c("#E8B8A2", "#DE8564")
        ),
        Age = colorRamp2(
          c(min(merge_metadata$Age, na.rm = TRUE), max(merge_metadata$Age, na.rm = TRUE)),
          c("#E6E6FA", "#6A5ACD")
        )
      ),show_legend = T,annotation_name_gp = gpar(fontsize = 5)
    )
##################################################
#figure generation
    fig2a_b <- Heatmap(
      log10_top25_clustered_matrix, 
      cluster_columns = FALSE, 
      cluster_rows = TRUE, 
      show_column_names = FALSE,  
      show_row_names = T,
      row_names_gp = gpar(fontsize = 5),
      top_annotation = ht_annotation, 
      right_annotation = prevalence_annotation, 
      col = heatmap_colors, 
      show_heatmap_legend = T)
    draw(fig2a_b)  
