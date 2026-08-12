###############This is data for Fig 1F######
rm(list = ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grid) 
library(ComplexHeatmap)
library(circlize)
library(reshape2)
library(tibble)
load("Fig1.RData")
###########################################################################
#data input preparation
    matrix_data <- as.matrix(matrix_data)
    group_colors <- c(
      "2nd Cephalosporins" = "#8FBC8F",
      "G+ bacteria" = "#CDC673",
      "Carbapenems" = "#CD8162", 
      "Penicillins" = "#FFBBFF",
      "Fluoroquinolones" = "lightblue", 
      "1st Cephalosporins" = "cyan",
      "Tetracyclines" = "pink", 
      "Aminoglycosides" = "#CD5C5C", 
      "Monobactams" = "#DEB887",         
      "Polypeptides" = "#8B658B", 
      "Fosfomycin" = "gray"           
    )
    group_for_columns <- antibiotic_totals %>%
    dplyr::select(Antibiotic_1, Group) %>%
    column_to_rownames("Antibiotic_1") %>%
    .[colnames(matrix_data), "Group"]
# pick a nice color palette
    color_palette <- colorRamp2(
      c(0, 1, 10,20,40),
      c("gray95", "#C9BDEA", "#ACA3D5", "#864AA8", "#7A378B")  
    )
    matrix_data <- as.matrix(matrix_data)
    group_order <- antibiotic_totals$Group
    group_for_columns <- antibiotic_totals$Group 
    print(group_for_columns)
    group_for_columns <- group_for_columns[match(rownames(matrix_data), antibiotic_totals$Antibiotic_1)]
    print(group_for_columns)
    group_for_columns <- as.character(group_for_columns) 

#heatmap annotation
    row_annotation <- rowAnnotation(
      a=anno_simple(group_for_columns, col = group_colors, width = unit(0.3, "cm")),
      annotation_legend_param = list(
      title_gp = gpar(fontsize = 8),  
      labels_gp = gpar(fontsize = 8),  
      legend_width = unit(1, "cm"),  
      legend_height = unit(5, "cm"), 
      legend_shape = "square"))
    
#figure generation
    Fig1F <- Heatmap(
      matrix_data,
      name = "Days", 
      col = color_palette, 
      cluster_rows = FALSE,
      cluster_columns = TRUE,
      clustering_method_columns = "ward.D2",
      show_row_names = TRUE,
      show_column_names = FALSE,
      row_names_gp = gpar(fontsize = 8),
      column_names_gp = gpar(fontsize = 8),
      column_dend_gp = gpar(lwd = 0.5),
      heatmap_legend_param = list(title_gp = gpar(fontsize = 8)),
      right_annotation = row_annotation)
    draw(Fig1F)