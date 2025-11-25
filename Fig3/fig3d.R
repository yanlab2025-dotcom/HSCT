###############This is data for Fig 3d########
library(ComplexHeatmap)
load("Fig3d.RData")
#####################################################################################
#data input preparation
    col_anno <- HeatmapAnnotation(
    In_Out = cluster_all$In_Out[match(final_column_order, cluster_all$number)],
    cluster_status =  cluster_all$cluster_status[match(final_column_order, cluster_all$number)],
    col = list(
    In_Out = c("In" = "#9CD2FB", "Out" = "#55B0FA"),
    cluster_status = c(
          "In_f__Prevotellaceae"    = alpha( "#44644a", 0.75),
          "In_f__Micrococcaceae"    = alpha("#549ec7", 0.75),
          "In_f__Streptococcaceae"  = alpha("#5454c7", 0.75),
          "Out_f__Burkholderiaceae" = alpha("#d62728", 0.75),
          "Out_f__Prevotellaceae"   = alpha("#44644a", 0.75),
          "Out_f__Micrococcaceae"   = alpha("#549ec7", 0.75),
          "Out_f__Veillonellaceae"  = alpha("#08306b", 0.75))),
    annotation_name_gp = gpar(fontsize = 4),   
    annotation_legend_param = list(In_Out = list(show = FALSE),
    cluster_status = list(show = FALSE)),
    show_legend = FALSE,  show_annotation_name = FALSE)

#figure generation
    ht <- Heatmap(
    matrix = mat[, final_column_order, drop = FALSE],
          name = "log-RPKM",
          col = col_fun,
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          row_order = row_order,
          show_row_dend = FALSE,
          show_column_dend = FALSE,
          show_row_names = FALSE,
          column_order = final_column_order,
          top_annotation = col_anno,
          show_column_names = FALSE,
          column_names_gp = gpar(fontsize =4),
          row_names_gp = gpar(fontsize = 4),
          show_heatmap_legend = FALSE,  
          heatmap_legend_param = list(title = "logRPKM"),  
          use_raster = TRUE,  
          raster_quality = 5)
    ht
