###############This is data for Fig 3D#######
rm(list=ls()) 
library(ggplot2)
library(readxl)
library(scales)
library(patchwork)
library(grid) 
library(circlize)
load( "Fig3final.RData")
#####################################################################################
#figure generation
col_fun <- colorRamp2(
  breaks = c(0, 1.5,3,3.45,4.1,5.6),
  colors = c("#FFFFFF", "#edf8b1","#9ed979", "#5ec3b1","#225ea8","#123680")
)

ht <- Heatmap(
  matrix = mat[, final_column_order, drop = FALSE],
  name = "log-RPKM",
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_order = row_order,
  show_row_dend = FALSE,
  show_column_dend = FALSE,
  row_labels = ifelse(rownames(mat) %in% target_genes, rownames(mat), ""),
  show_row_names = TRUE,
  column_order = final_column_order,
  top_annotation = col_anno,
  show_column_names = FALSE,
  column_names_gp = gpar(fontsize =4),
  row_names_gp = gpar(fontsize = 4),
  show_heatmap_legend = FALSE,  
  heatmap_legend_param = list(title = "logRPKM")  
  
)

pdf("Fig3D.pdf", width = 5, height = 6)
draw(ht)
dev.off()
