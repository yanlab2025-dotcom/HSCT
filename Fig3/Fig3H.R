############### This is data for Fig 3H ###############
library(data.table)
library(ggplot2)
library(ggrepel)
load("Fig3H.RData")
make_fig3H <- function(data) {
  levels <- c("CD4 T","CD8 T","NK","B cell","Monocyte","Macrophage","DC","Neutrophil","Basophil","Eosinophil","Cycling myeloid")
  cols <- c(
    "CD4 T"="#1F78B4","CD8 T"="#08306B","NK"="#E6550D",
    "NKT"="#F16913","B cell"="#6A3D9A","Monocyte"="#006D2C",
    "Macrophage"="#31A354","DC"="#008B8B","Neutrophil"="#4D4D4D",
    "Basophil"="#C51B8A","Eosinophil"="#D95F0E",
    "mast cell"="#8C6D31","Cycling myeloid"="#7F2704")
  umap <- as.data.table(data$umap_plot_data)
  labels <- umap[
    ,.(x=median(UMAP_1),y=median(UMAP_2),n=.N),
    by=merged_cell_type
  ][n>=50]
  left <- ggplot(
    umap,
    aes(UMAP_1,UMAP_2,colour=merged_cell_type)) +
    geom_point(size=0.2,alpha=0.78,stroke=0) +
    scale_colour_manual(
      values=cols[levels],
      drop=FALSE,
      name="Cell type") +
    geom_label_repel(
      data=labels,
      aes(x,y,label=merged_cell_type),
      inherit.aes=FALSE,
      size=2.2,
      label.size=0.15,
      fill="white",
      alpha=0.9,
      max.overlaps=Inf,
      min.segment.length=0,
      seed=20260704) +
    labs(
      title="Merged immune cell types: UMAP",
      x="UMAP 1",
      y="UMAP 2") +
    theme_classic(base_size=7) +
    theme(
      axis.text=element_blank(),
      axis.ticks=element_blank())
  comp <- as.data.table(data$composition_plot_data)
  comp[,merged_cell_type:=factor(
    as.character(merged_cell_type),
    levels=rev(levels))]
  right <- ggplot(
    comp,
    aes(sample,percent,fill=merged_cell_type)) +
    geom_col(
      width=0.78,
      colour="white",
      linewidth=0.12) +
    scale_fill_manual(
      values=cols[levels],
      breaks=levels,
      drop=FALSE) +
    labs(
      x=NULL,
      y="Merged immune cells (%)",
      fill="Cell type",
      title="Merged cell-type composition") +
    theme_classic(base_size=7) +
    theme(
      axis.text.x=element_text(angle=45,hjust=1))
  list(left=left,right=right)}

# figure generation
Fig3H <- make_fig3H(list(umap_plot_data = umap_plot_data,composition_plot_data = composition_plot_data))
Fig3H$left
Fig3H$right
