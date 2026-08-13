###############This is data for Fig 5A####
rm(list = ls())
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(ggsignif)
library(ggpubr)
library(ggbreak)
library(gridExtra)
library(readxl)
library(reshape2)
library(tidyverse)
library(ggridges)
library(cowplot)
library(patchwork)
library(colorspace)
load("fig5A.RData")
##############################################
species_order <- rev(unique(filtered_AES$dt_species))
selected_species <- as.data.frame(t(dt_species), check.names = FALSE) %>%
  dplyr::select(dplyr::any_of(species_order))
feature_table <- selected_species %>%
  t() %>%
  as.data.frame(check.names = FALSE) %>%
  tibble::rownames_to_column(var = "feature")
foi <- intersect(species_order, feature_table$feature)
foi
feature_table$feature <- factor(feature_table$feature, levels = foi)
#############################################
#prepare metadata
md <- metadata %>%
  dplyr::rename(
    sample_name = number,
    Plot_Group = infection_all
  ) %>%
  dplyr::select(sample_name, Plot_Group) %>%
  dplyr::filter(sample_name %in% colnames(feature_table)[-1])
md$Plot_Group <- factor(
  md$Plot_Group,
  levels = unique(md$Plot_Group)
)


group_levels <- levels(md$Plot_Group)
color_list <- setNames(
  c(
    "#5254A3",
    "#AD494A",
    "#76B7B2"
  )[seq_along(group_levels)],
  group_levels
)
##################################################
#figure generation
p_zlr <- graph_zlr_general(
  feature_table = feature_table,
  md = md,
  foi = foi,
  group_column = "Plot_Group",
  color_list = color_list,
  rotate_strip_text = TRUE,
  rotate_zeros_text = TRUE,
  my_bandwidth = 0.5
)
p_zlr

