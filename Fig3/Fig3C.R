############### This is data for Fig 3C ###############
library(survival)
library(survminer)
library(ggplot2)
library(cowplot)
library(patchwork)
load("Fig3C.RData")
cluster_colors <- c(
  "f__Burkholderiaceae"="#d62728",
  "f__Micrococcaceae"="#549ec7",
  "f__Prevotellaceae"="#44644a",
  "f__Streptococcaceae"="#5454c7",
  "f__Veillonellaceae"="#08306b")
make_fig3C <- function(data) {
  make_one <- function(metadata, label) {
    metadata$cluster <- factor(
      metadata$cluster,
      levels = names(cluster_colors)[
        names(cluster_colors) %in% unique(metadata$cluster)])
    fit <- survival::survfit(
      survival::Surv(aGvHD_time, aGvHD) ~ cluster,
      data = metadata)
    km <- survminer::ggsurvplot(
      fit, data = metadata,
      pval = TRUE, pval.size = 3.5,
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.title = "",
      risk.table.fontsize = 4,
      size = 0.66,
      legend.title = paste(label, "cluster"),
      legend.labs = levels(metadata$cluster),
      xlab = "Time (days)",
      ylab = "Survival probability",
      palette = cluster_colors[levels(metadata$cluster)],
      ggtheme = ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank()),
      tables.theme = ggplot2::theme_classic(base_size = 10) +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank()))
    cowplot::plot_grid(
      km$plot, km$table,
      ncol = 1,
      align = "v",
      rel_heights = c(3, 1.4))}
  pre <- make_one(data$metadata_pre, "Pre-HSCT")
  post <- make_one(data$metadata_post, "Post-HSCT")
  list(
    pre = pre,
    post = post,
    combined = patchwork::wrap_plots(pre, post, ncol = 2))}

# figure generation
Fig3C <- make_fig3C(list(metadata_pre = metadata_pre,metadata_post = metadata_post))
Fig3C