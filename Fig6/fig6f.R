###########This is data for Fig f#####################
rm(list=ls()) 
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
load("Fig6.RData")

######################################################################################## 
#effect size plot for immune cell 
aGVHD_brm_im$Y <- factor(aGVHD_brm_im$Y, levels = aGVHD_brm_im$Y[order(aGVHD_brm_im$Estimate, decreasing = FALSE)])
Fig6F_1<-ggplot(aGVHD_brm_im, aes(x = Estimate, y = Y)) +
  geom_point(size = 2) +  
  geom_errorbarh(aes(xmin = l.95..CI, xmax = u.95..CI), height = 0.2, color = "black") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  
  coord_cartesian(clip = "off") +  
  theme_classic() +
  labs(x = "Effect Size (Estimate)", y = "") +
  theme(
    axis.ticks = element_line(size = unit(0.2, "mm")),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", size = unit(0.2, "mm")),  
    plot.title = element_blank()
  )
Fig6F_1
######################################################################################## 
#boxplot for immune cell 
sorted_levels <- aGVHD_brm_im$X[order(aGVHD_brm_im$Estimate, decreasing = FALSE)]
dt.response.boxplot_long_im$variable <- factor(dt.response.boxplot_long_im$variable, levels = sorted_levels)
dt.response.boxplot_long_im$Target<-as.factor(dt.response.boxplot_long_im$Target)
dt.response.boxplot_long_im <- dt.response.boxplot_long_im %>%
  mutate(rel_abundance_for_FC = if ("value" %in% names(.)) value else (value))
fc_tbl_im <- dt.response.boxplot_long_im %>%
  group_by(variable, Target) %>%
  summarize(med = median(rel_abundance_for_FC, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Target, values_from = med, names_prefix = "grp_") %>%
  mutate(grp_0 = ifelse(is.na(grp_0), 0, grp_0),
         grp_1 = ifelse(is.na(grp_1), 0, grp_1)) %>%
  mutate(FC = ifelse(grp_0 == 0 & grp_1 == 0, 1,
                     ifelse(grp_0 == 0, Inf, grp_1 / grp_0)),
         log2FC = ifelse(is.finite(FC) & FC > 0, log2(FC),
                         NA_real_))

fc_tbl_im<-as.data.frame(fc_tbl_im)
fc_tbl_im <- fc_tbl_im %>%
  mutate(fc_centered = FC - 1)  
fc_tbl_im$variable <- factor(fc_tbl_im$variable, levels = sorted_levels)
Fig6F_2 <- ggplot(fc_tbl_im, aes(x = fc_centered, y = variable, fill = FC)) +
  geom_segment(aes(x = fc_centered, xend = fc_centered, y = variable),
               linetype = "dotted", color = "gray30", linewidth = 0.4) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +  
  scale_x_continuous(
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c("0", "0.5", "1", "1.5", "2")   
  ) +
  scale_fill_gradientn(
    colors = c("#51C3CC",
               "#bababa", 
               "#CC5800"),
    limits = c(0, 2),         
    breaks = c(0, 0.5, 1, 1.5, 2)
  )+
  theme_classic() +
  labs(
    x = "Fold Change", 
    y = NULL, 
    title = NULL
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_line(size = unit(0.2, "mm")),
    axis.line = element_line(color = "black", size = unit(0.2, "mm")),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


Fig6F_2
######################################################################################## 
#combine all plots
Fig6F_1 <- Fig6F_1 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
Fig6F_2 <- Fig6F_2 + theme(plot.margin = unit(rep(0, 4), "cm"))
g1_im <- ggplotGrob(Fig6F_1)
g3_im <- ggplotGrob(Fig6F_2)
max_height_im <- unit.pmax(g1_im$heights, g3_im$heights)
g1_im$heights <- max_height_im
g3_im$heights <- max_height_im
max_widths <- unit.pmax(g1$widths, g1_im$widths)
g1$widths <-  g1_im$widths <-  max_widths

fig6f <- grid.arrange(g1_im, g3_im,
                                ncol = 3,
                                widths = c(4.1, 1.1, 1.4),
                                layout_matrix = rbind(c(1, 2, 3)))