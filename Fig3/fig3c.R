###############This is data for Fig 3c########
library(Rtsne)
library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(vegan)
library(tidyr)
library(purrr)
load("Fig3c.RData")
#####################################################################################
#data input preparation
    bc_matrix <- as.matrix(vegdist(merged_data, method = "bray"))
    sample_info <- metadata %>% dplyr::select(X, Patient_Study, Curated_sampling) %>% filter(Curated_sampling %in% c("Pre-HSCT", "Post-HSCT"))
    patient_pairs <- sample_info %>%
                  group_by(Patient_Study) %>%
                  filter(n() == 2) %>%
    summarize(pre_sample = X[Curated_sampling == "Pre-HSCT"],post_sample = X[Curated_sampling == "Post-HSCT"], .groups = "drop") %>%
    filter(!is.na(pre_sample) & !is.na(post_sample))
    results <- patient_pairs %>%
    mutate(Pre_Post_Distance = map2_dbl(pre_sample, post_sample, ~bc_matrix[.x, .y]),stability = 1 - Pre_Post_Distance) %>% 
                  dplyr::select(Patient_Study, stability)
    metadata <- metadata %>% left_join(results, by = "Patient_Study")
    cluster_transition <- metadata %>% dplyr::select(Patient_Study, Curated_sampling, cluster) %>%
                  pivot_wider(names_from = Curated_sampling,values_from = cluster,names_prefix = "cluster_") %>%
                  mutate(cluster_trans = paste(`cluster_Pre-HSCT`, `cluster_Post-HSCT`, sep = "-"))
    results <- results %>%
    left_join(cluster_transition %>% 
                  dplyr::select(Patient_Study, cluster_trans),by = "Patient_Study") %>% 
                  left_join(group_mapping, by = "cluster_trans") %>%
    mutate(new_group = factor(new_group, levels = group_order),
                  panel_group = if_else(new_group %in% group_order[1:3], "Transmission", "Stable"),
                  panel_group = factor(panel_group, levels = c("Transmission", "Stable")),
                  new_group = recode(new_group, !!!name_mapping),
                  new_group = factor(new_group, levels = name_mapping))
    
#figure generation
    p3 <- ggplot(results, aes(x = new_group, y = stability, fill = new_group)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +  
      facet_grid(. ~ panel_group, scales = "free_x", space = "free_x") +
      labs(x = "Cluster Transition Groups", y = "1-BC-distance") +
      theme_classic() +
      scale_y_continuous(limits = c(0, 0.9),  breaks = c(0, 0.4, 0.8)) +
      scale_fill_manual(values = custom_colors) +
      theme(axis.text.x = element_blank(),
                  axis.text.y = element_text(size = 12, color = "black"),
                  axis.title.y = element_text(size = 12, color = "black"),
                  axis.title.x = element_blank(),
                  legend.position = "right",
                  axis.line = element_line(size = 0.33, color = "black"),
                  strip.text = element_text(size = 12, color = "black"))           
    p3
