############### This is data for Fig 3B ###############
library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)
load("Fig3B.RData")
sankey_data <- metadata %>%
  pivot_wider(
    id_cols = Patient_Study,
    names_from = Curated_sampling,
    values_from = cluster) %>%
  count(`Pre-HSCT`, `Post-HSCT`) %>%
  rename(
    pre_cluster = `Pre-HSCT`,
    post_cluster = `Post-HSCT`)
# figure generation
Fig3B <- ggplot(sankey_data,aes(axis1 = pre_cluster,axis2 = post_cluster,y = n)) +             
  geom_alluvium(aes(fill = pre_cluster),alpha = 0.5, width = 1/12) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 1/12, alpha = 1,color = "white") +  
  scale_x_discrete(limits = c("Pre-HSCT", "Post-HSCT"), expand = c(0.05, 0.05)) +
  scale_fill_manual(values = c("f__Micrococcaceae"   = "#549ec7", 
                               "f__Prevotellaceae"   = "#44644a",
                               "f__Streptococcaceae" = "#5454c7",
                               "f__Veillonellaceae"  = "#08306b",
                               "f__Burkholderiaceae" = "#d62728")) +
  labs(x = "Sampling", y = "Number of Patients", fill = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
Fig3B