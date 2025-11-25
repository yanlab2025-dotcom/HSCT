###############This is data for Fig 3b###################
library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)
load("Fig3b.RData")
##############################################################################
#data input preparation
    df_pre <- data.frame(
            X = metadata_pre$ID,
            Patient_Study = paste(metadata_pre$Patient_ID, metadata_pre$Study, sep = "-"),
            Curated_sampling = "Pre-HSCT",
            cluster = metadata_pre$cluster)
    df_post <- data.frame(
            X = metadata_post$ID,
            Patient_Study = paste(metadata_post$Patient_ID, metadata_post$Study, sep = "-"),
            Curated_sampling = "Post-HSCT",
            cluster = metadata_post$cluster)
    metadata <- rbind(df_pre, df_post)
    metadata <- metadata %>%
            group_by(Patient_Study) %>%
            filter(n() >= 2) %>%
            ungroup()
    metadata <- metadata[!(metadata$Patient_Study == "P14-Ingham_2021" & metadata$cluster == "f__Prevotellaceae"), ]
    sankey_data <- metadata %>%
            dplyr::select(Patient_Study, Curated_sampling, cluster) %>%
            pivot_wider(names_from = Curated_sampling,
            values_from = cluster) %>%
            rename(pre_cluster = `Pre-HSCT`,
            post_cluster = `Post-HSCT`) %>%
            count(pre_cluster, post_cluster) 

#figure generation
    p1 <- ggplot(sankey_data,
            aes(axis1 = pre_cluster,  
            axis2 = post_cluster,  
            y = n)) +             
      geom_alluvium(aes(fill = pre_cluster),   
            alpha = 0.5, width = 1/12) +
      geom_stratum(aes(fill = after_stat(stratum)), width = 1/12, alpha = 1,color = "white") +  
      scale_x_discrete(limits = c("Pre-HSCT", "Post-HSCT"), 
            expand = c(0.05, 0.05)) +
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
    p1
