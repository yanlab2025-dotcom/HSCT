###############This is data for Fig 4e#######
library(tidyr)
library(ggplot2)
load("Fig4e.RData")
##############################################################################
#define species list
    species_list <- c("Prevotella corporis",
                      "Pauljensenia hongkongensis",  
                      "Prevotella fusca",
                      "Prevotella denticola",
                      "Schaalia meyeri", 
                      "Prevotella intermedia",
                      "Schaalia sp. HMT-172",
                      "Veillonella rogosae",
                      "Prevotella melaninogenica",
                      "Rothia mucilaginosa",
                      "Abiotrophia defectiva")
    study_median_long$aGvHD_binary<-as.factor(study_median_long$aGvHD_binary)

#define plot colors
    color_mapping <- c(
                      "HMP1_healthy"    = "grey30",
                      "Discovery"       = "#9CD2FB", 
                      "Ingham_2021"     = "#a6dba0",
                      "Heidrich_2023"   = "#bf812d",
                      "Faraci2024"      = "#fb9a99",
                      "Raychaudhuri2025"= "#b2abd2",
                      "Oku2020"         = "#1b7837",
                      "Rashidi2025"     = "#fdbf6f",
                      "Shouval2020"     = "#ff7f00",
                      "Shtossel2025"    = "#a55480",
                      "Kambara2025"     = "grey90",
                      "Gem2024"         = "#ffff99")

#data input preparation
    species_vec <- unique(study_median_long$species)
    a_species <- gsub("_", " ", aGVHD_p$Term)
    d_species <- gsub("_", " ", death_p$Term)
    p_species <- gsub("_", " ", PFS_p$Term)
    marker_df <- data.frame(
          species = species_vec,
          aGVHD = ifelse(species_vec %in% a_species, "+", "-"),
          PFS   = ifelse(species_vec %in% p_species, "+", "-"),
          death = ifelse(species_vec %in% d_species, "+", "-"),
          stringsAsFactors = FALSE)
    marker_df$species_label <- apply(marker_df, 1, function(x) {
          sprintf("%-10s %2s %3s %3s",
                  x["species"],
                  x["aGVHD"],
                  x["PFS"],
                  x["death"])})
    label_map <- setNames(marker_df$species_label, marker_df$species)
    study_median_long$species_label <- factor(
          label_map[ study_median_long$species ],
          levels = label_map[ species_vec ])
    study_median_long$species <- factor(study_median_long$species, levels = (mean_abundance_sorted$species))
    tmp <- unique(study_median_long[, c("species", "species_label")])
    label_levels <- tmp$species_label[
          match(levels(study_median_long$species), tmp$species)]
    study_median_long$species_label <- factor(
          study_median_long$species_label,
          levels = label_levels)

#figure generation
    fig4e <- ggplot(study_median_long, 
                      aes(x = species_label, y = median_abundance,
                      fill = aGvHD_binary, group = interaction(species, aGvHD_binary))) +
          geom_boxplot(position = position_dodge(0.8), width = 0.7,
                      alpha = 1, outlier.shape = NA) +
          geom_point(aes(color = Study),
                      position = position_jitterdodge(jitter.width = 0,
                      dodge.width = 0.8),size = 1.5) +
          scale_fill_manual(values = c("No" = "#FFE8B5", "Yes" = "#E0A65E", "Healthy" = "grey30")) +
          scale_y_log10() +
          scale_color_manual(values = color_mapping) +
          labs(x = "species aGVHD PFS Death", y = "Mean Abundance (log10)") +
          theme_classic() +
          theme(axis.text.y  = element_text( size = 10), 
                      axis.text.x  = element_text( size = 10,angle = 90), 
                      axis.title.x = element_text( size = 10),  
                      legend.position = "none")
    fig4e
