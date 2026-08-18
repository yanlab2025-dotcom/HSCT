################# This is data for Fig 6C,D ################# 

rm(list=ls()) 
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(caret)
library(brms)
load("Fig6C_D.RData")

########################################################################################

# --- 01 Brm model ---

data <- as.data.frame(data) 
data <- subset(data, AUS_grp == "low_score")

row.names(data) <- data$Sample_id  ## set row.names
abundance <- data[,c(-1,-168)]

dt_species <- abundance

dt_species$PFS <- as.factor(dt_species$PFS)

## train & test datasets
set.seed(123)
trainIndex <- createDataPartition(dt_species$PFS, p = 0.8, list = FALSE)
train_data <- dt_species[trainIndex, ]
test_data <- dt_species[-trainIndex, ]

lrfit1 <-  brm(PFS ~ ., 
               data = train_data,
               family = bernoulli(),
               chains = 4,
               warmup = 1000,
               iter = 2000,
               seed = 12345,
               refresh = 0,
               backend = "cmdstanr")

summary_lrfit1 <- summary(lrfit1)$fixed


biomarker_23 <- c("Schaalia_radingae",	"Leptotrichia_sp_oral_taxon_212",
                  "Streptococcus_phage_PH10","Prevotella_oris",	"Haemophilus_parainfluenzae",
                  "Streptococcus_oralis",	"Neisseria_bacilliformis",	"Candidatus_Nanosynbacter_sp_HMT_352",
                  "Streptococcus_sp_NPS_308",	"Streptomyces_sp_T12",	"Capnocytophaga_sputigena",	"Veillonella_parvula",
                  "Prevotella_nigrescens",	"Fusobacterium_periodonticum",	"Stenotrophomonas_maltophilia",
                  "Streptococcus_salivarius",	"Lactiplantibacillus_pentosus", "Leptotrichia_sp_oral_taxon_221",
                  "Neisseria_elongata",	"Actinomyces_trachealis",	"Campylobacter_showae",
                  "Actinomyces_sp_oral_taxon_414", "Prevotella_corporis")

estimates_23_feature <- summary_lrfit1[rownames(summary_lrfit1) %in% biomarker_23, ]


# --- 02 Effect size plot  ---

PFS_brm$Feature <- factor(PFS_brm$Feature, levels = PFS_brm$Feature[order(PFS_brm$Estimate, decreasing = FALSE)])
Fig6C_1 <- ggplot(PFS_brm, aes(x = Estimate, y = Feature)) +
      geom_point(aes(color = Estimate), size = 2) +  
      geom_errorbarh(aes(xmin = l.95..CI, xmax = u.95..CI), 
                     width = 0.2, color = "black", size = unit(0.2, "mm")) +  
      scale_color_gradient2(low = "#084594", mid = "#e2e2e2", high = "#cc0000",
                            midpoint = 0, limits = c(-2000, 2000), breaks = c(-2000, 0, 2000),
                            oob = scales::squish,  name = "Estimates") +
      coord_cartesian(clip = "off") +  
      theme_classic() +
      labs(x = "Effect Size (Estimate)", y = "") +
      theme(
        axis.ticks = element_line(size = unit(0.2, "mm")),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.border = element_rect(fill = NA, color = "black", size = unit(0.2, "mm")),  
        plot.title = element_blank(),
        legend.position = c(0.85, 0.25), 
        legend.background = element_blank()
      )
    Fig6C_1
    

# --- 03 Wilcox test plot  ---
dt.response.boxplot_long$PFS <- ifelse(dt.response.boxplot_long$PFS == "NO", "0",
                                       ifelse(dt.response.boxplot_long$PFS == "YES", "1",
                                              dt.response.boxplot_long$PFS))    
    
dt.response.boxplot_long$variable <- as.character(dt.response.boxplot_long$variable)
sorted_levels <- PFS_brm$Feature[order(PFS_brm$Estimate, decreasing = FALSE)]
dt.response.boxplot_long$variable <- factor(dt.response.boxplot_long$variable, levels = sorted_levels)
dt.response.boxplot_long$PFS<-as.factor(dt.response.boxplot_long$PFS)
dt.response.boxplot_long <- dt.response.boxplot_long %>%
    mutate(rel_abundance_for_FC = if ("sqrtAbundance" %in% names(.)) sqrtAbundance else (sqrtAbundance)^2)

fc_tbl$variable <- factor(fc_tbl$variable, levels = sorted_levels)
    
fc_tbl <- fc_tbl %>%
      left_join(wilcox_test_results, by = "variable")
Fig6D_1 <- ggplot(fc_tbl, aes(x = fc_centered, y = variable, fill = FC)) +
      geom_segment(aes(x = fc_centered, xend = fc_centered, y = variable),
                   linetype = "dotted", color = "gray30", linewidth = 0.4) +
      geom_col(width = 0.6) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
      
      geom_text(aes(x = ifelse(fc_centered >= 0, fc_centered + 0.1, fc_centered - 0.1),
                    label = significance.x),
                size = 4, color = "black", vjust = 0.7) +
      
      scale_x_continuous(
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = c("0", "0.5", "1", "1.5", "2")
      ) +
      scale_fill_gradientn(
        colors = c("#51C3CC", "#bababa", "#CC5800"),
        limits = c(0, 2),
        breaks = c(0, 0.5, 1, 1.5, 2)
      ) +
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
    
    
    Fig6D_1

    
# --- 04 Combine Fig6C & D  ---

 Fig6C_1 <- Fig6C_1 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
 Fig6D_1 <- Fig6D_1 + theme(plot.margin = unit(rep(0, 4), "cm"))
 g1 <- ggplotGrob(Fig6C_1)
 g2 <- ggplotGrob(Fig6D_1)
 max_height <- unit.pmax(g1$heights,g2$heights)
 g1$heights <- max_height
 g2$heights <- max_height
 Fig6C_1 <- Fig6C_1 + theme(plot.margin = unit(rep(0.1, 4), "cm"))
 Fig6D_1 <- Fig6D_1 + theme(plot.margin = unit(rep(0, 4), "cm"))
 combined_plot <- grid.arrange(g1, g2,
                             ncol = 2,
                             widths = c(4.1, 1.1),
                             layout_matrix = rbind(c(1, 2)))





           
