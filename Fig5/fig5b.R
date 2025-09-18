#############This is data for Fig 5b#############
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggh4x)
library(scales)
library(patchwork)
load("Fig5.RData")
############################################################################
#data visualization function
    plot_dot_effectsize <- function(data,
                                    cor_var = "Cor",
                                    size_var = "SizeGroup",
                                    label_var = "PLabel",
                                    x_var = "Species",
                                    y_var = "Cell",
                                    facet_var = "Group",
                                    fill_limits = c(-0.25, 0.25),
                                    fill_colours = c("#0571b0","#92c5de","#f7f7f7","#f4a582","#ca0020"),
                                    size_values = c("0.1" = 2.5, "0.2" = 4, "0.3" = 5),
                                    alpha = 0.7,  # 添加alpha参数
                                    reverse_y = TRUE) {

# figure generation 
      p <- ggplot(data) +
        geom_point(aes(x = .data[[x_var]], 
                       y = .data[[y_var]], 
                       fill = .data[[cor_var]], 
                       size = .data[[size_var]],
                       alpha = alpha), 
                   shape = 21, color = "black") +
        geom_text(aes(x = .data[[x_var]], 
                      y = .data[[y_var]], 
                      label = .data[[label_var]]), 
                  size = 6, color = "white") +
        scale_fill_gradientn(
          colours = fill_colours,
          limits = fill_limits,
          name = "Effect size"
        ) +
        scale_size_manual(values = size_values) +
        scale_alpha_identity() +  
        theme_classic() +
        coord_cartesian(clip = "off") + 
        facet_nested(as.formula(paste(". ~", facet_var)), 
                     scales = "free_x", space = "free_x") +
        coord_flip() +
        theme(
          strip.background = element_rect(fill = "grey80", color = "black", linewidth = 0.72),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.line = element_blank(),
          axis.ticks = element_line(linewidth = 0.5, color = "black"),
          axis.ticks.length = unit(0.1, "cm"),
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 0.72),
          legend.position = "right"
        )
      if (reverse_y) {
        p <- p + scale_y_discrete(limits = rev)
      }
      return(p)
    }


Imune_mic_cor$sample <- ifelse(Imune_mic_cor$sample == "all_patients", "ALL",
                         ifelse(Imune_mic_cor$sample == "low_AUS_patients", "Low", 
                                Imune_mic_cor$sample))

# data preparation 
    Imune_mic_cor <- Imune_mic_cor[Imune_mic_cor$Species %in% c("Prevotella_melaninogenica","Prevotella_jejuni",
                                              "Fusobacterium_pseudoperiodonticum","Prevotella_sp_oral_taxon_299",
                                              "Leptotrichia_wadei" ), ]
    Imune_mic_cor$Signif <- ifelse(Imune_mic_cor$Cor < 0, "Negative", "Positive")
    Imune_mic_cor$Signif <- ifelse(Imune_mic_cor$P < 0.05, Imune_mic_cor$Signif, "Missing")
    Imune_mic_cor$Species <- factor(Imune_mic_cor$Species, levels = unique(Imune_mic_cor$Species))
    Imune_mic_cor$Cell <- factor(Imune_mic_cor$Cell, levels = rev(unique(Imune_mic_cor$Cell))) 
    data_all <- Imune_mic_cor[Imune_mic_cor$sample == "ALL", ]
    data_low <- Imune_mic_cor[Imune_mic_cor$sample == "Low", ]
    all_species <- unique(c(data_low$Species, data_all$Species))
    all_cells   <- unique(c(data_low$Cell, data_all$Cell))
    all_species <- sort(all_species)
    all_cells <- sort(all_cells)

# add ordering and region information
    data_low <- data_low %>%
      mutate(
        Species_order = as.numeric(factor(Species, levels = all_species)),
        Cell_order = as.numeric(factor(Cell, levels = all_cells)),
        Region = "Right"  
      )
    data_all <- data_all %>%
      mutate(
        Species_order = as.numeric(factor(Species, levels = all_species)),
        Cell_order = as.numeric(factor(Cell, levels = all_cells)),
        Region = "Left"  
      )
    plot_data <- bind_rows(data_low, data_all)
    AUS <- AUS %>%
      distinct(dt_species, .keep_all = TRUE)
    plot_data<-merge(plot_data,AUS,by.x="Species",by.y="dt_species")
    plot_data$sig_lable <- paste0(plot_data$Species, "_", plot_data$Cell)
    sig_labels_under_0.05 <- plot_data %>%
      filter(P < 0.05) %>%
      pull(sig_lable) %>%
      unique()

# prepare data for plotting
    plot_data_all <-  plot_data[plot_data$sample == "ALL", ]
    plot_data_low <- plot_data[plot_data$sample == "Low", ]
    paired_df <- merge(plot_data_all, plot_data_low, 
                       by = c("Species", "Cell"), suffixes = c("_all", "_low"))
    paired_df$opposite_signif <- with(paired_df, 
                                      Signif_all %in% c("Positive", "Negative") &
                                        Signif_low %in% c("Positive", "Negative") &
                                        sign(Cor_all) != sign(Cor_low))
    highlight_opposite <- paired_df %>% filter(opposite_signif) %>% dplyr::select(Species, Cell)

# define species order for plotting
    species_order <- c(
      "Prevotella_melaninogenica",
      "Prevotella_jejuni",
      "Fusobacterium_pseudoperiodonticum",
      "Prevotella_sp_oral_taxon_299",
      "Leptotrichia_wadei"
    )
    plot_data_all$Species <- factor(plot_data_all$Species, levels = rev(species_order))
    plot_data_low$Species <- factor(plot_data_low$Species, levels = rev(species_order))
    plot_data_all$SizeGroup <- cut(abs(plot_data_all$Cor),
                                   breaks = c(-Inf, 0.1, 0.2, Inf),
                                   labels = c("0.1", "0.2", "0.3"))
    plot_data_low$SizeGroup <- cut(abs(plot_data_low$Cor),
                                   breaks = c(-Inf, 0.1, 0.2, Inf),
                                   labels = c("0.1", "0.2", "0.3"))
    plot_data_all$PLabel <- ifelse(plot_data_all$P < 0.01, "*",
                                   ifelse(plot_data_all$P < 0.05, ".", ""))
    plot_data_low$PLabel <- ifelse(plot_data_low$P < 0.01, "*",
                                   ifelse(plot_data_low$P < 0.05, ".", ""))
    inflammation_markers <- c("IL_6", "IL_10", "IL_4", "CRP", "IgM")
    plot_data_all$Group <- ifelse(plot_data_all$Cell %in% inflammation_markers, "ALL Inflammatory", "ALL Immune")
    plot_data_low$Group <- ifelse(plot_data_low$Cell %in% inflammation_markers, "Low Inflammatory", "Low Immune")
    plot_data_all$Group <- factor(plot_data_all$Group, levels = c("ALL Inflammatory", "ALL Immune"))
    plot_data_low$Group <- factor(plot_data_low$Group, levels = c("Low Inflammatory", "Low Immune"))
    plot_data_all$Cor <- pmax(pmin(plot_data_all$Cor, 0.25), -0.25)
    plot_data_low$Cor <- pmax(pmin(plot_data_low$Cor, 0.25), -0.25)
    p1 <- plot_dot_effectsize(plot_data_all)
    p2 <- plot_dot_effectsize(plot_data_low)
    
    #Combine plots
    Fig5b <- (p1 / p2) + 
      plot_layout(guides = "collect") & 
      theme(legend.position = "right")

    Fig5b
