###############This is figure 2j####
rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx) 
library(tidyr)
library(dplyr)
library(ggrepel)
library(ggplot2)
load("fig2.RData")
#data input preparation and transformation
    AUS_limma_In <- AUS_limma_In[AUS_limma_In$adj.P.Val <=0.25 & AUS_limma_In$freq_ratio >= 0.1, ]
    AUS_limma_In$group <- "AUS"
    AUS_limma_In <- AUS_limma_In[, c("Gene", "logFC", "group","P.Value","freq_ratio",
                                     "adj.P.Val","Sig_p.val","Sig_adj.p.val","sample_count" ,                       
                                     "AMR_Gene_Family" ,"Resistance_Mechanism"   )]
    DEG_df_In <- as.data.frame(AUS_limma_In)
    
    summary(AUS_limma_In$P.Value)
    DEG_df_In$Sig_adj.p.val <- factor(DEG_df_In$Sig_adj.p.val, levels = c("Up", "Down", "None"))
    cut_off_logFC <- 0
    cut_off_pvalue <- 0.05
    family_colors <- c(
      "resistance-nodulation-cell division (RND) antibiotic efflux pump" = "#E74C3C",      
      "Erm 23S ribosomal RNA methyltransferase" = "#0072B2",                               
      "ATP-binding cassette (ABC) antibiotic efflux pump" = "#009E73",                    
      "lincosamide nucleotidyltransferase (LNU)" = "#984EA3",                              
      "tetracycline-resistant ribosomal protection protein" = "#AFEEEE",                  
      "23S rRNA with mutation conferring resistance to macrolide antibiotics" = "#DEB887",
      "Penicillin-binding protein mutations conferring resistance to beta-lactam antibiotics" = "#CC79A7",  
      "fluoroquinolone resistant parC" = "#00BFC4",                                       
      "antibiotic resistant fusA" = "#F0E442",                                            
      "CfxA beta-lactamase" = "#332288",                                                  
      "major facilitator superfamily (MFS) antibiotic efflux pump" = "#117733",           
      "lsa-type ABC-F protein" = "#FFA07A",                                               
      "elfamycin resistant EF-Tu" = "#8B0000",                                            
      "Others" = "#A9A9B1",                                                                
      "pyrazinamide resistant rpsA" = "#FF7F0E",  
      "fluoroquinolone resistant gyrA" = "#1F77B4",  
      "antibiotic resistant ndh" = "#2CA02C",  
      "rifamycin-resistant beta-subunit of RNA polymerase (rpoB)" = "#9467BD",
      "16S rRNA with mutation conferring resistance to aminoglycoside antibiotics" = "#FFD700",
      "fluoroquinolone resistant gyrB" = "#8A2BE2",         
      "Miscellaneous ABC-F subfamily ATP-binding cassette ribosomal protection proteins" = "#20B2AA"  
    )
    DEG_df_In$AMR_Gene_Family <- factor(DEG_df_In$AMR_Gene_Family)
    unique_AMR_Gene_Families <- unique(DEG_df_In$AMR_Gene_Family)

#Data Processing
    levels(DEG_df_In$AMR_Gene_Family) <- c(levels(DEG_df_In$AMR_Gene_Family), "None")
    DEG_df_In$AMR_Gene_Family[DEG_df_In$Sig_adj.p.val == "None"] <- "None"
    freq_table <- table(DEG_df_In$AMR_Gene_Family)
    low_freq_families <- names(freq_table[freq_table == 1])
    DEG_df_In$AMR_Gene_Family <- ifelse(
      DEG_df_In$AMR_Gene_Family %in% low_freq_families,
      "Others",
      as.character(DEG_df_In$AMR_Gene_Family)
    )
    top_genes <- DEG_df_In %>%
      filter(Sig_adj.p.val != "None") %>% 
      arrange(adj.P.Val) %>%           
      head(10)  

#figure generation
    fig2j <- ggplot(DEG_df_In, aes(x = logFC, y = -log10(adj.P.Val), fill = AMR_Gene_Family)) +
      geom_point(alpha = 0.8, size = 2, shape = 21) +  
      scale_fill_manual(
        values = family_colors,
        na.value = "gray90"  
      ) +
      geom_vline(xintercept = c(-cut_off_logFC, cut_off_logFC), 
                 linetype = 4, color = "black", linewidth = 0.8) +
      geom_text_repel(
        data = top_genes,
        aes(label = Gene),
        size =2.5,
        box.padding = 0.2,
        point.padding = 0.5,
        max.overlaps = Inf
      ) +
      annotate("segment", 
               x = -0.05, xend = -1, 
               y = 0.1, yend = 0.1, 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
               color = "#93C6E1FF", linewidth = 0.33) +
      annotate("text", 
               x = -0.5, y = 0.0, 
               label = "low-AUS", 
               color = "black", hjust = 1, size = 3) +
      annotate("segment", 
               x = 0.05, xend = 1, 
               y = 0.1, yend = 0.1, 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
               color = "#08306b", linewidth = 0.33) +
      annotate("text", 
               x = 0.5, y = 0.0, 
               label = "high-AUS", 
               color = "black", hjust = 0, size = 3) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black", linewidth = 0.33) +
      labs(x = "log2(Fold Change)", y = "-log10 (Q-value)") +
      ggtitle("Volcano_plot") +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 7),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 0.5),
        axis.ticks = element_line(linewidth = 0.5),
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.text = element_text(size = 7)
      ) +
      ylim(c(0, max(-log10(DEG_df_In$adj.P.Val))))
    dev.new() 
    print(fig2j)

    
