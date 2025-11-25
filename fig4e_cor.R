###############This is data for Fig 4e#######
library(patchwork)
load("Fig4e_cor.RData")
##############################################################################
#figure generation
    p1 <- ggplot(data, aes(x = log10(Pauljensenia_hongkongensis + 1e-6), 
                           y = log10(NK_cells +1))) +
          geom_point(alpha = 1, size =2, color = "grey",shape=16) +  
          geom_smooth(data = subset(data,(AUS_group == "high_score" | AUS_group == "low_score")),
                       aes(x = log10(Pauljensenia_hongkongensis + 1e-6), 
                           y = log10(NK_cells + 1)),
          method = "lm", se = T, color = "#C1D9E5", fill = "#C1D9E550") +
          geom_smooth(data = subset(data, ( AUS_group == "low_score")),
                       aes(x = log10(Pauljensenia_hongkongensis + 1e-6), 
                           y = log10(NK_cells + 1)),
          method = "lm", se = T, color = "#08306b", fill = "#08306b50") +
          labs(x = "log10(Pauljensenia_hongkongensis + 1e-6)/nrelative abundance(post-HSCT)",
               y = "log10(NK cell counts + 1)(10-50 days)") +
          stat_cor(data = subset(data, (AUS_group == "high_score" | AUS_group == "low_score")),
                      aes(x = Pauljensenia_hongkongensis, y = NK_cells), 
                      method = "spearman",
                      label.x.npc = "left",
                      label.y = 4,
                      color = "#C1D9E5") +
          stat_cor(data = subset(data, AUS_group == "low_score"),
                      aes(x = Pauljensenia_hongkongensis, y = NK_cells), 
                      method = "spearman",
                      label.x.npc = "left", 
                      label.y = 3.5,
                      color = "#08306b") +
          theme_bw() +
          theme(legend.position = "none",  
                plot.title = element_text(hjust = 0.5),
                panel.border = element_rect(color = "black", fill = NA))
    p1
    p2 <- ggplot(data, aes(x = log10(Pauljensenia_hongkongensis + 1e-6), 
                           y = log10(T8_cells +1))) +
          geom_point(alpha = 1, size = 2, color = "grey",shape=16) +  
          geom_smooth(data = subset(data,(AUS_group == "high_score" | AUS_group == "low_score")),
                       aes(x = log10(Pauljensenia_hongkongensis + 1e-6), 
                           y = log10(T8_cells + 1)),
          method = "lm", se = T, color = "#C1D9E5", fill = "#C1D9E550") +
          geom_smooth(data = subset(data, ( AUS_group == "low_score")),
                       aes(x = log10(Pauljensenia_hongkongensis + 1e-6), 
                           y = log10(T8_cells + 1)),
          method = "lm", se = T, color = "#08306b", fill = "#08306b50") +
          stat_cor(data = subset(data, (AUS_group == "high_score" | AUS_group == "low_score")),
                       aes(x = Pauljensenia_hongkongensis, y = T8_cells), 
                       method = "spearman",
                       label.x.npc = "left",
                       label.y = 4,
                       color = "#C1D9E5") +
          stat_cor(data = subset(data, AUS_group == "low_score"),
                       aes(x = Pauljensenia_hongkongensis, y = T8_cells), 
                       method = "spearman",
                       label.x.npc = "left", 
                       label.y = 3.5,
                       color = "#08306b" ) +
          theme_bw() +
          theme(legend.position = "none",  
                plot.title = element_text(hjust = 0.5),
                panel.border = element_rect(color = "black", fill = NA))
    p2
    p <- (p1 / p2) 
    p
