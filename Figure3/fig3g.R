###############This is data for Fig 3g########
library(ggplot2)
load("Fig3g.RData")
#####################################################################################
#figure generation
    p_bubble <- ggplot(sig_abundance_top9, aes(x = Study, y = Species)) +
    geom_point(data = ~ filter(.x, Abundance > 0),  
               aes(size = Abundance, color = Abundance), alpha = 1) +
               scale_size_continuous(name = "Mean abundance",
               range = c(2, 6),  
               breaks = c(0, 0.025, 0.05, 0.075, 0.1) )+
    scale_color_gradientn(
               colours = c("#BFDBFE", "#FFE0B2", "#FFCC80", "#FFB74D", "#F57C00"),,
               name = "Relative\nabundance")+
    labs(x = "Study")+
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, color = "black", angle = 90, vjust = 0.5, hjust = 1),
               axis.text.y = element_text(size = 12, color = "black", face = "italic"),
               axis.title = element_blank(),
               panel.grid.major = element_line(color = "grey90"),
               legend.position = "right")
    p_bubble