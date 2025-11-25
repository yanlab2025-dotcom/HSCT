###############This is data for Fig 4a#######
library(ggplot2)
library(grid)
load("Fig4.RData")
#####################################################################################
#figure generation
    Fig4a <- ggplot(data = fig4a_data, aes(x = reorder(clinical, R2), y = R2, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    geom_text(aes(label = stars, 
        vjust = ifelse(Group == "In", 0.65, 0.9)), #"In" represents "Pre-HSCT"
        position = position_dodge(width = 0.75), 
        hjust = -0.3,
        size = 2) +
    theme_bw(base_size = 20) + 
    coord_flip() + 
    labs(y = "Univariate R2") +
    scale_fill_manual(values = c("#C77D88","#9CD2FB", "#55B0FA"),
        breaks = c("In_immune","In","Out")) + #"Out" represents "Post-HSCT"
    theme(
        axis.title.x = element_text(size=12,color = "black"),     
        axis.title.y = element_blank(),     
        legend.position = "right",
        legend.title = element_text(size = 8,color = "black"),      
        legend.text = element_text(size = 8,color = "black"),        
        legend.key.size = unit(0.3, "cm"),          
        legend.spacing.x = unit(0.3, 'cm'),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 0.66) ,
        axis.text.x = element_text(size=12,color = "black"),     
        axis.text.y = element_text(size=12,color = "black"),     
        axis.ticks = element_line(color = "black", size = 0.33),   
        axis.ticks.length = unit(0.1, "cm")                       )
    Fig4a
