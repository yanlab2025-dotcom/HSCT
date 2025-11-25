###############This is data for Fig 4c#######
library(stringr)
library(dplyr)
load("Fig4c.RData")
##############################################################################
#data input preparation
    TABLE_massslin3$ave_coef <- rowMeans(TABLE_massslin3[, 15:25], na.rm = TRUE)
    TABLE_massslin_1 <- TABLE_massslin3 %>% filter(Masslin %in% c(6:11))
    TABLE_massslin_1 <- TABLE_massslin_1 %>%
         arrange(desc(abs(ave_coef))) %>% 
         slice_head(n = 10)  
    TABLE_massslin3_sorted <- dplyr::bind_rows(
    TABLE_massslin_1  %>% mutate(Group = "abs(Coef) top10"))
    common_rows <- intersect(TABLE_massslin3_sorted$Row.names, result$Variable)
    filtered_result <- result[result$Variable %in% common_rows, ]
    TABLE_massslin3_sorted_fil<-TABLE_massslin3_sorted[,c(1,15:25)]
    TABLE_massslin3_sorted_fil <- TABLE_massslin3_sorted_fil %>%
         rename_with(~ str_remove(., "\\.y$")) 
    boxplot_data <- filtered_result %>%
         pivot_wider(
         names_from = Statistic,
         values_from = Value)
    filter_table_long <- TABLE_massslin3_sorted_fil %>%
         pivot_longer(
         cols = -Row.names,
         names_to = "Study",
         values_to = "present"
          ) %>%
         rename(Variable = Row.names) 
    valid_combinations <- filter_table_long %>%
         filter(present != 0)
    valid_combinations$Study <- gsub("_coef$", "", valid_combinations$Study)

    #filtering boxplot data 
    boxplot_data_filtered <- boxplot_data %>%
         inner_join(valid_combinations %>% dplyr::select(Variable, Study), 
         by = c("Variable", "Study")) %>%
         bind_rows(
    boxplot_data %>% filter(Study == "HMP1_healthy")) %>%distinct()

#prepare sorting variable
    boxplot_merge_data<-merge(boxplot_data_filtered,TABLE_massslin3_sorted[,c(1,14,27,28)],by.x="Variable",by.y="Row.names")
    boxplot_data_filtered$Study

#define plot colors
    color_mapping <- c("HMP1_healthy"      = "grey30",
                       "Discovery"         = "#9CD2FB", 
                       "Ingham_2021"       = "#a6dba0",
                       "Heidrich_2023"     = "#bf812d",
                       "Faraci_2024"       = "#fb9a99",
                       "Raychaudhuri_2025" = "#b2abd2",
                       "Oku2020"           = "#1b7837",
                       "Rashidi2025"       = "#fdbf6f",
                       "Shouval2020"       = "#ff7f00",
                       "Shtossel2025"      = "#a55480",
                       "Kambara2025"       = "grey90",
                       "Gem2024"           = "#ffff01") 
    boxplot_merge_data$mean_ab_NO_HMP1<-as.numeric(boxplot_merge_data$ave_coef)
    sorted_variables <- boxplot_merge_data %>%
          group_by(Variable) %>%
          summarise(Avg_mean_ab_NO_HMP1 = mean(mean_ab_NO_HMP1, na.rm = TRUE)) %>%
          arrange(Avg_mean_ab_NO_HMP1)  
    boxplot_merge_data$Variable <- factor(boxplot_merge_data$Variable, 
                            levels = rev (sorted_variables$Variable))

#figure generation
    fig4c <- ggplot(boxplot_merge_data, aes(x = Variable, y = median)) +
          scale_y_continuous(trans = "log10") +
          geom_boxplot(fill = "white", color = "black", outlier.shape = NA, width = 0.6) +
          geom_jitter(aes(color = Study), size = 1.5,  alpha = 1,
          position = position_jitterdodge( jitter.width = 0, dodge.width = 0)) +
          geom_point(data = subset(boxplot_merge_data, Study == "HSCT"),
                            aes(color = Study), 
                            size = 1.5,  
                            alpha = 1,  
                            position = position_jitterdodge(
                            jitter.width = 0,dodge.width = 0 )) +
          scale_fill_manual(values = color_mapping,
                            labels = c("HMP1","HSCT","Ingham2021", "Heidrich2023", "Faraci2024", "Raychaudhuri2025",
                                       "Oku2023","Rashidi2025", "Shouval2020" ,"Shtossel2025","Kambara2025","Gem2024" ),
                            breaks = names(color_mapping),
                            name = "Disease") +
          scale_color_manual(values = color_mapping,
                            labels = c("HMP1_table","HSCT","Ingham2021", "Heidrich2023", "Faraci2024", "Raychaudhuri2025",
                                        "Oku2023","Rashidi2025", "Shouval2020" ,"Shtossel2025","Kambara2025","Gem2024" ),
                            breaks = names(color_mapping),
                            name = "Disease") +
          labs(x = NULL, y = "Relative Abundance(log10)") +
          theme_classic(base_size = 12) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                            legend.position = "right")
    fig4c