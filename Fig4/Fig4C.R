############### This is data for Fig 4C ###############

###### Maaslin3 DA testing (ref: HMP1_healthy) ######
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
load("Fig4C_cohort_data.RData")
comparisons_cohort <- list(
  c("HMP1_healthy", "Discovery"),
  c("HMP1_healthy", "Faraci_2024"),
  c("HMP1_healthy", "Gem2024"),
  c("HMP1_healthy", "Heidrich_2023"),
  c("HMP1_healthy", "Ingham_2021"),
  c("HMP1_healthy", "Kambara2025"),
  c("HMP1_healthy", "Oku2020"),
  c("HMP1_healthy", "Rashidi2025"),
  c("HMP1_healthy", "Raychaudhuri_2025"),
  c("HMP1_healthy", "Shouval2020"),
  c("HMP1_healthy", "Shtossel2025"))
for (comp in comparisons_cohort) {
  ref_source <- comp[1]
  target_source <- comp[2]
  metadata_filtered <- metadata %>% filter(Study %in% c(ref_source, target_source))
  common_ids <- intersect(metadata_filtered$Sample_ID, rownames(metagenome))
  merged_data_filtered <- metagenome[common_ids, ]
  metadata_filtered <- metadata_filtered %>% filter(Sample_ID %in% common_ids)
  output_dir <- paste0(ref_source, "_vs_", target_source)  
  maaslin3(
    input_data = merged_data_filtered,
    input_metadata = metadata_filtered,
    normalization = "TSS",
    transform = "LOG",
    output = output_dir,
    fixed_effects = c("Study"),
    reference = paste0("Study,", ref_source))}
######Maaslin3 result processing######
load("Fig4C.RData")
all_data <- lapply(names(comparisons), function(name) {
  file <- comparisons[[name]]
  if (file.exists(file)) {
    df <- read_tsv(file)
    process_df(df, name)
  } else {
    warning(paste("File not found:", file))
    NULL}})
all_data <- all_data[!sapply(all_data, is.null)]
all_data <- lapply(all_data, function(df) {
  df_clean <- df[complete.cases(df), ]
  return(df_clean)})
all_species <- Reduce(union, lapply(all_data, rownames))
all_data_filled <- lapply(all_data, fill_missing_species, all_species = all_species)
merged_masslin3 <- do.call(cbind, all_data_filled) %>% select(-c(ends_with("value"), ends_with("model")))
rownames(mean_abundance_cohort)<- mean_abundance_cohort$Study
mean_abundance_cohort<- as.data.frame(t(mean_abundance_cohort))
TABLE_massslin3<-merge(mean_abundance_cohort,merged_masslin3,by="row.names")
TABLE_massslin3$Masslin <- apply(TABLE_massslin3[, 15:25], 1, function(x) sum(x != 0, na.rm = TRUE))
merge_data_paper<- merge(metadata,metagenome,by="row.names")
merge_data_paper <- merge_data_paper[, -c(1:2, 4:21)]
result <- merge_data_paper %>%
  group_by(Study) %>%
  summarise(across(
    everything(),
    list(
      median = ~mean(., na.rm = TRUE),
      q1 = ~quantile(., probs = 0.25, na.rm = TRUE),
      q3 = ~quantile(., probs = 0.75, na.rm = TRUE)))) %>%
  pivot_longer(
    cols = -Study,              
    names_to = c("Variable", "Statistic"),
    names_sep = "_",              
    values_to = "Value")
#rm(list = setdiff(ls(), c("result","TABLE_massslin3")))

####### Data input preparation ######
TABLE_massslin3$ave_coef <- rowMeans(TABLE_massslin3[, 15:25], na.rm = TRUE)
TABLE_massslin_1 <- TABLE_massslin3 %>% filter(Masslin %in% c(6:11))
TABLE_massslin_1 <- TABLE_massslin_1 %>% arrange(desc(abs(ave_coef))) %>% slice_head(n = 10)  
TABLE_massslin3_sorted <- dplyr::bind_rows(TABLE_massslin_1  %>% mutate(Group = "abs(Coef) top10"))
common_rows <- intersect(TABLE_massslin3_sorted$Row.names, result$Variable)
filtered_result <- result[result$Variable %in% common_rows, ]
TABLE_massslin3_sorted_fil<-TABLE_massslin3_sorted[,c(1,15:25)]
TABLE_massslin3_sorted_fil <- TABLE_massslin3_sorted_fil %>% rename_with(~ str_remove(., "\\.y$")) 
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

####### Filtering Fig4C data ######
boxplot_data_filtered <- boxplot_data %>%
  inner_join(valid_combinations %>% dplyr::select(Variable, Study), 
             by = c("Variable", "Study")) %>%
  bind_rows(
    boxplot_data %>% filter(Study == "HMP1_healthy")) %>%distinct()
boxplot_merge_data<-merge(boxplot_data_filtered,TABLE_massslin3_sorted[,c(1,14,27,28)],by.x="Variable",by.y="Row.names")
boxplot_data_filtered$Study
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
boxplot_merge_data$Variable <- factor(boxplot_merge_data$Variable,levels = rev (sorted_variables$Variable))

####### Figure generation ######
Fig4C <- ggplot(boxplot_merge_data, aes(x = Variable, y = median)) +
  scale_y_log10(breaks=c(1e-6,1e-5,1e-4,1e-3,1e-2),labels=c("1e-6","1e-5","1e-4","1e-3","1e-2")) +
  geom_boxplot(fill = "white", color = "black", outlier.shape = NA, width = 0.6) +
  geom_jitter(aes(color = Study), size = 1.5,  alpha = 1, position = position_jitterdodge( jitter.width = 0, dodge.width = 0)) +
  geom_point(data = subset(boxplot_merge_data, Study == "HSCT"),
             aes(color = Study), size = 1.5, alpha = 1,  
             position = position_jitterdodge(jitter.width = 0,dodge.width = 0 )) +
  scale_fill_manual(values = color_mapping,
                    labels = c("HMP1","HSCT","Ingham2021", "Heidrich2023", "Faraci2024", "Raychaudhuri2025",
                               "Oku2023","Rashidi2025", "Shouval2020" ,"Shtossel2025","Kambara2025","Gem2024" ),
                    breaks = names(color_mapping),name = "Disease") +
  scale_color_manual(values = color_mapping,
                     labels = c("HMP1_table","HSCT","Ingham2021", "Heidrich2023", "Faraci2024", "Raychaudhuri2025",
                                "Oku2023","Rashidi2025", "Shouval2020" ,"Shtossel2025","Kambara2025","Gem2024" ),
                     breaks = names(color_mapping),name = "Disease") +
  labs(x = NULL, y = "Relative Abundance") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position = "right")
Fig4C
