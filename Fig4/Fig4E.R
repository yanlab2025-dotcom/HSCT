library(ggplot2)
library(tidyr)
library(readxl)
library(compositions)
library(glmnet)
library(dplyr)


# This is a file for 
# load data
load("Fig4E.RData")

# filter
row_sums <- rowSums(rarefactin_data)
rarefactin_data_percentage <- sweep(rarefactin_data, 1, row_sums, FUN = "/") 
filtered_data <- rarefactin_data_percentage[, 
                                            apply(rarefactin_data_percentage, 2, function(x) sum(x > 0.0001) > 0.1 * nrow(rarefactin_data_percentage))]
rownames(filtered_data) <- gsub("\\.", "-", rownames(filtered_data))


# get sum
species_sums <- colSums(filtered_data)

# ordering
sorted_species <- sort(species_sums, decreasing = TRUE)
selected_columns <- c("PID", "aGVHD", "aGVHD_time", "PFS", "PFS_time", 
                      "death", "death_time", "AUS_model_0new", 
                      "AUS_group_death_PFS_model_0new","number")

# subset
summarized_data <- summarized_data[, selected_columns]
# 
filtered_data_In <- filtered_data[rownames(filtered_data) %in% summarized_data$number, ]

summarized_data <- summarized_data %>%
  rename(
    AUS_group = AUS_group_death_PFS_model_0new,
    AUS = AUS_model_0new
  )
head(summarized_data)

filtered_data_In <- filtered_data_In %>% 
  tibble::rownames_to_column("number")
merged_data <- summarized_data %>%
  left_join(filtered_data_In, by = "number")
head(merged_data)

# 
microbe_columns <- colnames(merged_data)[11:ncol(merged_data)]
# 
coefficient_results_PFS <- data.frame()
# 
merged_data_clean <- na.omit(merged_data[, c("PFS", "AUS", microbe_columns)])
# 
for (microbe in microbe_columns) {
  tryCatch({
    # glm model
    formula <- as.formula(paste("PFS ~ AUS +", microbe))
    model <- glm(formula, data = merged_data_clean, family = binomial())
    # 
    model_summary <- summary(model)
    coefficients_table <- model_summary$coefficients
    ci <- confint(model, method = "Wald")
    # 
    for (term in rownames(coefficients_table)) {
      coefficient_results_PFS <- rbind(coefficient_results_PFS, data.frame(
        Microbe_Model = microbe,
        Term = term,
        Estimate = coefficients_table[term, "Estimate"],
        Std_Error = coefficients_table[term, "Std. Error"],
        Z_value = coefficients_table[term, "z value"],
        P_value = coefficients_table[term, "Pr(>|z|)"],
        OR = exp(coefficients_table[term, "Estimate"]),
        OR_95CI_lower = exp(ci[term, 1]),
        OR_95CI_upper = exp(ci[term, 2]),
        N = nobs(model),  
        stringsAsFactors = FALSE
      ))
    }
    
  }, error = function(e) {
    NULL
  })
}

# get abundance
X <- merged_data_clean[, microbe_columns]
# CLR transform
X_clr <- clr(X + 1e-6)

# y: PFS outcome
y <- merged_data_clean$PFS

# LASSO
fit_lasso_clr <- cv.glmnet(
  x = as.matrix(X_clr),
  y = y,
  family = "binomial",
  alpha = 1
  )

lasso_clr_coef <- coef(fit_lasso_clr, s = "lambda.min")
result_lasso_clr <- data.frame(
  Feature = row.names(lasso_clr_coef),
  Coef = as.numeric(lasso_clr_coef)
) %>%
  filter(Coef != 0 & Feature != "(Intercept)")

result_lasso_clr

# get df
coefficient_results_death <- data.frame()
merged_data_clean <- na.omit(merged_data[, c("death", "AUS", microbe_columns)])

# calculate per species 
for (microbe in microbe_columns) {
  tryCatch({
    # model：outcome ~ AUS + species
    formula <- as.formula(paste("death ~ AUS +", microbe))
    model <- glm(formula, data = merged_data_clean, family = binomial())
    model_summary <- summary(model)
    coefficients_table <- model_summary$coefficients
    ci <- confint(model, method = "Wald")
    # 
    for (term in rownames(coefficients_table)) {
      coefficient_results_death <- rbind(coefficient_results_death, data.frame(
        Microbe_Model = microbe,
        Term = term,
        Estimate = coefficients_table[term, "Estimate"],
        Std_Error = coefficients_table[term, "Std. Error"],
        Z_value = coefficients_table[term, "z value"],
        P_value = coefficients_table[term, "Pr(>|z|)"],
        OR = exp(coefficients_table[term, "Estimate"]),
        OR_95CI_lower = exp(ci[term, 1]),
        OR_95CI_upper = exp(ci[term, 2]),
        N = nobs(model),  
        stringsAsFactors = FALSE
      ))
    }
    
  }, error = function(e) {
    NULL  
  })
}


# 
coefficient_results_aGVHD <- data.frame()
# 
merged_data_clean <- na.omit(merged_data[, c("aGVHD", "AUS", microbe_columns)])
# 
for (microbe in microbe_columns) {
  tryCatch({
    # model：aGVHD ~ AUS + species
    formula <- as.formula(paste("aGVHD ~ AUS +", microbe))
    model <- glm(formula, data = merged_data_clean, family = binomial())
    model_summary <- summary(model)
    coefficients_table <- model_summary$coefficients
    ci <- confint(model, method = "Wald")
    #
    for (term in rownames(coefficients_table)) {
      coefficient_results_aGVHD <- rbind(coefficient_results_aGVHD, data.frame(
        Microbe_Model = microbe,
        Term = term,
        Estimate = coefficients_table[term, "Estimate"],
        Std_Error = coefficients_table[term, "Std. Error"],
        Z_value = coefficients_table[term, "z value"],
        P_value = coefficients_table[term, "Pr(>|z|)"],
        OR = exp(coefficients_table[term, "Estimate"]),
        OR_95CI_lower = exp(ci[term, 1]),
        OR_95CI_upper = exp(ci[term, 2]),
        N = nobs(model),  
        stringsAsFactors = FALSE
      ))
    }
    
  }, error = function(e) {
    NULL  
  })
}

#
coefficient_results_aGVHD[,11] <- rep("aGvHD", dim(coefficient_results_aGVHD)[1])
colnames(coefficient_results_aGVHD)[11] <- "outcome"
coefficient_results_aGVHD <- subset(coefficient_results_aGVHD, P_value < 0.05 )
coefficient_results_aGVHD <- coefficient_results_aGVHD[!grepl("intercept", coefficient_results_aGVHD$Term, ignore.case = TRUE), ]
coefficient_results_aGVHD[,"log10_abs_estimate"] <- log10(abs(coefficient_results_aGVHD$Estimate))
coefficient_results_aGVHD[,"sign_of_estimate"] <- sign(coefficient_results_aGVHD$Estimate)
coefficient_results_aGVHD[,"aGVHD_coef"] <- coefficient_results_aGVHD[,"log10_abs_estimate"] * coefficient_results_aGVHD[,"sign_of_estimate"] 
coefficient_results_aGVHD <- coefficient_results_aGVHD[, !names(coefficient_results_aGVHD) %in% c("N", "outcome")]
# 
coefficient_results_aGVHD <-  coefficient_results_aGVHD[order(coefficient_results_aGVHD$P_value, decreasing = TRUE), ]
#
coefficient_results_death[,11] <- rep("Death", dim(coefficient_results_death)[1])
colnames(coefficient_results_death)[11] <- "outcome"
coefficient_results_death <- subset(coefficient_results_death, P_value < 0.05 )
coefficient_results_death <- coefficient_results_death[!grepl("intercept", coefficient_results_death$Term, ignore.case = TRUE), ]
coefficient_results_death[,"log10_abs_estimate"] <- log10(abs(coefficient_results_death$Estimate))
coefficient_results_death[,"sign_of_estimate"] <- sign(coefficient_results_death$Estimate)
coefficient_results_death[,"aGVHD_coef"] <- coefficient_results_death[,"log10_abs_estimate"] * coefficient_results_death[,"sign_of_estimate"] 
coefficient_results_death <- coefficient_results_death[, !names(coefficient_results_death) %in% c("N", "outcome")]
#
coefficient_results_death <-  coefficient_results_death[order(coefficient_results_death$P_value, decreasing = TRUE), ]

#
coefficient_results_PFS[,11] <- rep("PFS", dim(coefficient_results_PFS)[1])
colnames(coefficient_results_PFS)[11] <- "outcome"
coefficient_results_PFS <- subset(coefficient_results_PFS, P_value < 0.05 )
coefficient_results_PFS <- coefficient_results_PFS[!grepl("intercept", coefficient_results_PFS$Term, ignore.case = TRUE), ]
coefficient_results_PFS[,"log10_abs_estimate"] <- log10(abs(coefficient_results_PFS$Estimate))
coefficient_results_PFS[,"sign_of_estimate"] <- sign(coefficient_results_PFS$Estimate)
coefficient_results_PFS[,"aGVHD_coef"] <- coefficient_results_PFS[,"log10_abs_estimate"] * coefficient_results_PFS[,"sign_of_estimate"] 
coefficient_results_PFS <- coefficient_results_PFS[, !names(coefficient_results_PFS) %in% c("N", "outcome")]
#
coefficient_results_PFS <-  coefficient_results_PFS[order(coefficient_results_PFS$P_value, decreasing = TRUE), ]

#d efine species list
species_list <- unique(c(coefficient_results_PFS$Microbe_Model, coefficient_results_death$Microbe_Model, coefficient_results_aGVHD$Microbe_Model))
species_list <- gsub("_", " ", species_list)

# 
study_median_long$aGvHD_binary<- as.factor(study_median_long$aGvHD_binary)

# define plot colors
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

# data input preparation
species_vec <- unique(study_median_long$species)
a_species <- gsub("_", " ", coefficient_results_aGVHD$Term)
d_species <- gsub("_", " ", coefficient_results_death$Term)
p_species <- gsub("_", " ", coefficient_results_PFS$Term)
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

# figure generation
fig4E <- ggplot(study_median_long, 
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
  labs(x = "species aGVHD PFS Death", y = "Mean Abundance") +
  theme_classic() +
  theme(axis.text.y  = element_text( size = 10), 
        axis.text.x  = element_text( size = 10,angle = 90), 
        axis.title.x = element_text( size = 10),  
        legend.position = "none")
fig4E
