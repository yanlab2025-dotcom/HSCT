###############This is data for Fig 5D####
rm(list = ls())
library("mediation")
library("dplyr")
library("readxl")
library("openxlsx")
load("Fig5D.RData")
################################################################################
#As the full mediation analysis across all 64 groups was computationally intensive, we performed a targeted analysis specifically for
#Prevotella melaninogenica within the eight subgroups corresponding to Fig5D.
################################################################################
#Fixed mediation parameters
set.seed(123)
target_feature <- "Prevotella_melaninogenica"
tm <- "Pre-HSCT"
outcome_to_run <- "Death" 
base_covs <- c("Age", "Gender", "BMI", "Disease_group")
categories <- c("All", "No-infection", "Low-score", "Batch+Infection")
directions <- c("Forward", "Reverse")
################################################################################
#Loop over the 8 group combinations
pm_final_results <- list()
for (cat_name in categories) {
  message(paste("\n>>> Processing Category:", cat_name))
  tm_meta_sub <- metadata[metadata$Curated_sampling == tm, ]
  if (cat_name == "No-infection") {
    tm_meta_sub <- tm_meta_sub[tm_meta_sub$Infection == "No", ]
  } else if (cat_name == "Low-score") {
    tm_meta_sub <- tm_meta_sub[tm_meta_sub$AUS_group == "low_score", ]
  }
  common_ids <- intersect(tm_meta_sub$Sample_ID, colnames(Feature_norm))
  common_ids <- intersect(common_ids, Abx_use_filtered$Sample_ID)
  if (length(common_ids) < 10) {
    message(paste("Skip", cat_name, "- sample size too small."))
    next
  }
  mic_values <- log(as.numeric(Feature_norm[target_feature, common_ids]) + 1e-6)
  curr_dis <- tm_meta_sub[match(common_ids, tm_meta_sub$Sample_ID), ]
  curr_abx <- Abx_use_filtered[match(common_ids, Abx_use_filtered$Sample_ID), ]
  for (direc in directions) {
    message(paste("    Direction:", direc))
    tryCatch({
      df_loop <- data.frame(
        Mic = mic_values,
        outcome = as.factor(curr_dis[[outcome_to_run]]),
        Abx = as.numeric(curr_abx$AUS),
        Age = as.numeric(curr_dis$Age),
        Gender = as.factor(curr_dis$Gender),
        BMI = as.numeric(curr_dis$BMI),
        Disease_group = as.factor(curr_dis$Disease_group))
      current_covs <- base_covs
      if (cat_name == "Batch+Infection") {
        df_loop$Infection <- as.factor(curr_dis$Infection)
        current_covs <- c(base_covs, "Infection")
      }
      df_loop[is.na(df_loop)] <- 0
      if (length(unique(df_loop$outcome)) < 2) next
      if (direc == "Forward") {
        c_val <- quantile(df_loop$Abx, 0.25)
        t_val <- quantile(df_loop$Abx, 0.75)
        m1 <- glm(Mic ~ Abx + Age + Gender + BMI + Disease_group, 
                  data = df_loop, family = gaussian(link = "identity"))
        m2 <- glm(outcome ~ Abx + Mic + Age + Gender + BMI + Disease_group, 
                  data = df_loop, family = binomial("logit"))
        med_out <- mediate(m1, m2, treat = "Abx", mediator = "Mic", 
                           sims = 500, boot = TRUE,
                           control.value = c_val, treat.value = t_val, 
                           boot.ci.type = "bca")
      } else {
        df_loop$Abx <- df_loop$Abx + 1  
        m1 <- lm(Abx ~ Mic + Age + Gender + BMI + Disease_group, data = df_loop)
        m2 <- glm(outcome ~ Mic + Abx + Age + Gender + BMI + Disease_group, 
                  data = df_loop, family = binomial("logit"))
        med_out <- mediate(m1, m2, treat = "Mic", mediator = "Abx", 
                           sims = 500, boot = FALSE) 
      }
      res_key <- paste(cat_name, direc, sep = "_")
      pm_final_results[[res_key]] <- summary(med_out)
    }, error = function(e) {
      message(paste("Error in", cat_name, direc, ":", e$message))
    })
  }
}
################################################################################
#Extract mediation results
res_list <- list()
for (res_name in names(pm_final_results)) {
  s <- pm_final_results[[res_name]]
  tmp_df <- data.frame(
    group = res_name,
    coef_mediate = s$d.avg,
    Pval_mediate = s$d.avg.p,
    coefCI_mediate_low = s$d.avg.ci[1],
    coefCI_mediate_high = s$d.avg.ci[2],
    coef_direct = s$z.avg,
    Pval_direct = s$z.avg.p,
    coef_total = s$tau.coef,
    Pval_total = s$tau.p)
  res_list[[res_name]] <- tmp_df
}
pm_summary_table <- do.call(rbind, res_list)
pm_summary_table <- pm_summary_table[order(pm_summary_table$Pval_mediate), ]
