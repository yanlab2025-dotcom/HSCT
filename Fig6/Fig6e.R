###########This is data for Fig 6e#####################
library(survival)
library(survminer)
library(dplyr)
library(survMisc)
###############################################################################
# 01 define function
cutPMethodAndCox <- function(df, time, event, biomarker) {
  new_df <- data.frame(
    biomarker = df[[biomarker]],  
    time = df[[time]],             
    event = df[[event]]         
  )
  
  tryCatch({
    cph1 <- coxph(Surv(time, event) ~ biomarker, data = new_df)
    cut_result <- cutp(cph1)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!exists("cut_result")) return(NULL)
  
  allCutpointsDf <- data.frame(cut_result[[1]])

  if(nrow(allCutpointsDf) == 0) return(NULL)
  
  allCutpointsDf$cox_p_value <- NA
  allCutpointsDf$HR <- NA
  allCutpointsDf$CI_lower <- NA
  allCutpointsDf$CI_upper <- NA
  allCutpointsDf$n_high <- NA
  allCutpointsDf$n_low <- NA
  
  for (i in 1:nrow(allCutpointsDf)) {
    cutpoint_val <- allCutpointsDf[i, 1] 
    
    new_df$Group <- ifelse(new_df$biomarker >= cutpoint_val, "High", "Low")
    
    counts <- table(new_df$Group)
    if (length(counts) < 2 || min(counts) < 3) {
      next 
    }
    
    n_high <- if("High" %in% names(counts)) counts["High"] else 0
    n_low <- if("Low" %in% names(counts)) counts["Low"] else 0
    
    # Low = reference
    new_df$Group <- factor(new_df$Group, levels = c("Low", "High"))
    
    tryCatch({
      # Cox
      cox_model <- coxph(Surv(time, event) ~ Group, data = new_df)
      summ <- summary(cox_model)
      
      allCutpointsDf[i, "cox_p_value"] <- summ$coefficients[5] # Wald P
      allCutpointsDf[i, "HR"] <- summ$conf.int[1]
      allCutpointsDf[i, "CI_lower"] <- summ$conf.int[3]
      allCutpointsDf[i, "CI_upper"] <- summ$conf.int[4]
      allCutpointsDf[i, "n_high"] <- n_high
      allCutpointsDf[i, "n_low"] <- n_low
      
    }, error = function(e) {
    })
  }
  
  valid_cuts <- allCutpointsDf[!is.na(allCutpointsDf$cox_p_value), ]
  
  if (nrow(valid_cuts) == 0) {
    return(NULL)
  }
  
  best_cut <- valid_cuts[order(valid_cuts$cox_p_value, decreasing = FALSE), ][1, ]
  
  return(data.frame(
    cutpoint = best_cut[[1]],
    p_value = best_cut$cox_p_value,
    HR = best_cut$HR,
    CI_lower = best_cut$CI_lower,
    CI_upper = best_cut$CI_upper,
    n_high = best_cut$n_high,
    n_low = best_cut$n_low
  ))
}

###############################################################################
# 02 load data: abundance and PFS

data <- readxl::read_excel("HSCT_data.xlsx", sheet = 12) ## relative abundance and PFS
data <- as.data.frame(data)
row.names(data) <- data$Sample_id  ## set row.names

metadata <- as.data.frame(cbind(Sample_id = data$Sample_id, PFS = data$PFS, PFS_time = data$PFS_time))
abundance <- data[,c(2,3,4)]

## sqrt-transfer
dt_species <- apply(abundance, 2, function(x) asin(sqrt(x / 100)))
dt_species<- as.data.frame(dt_species) # dim = 100*3

## merge species abundance and metadata
dt_species <- dt_species %>%
  tibble::rownames_to_column("Sample_id") %>%
  left_join(metadata[, c("Sample_id", "PFS", "PFS_time")], by = "Sample_id") %>%
  tibble::column_to_rownames("Sample_id")

D <- dt_species
D$PFS <- as.numeric(D$PFS)
D$PFS_time <- as.numeric(D$PFS_time)

###############################################################################
# 03 find the best cutpoint
plots_PFS <- list()
results_PFS <- data.frame(bug = character(), p_value = numeric(),
                            HR = numeric(),           
                            CI_lower = numeric(),     
                            CI_upper = numeric(),     
                            n_high = numeric(), n_low = numeric(),
                            cutpoint = numeric())

all_bugs <- colnames(abundance)

plots_PFS <- list()
results_PFS <- data.frame() 

for (bug in all_bugs) {
  bug_g <- bug

  best_res <- cutPMethodAndCox(D, time = "PFS_time", event = "PFS", biomarker = bug)
  
  if (is.null(best_res)) {
    next
  }
  
  cutpoint <- best_res$cutpoint
  D$bug_g <- ifelse(D[[bug]] >= cutpoint, "High", "Low")
  D$bug_g <- factor(D$bug_g, levels = c("Low", "High")) # 保持 Low 为参考
  
  surv_obj <- Surv(D$PFS_time, D$PFS)
  fit <- survfit(surv_obj ~ bug_g, data = D)
  
  title_text <- paste0(bug, "\nCutpoint: ", round(cutpoint, 4), 
                       "\nHR: ", round(best_res$HR, 2), 
                       " (p=", signif(best_res$p_value, 3), ")")
  
  PFS <- ggsurvplot(
    fit,
    data = D,
    pval = TRUE,                   
    conf.int = TRUE,               
    risk.table = TRUE,             
    xlab = "Time",                 
    ylab = "Survival probability", 
    title = title_text,
    legend.title = "Group",          
    legend.labs = c("Low", "High"),
    palette = c("#2E9FDF", "#E7B800") 
  )
  
  plots_PFS[[length(plots_PFS) + 1]] <- PFS
  
  new_row <- data.frame(
    bug = bug,
    p_value = best_res$p_value,
    HR = best_res$HR,
    n_high = best_res$n_high,
    n_low = best_res$n_low,
    cutpoint = cutpoint
  )
  results_PFS <- rbind(results_PFS, new_row)
}

# results: cutpoint, HR and Wald p-value
results_PFS

###############################################################################
# 04 plot
## (1) bug = "Prevotella_corporis"
target_bug <- "Prevotella_corporis"
fixed_cutpoint <- 0.0005230973

D$bug_g <- ifelse(D[[target_bug]] >= fixed_cutpoint, "High", "Low")
D$bug_g <- factor(D$bug_g, levels = c("Low", "High")) # Low as the reference

cox_fit <- coxph(Surv(PFS_time, PFS) ~ bug_g, data = D)
summ <- summary(cox_fit)

hr <- summ$conf.int[1]
p_val <- summ$coefficients[5]

surv_fit <- survfit(Surv(PFS_time, PFS) ~ bug_g, data = D)

p_Prevotella_corporis <- ggsurvplot(
  surv_fit,
  data = D,
  pval = TRUE,                
  conf.int = TRUE,            
  palette = c("#FDAE61", "#74ADD1"), 
  legend.labs = c("Low", "High"),
  legend.title = "Abundance",
  xlab = "Time (Days)",
  ylab = "Survival probability",
  title = target_bug,
  ggtheme = theme_light()
)
p_Prevotella_corporis

## (2) bug = "Capnocytophaga_sputigena"

target_bug <- "Capnocytophaga_sputigena"
fixed_cutpoint <- 0.0019999288
D$bug_g <- ifelse(D[[target_bug]] >= fixed_cutpoint, "High", "Low")
D$bug_g <- factor(D$bug_g, levels = c("Low", "High")) # Low as the reference

cox_fit <- coxph(Surv(PFS_time, PFS) ~ bug_g, data = D)
summ <- summary(cox_fit)

hr <- summ$conf.int[1]
p_val <- summ$coefficients[5]

surv_fit <- survfit(Surv(PFS_time, PFS) ~ bug_g, data = D)

p_Capnocytophaga_sputigena <- ggsurvplot(
  surv_fit,
  data = D,
  pval = TRUE,                
  conf.int = TRUE,            
  palette = c("#FDAE61", "#74ADD1"), 
  legend.labs = c("Low", "High"),
  legend.title = "Abundance",
  xlab = "Time (Days)",
  ylab = "Survival probability",
  title = target_bug,
  ggtheme = theme_light()
)

p_Capnocytophaga_sputigena


## (3) bug = "Stenotrophomonas_maltophilia"

target_bug <- "Stenotrophomonas_maltophilia"
fixed_cutpoint <- 0.0002839890

D$bug_g <- ifelse(D[[target_bug]] >= fixed_cutpoint, "High", "Low")
D$bug_g <- factor(D$bug_g, levels = c("Low", "High")) # Low as the reference

cox_fit <- coxph(Surv(PFS_time, PFS) ~ bug_g, data = D)
summ <- summary(cox_fit)

hr <- summ$conf.int[1]
p_val <- summ$coefficients[5]

surv_fit <- survfit(Surv(PFS_time, PFS) ~ bug_g, data = D)

p_Stenotrophomonas_maltophilia <- ggsurvplot(
  surv_fit,
  data = D,
  pval = TRUE,                
  conf.int = TRUE,            
  palette = c("#FDAE61", "#74ADD1"), 
  legend.labs = c("Low", "High"),
  legend.title = "Abundance",
  xlab = "Time (Days)",
  ylab = "Survival probability",
  title = target_bug,
  ggtheme = theme_light()
)

p_Stenotrophomonas_maltophilia

