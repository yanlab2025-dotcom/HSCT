###########This is data for Fig 6e#####################
library(survMisc)
library(survival)
library(survminer)
###############################################################################
cox_data<-cox_data[,c("Prevotella_corporis","Capnocytophaga_sputigena","Stenotrophomonas_maltophilia","PFS_time","PFS")]
D <- cox_data

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

