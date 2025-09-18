###########This is data for Fig 6e#####################
library(survMisc)
library(survival)
library(survminer)
###############################################################################
cox_data<-cox_data[,c("Prevotella_corporis","Capnocytophaga_sputigena","Stenotrophomonas_maltophilia","PFS_time","PFS")]

for (bug in  colnames(cox_data)[2:3]) {
bug<-"Prevotella_corporis"
bug_result <- as.data.frame(cutPMethodAndCox(cox_data, time = "death_time", event = "death", biomarker = bug))

cut_point <- bug_result[1, 1]

cox_data$bug_g <- ifelse(cox_data[[bug]] >= cut_point, "high", "low")

surv_obj <- Surv(cox_data$death_time, cox_data$death)

fit <- survfit(surv_obj ~ bug_g, data = cox_data)

fig6e <- ggsurvplot(
  fit,
  data = cox_data,
  pval = TRUE,                 
  conf.int = TRUE,            
  #risk.table = TRUE,            
  xlab = "Time",                 
  ylab = "Survival probability", 
  title = "Kaplan-Meier Curve for Death",  
  legend.title = bug,         
  legend.labs = c("high", "low") , 
  palette = c("#FDAE61", "#74ADD1"),
  risk.table.ylab = "Number at Risk" 
  )
fig6e
}
