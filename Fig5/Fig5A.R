###########This is data for Fig5a###############
rm(list = ls())
library(ggplot2)   
library(dplyr)      
library(vegan)     
library(ggpubr)     
library(patchwork)  
library(scales)  
load("Fig5.RData")
############################################################################
#---------------------------Mediation analysis-------------
result <- expand.grid(AES = "AUS", 
                      colnames =  unique(colnames(dt_species)),
                      #a="Pseudomonas_aeruginosa",
                      outcome = c("PFS"))


colnames(result) <- c("ABX", "dt_species", "outcome")
set.seed(123)
for (i in 1: nrow(result)) { 
  tryCatch({
    data <- data.frame(
      cbind(
        dt_log[, as.character(result$dt_species[i])],
        diseases[, as.character(result$outcome[i])],
        Abx_use_filtered[, as.character(result$ABX[i])],
        cov
      )
    )
    data[is.na(data)] <- "0"
    colnames(data) <- c("Mic", "outcome", "Abx", "Age","Gender", "BMI", "Disease_group")
    data$Mic <- as.numeric(data$Mic)
    data$Abx <- as.factor(data$Abx)
    data$Age <- as.numeric(data$Age)
    data$Gender <- as.factor(data$Gender)
    data$Disease_group <- as.factor(data$Disease_group)
    data$outcome <- as.factor(data$outcome)
    
    # med
    model.m <- glm(Mic ~ Abx+Age + Gender + BMI,
                   data = data, family = gaussian(link = "identity")) 
    model.y <- glm(outcome ~ Abx + Mic+Age + Gender + BMI,
                   data = data, family = binomial(link = "logit")) 
    med <- mediate(model.m, model.y, treat = "Abx", mediator = "Mic", 
                   sims = 500, boot = TRUE, 
                   
                   control.value = "low_score", treat.value = "high_score",
                   boot.ci.type = "bca")
    med.summary <- summary(med)
    
    result$Pval_mediate[i] <- med.summary$d.avg.p
    result$coef_mediate[i] <- med.summary$d.avg
    result$coefCI_mediate_low[i] <- ifelse(length(med.summary$d.avg.ci) == 2, med.summary$d.avg.ci[1], NA)  
    result$coefCI_mediate_high[i] <- ifelse(length(med.summary$d.avg.ci) == 2, med.summary$d.avg.ci[2], NA)  
    
    result$Pval_direct[i] <- med.summary$z.avg.p
    result$coef_direct[i] <- med.summary$z.avg
    result$coefCI_direct_low[i] <- ifelse(length(med.summary$z.avg.ci) == 2, med.summary$z.avg.ci[1], NA)  
    result$coefCI_direct_high[i] <- ifelse(length(med.summary$z.avg.ci) == 2, med.summary$z.avg.ci[2], NA)  
    
    result$Pval_total[i] <- med.summary$tau.p
    result$coef_total[i] <- med.summary$tau.coef
    result$coefCI_total_low[i] <- ifelse(length(med.summary$tau.ci) == 2, med.summary$tau.ci[1], NA)  
    result$coefCI_total_high[i] <- ifelse(length(med.summary$tau.ci) == 2, med.summary$tau.ci[2], NA)  
    
    result$Pval_ratio[i] <- med.summary$n.avg.p
    result$coef_ratio[i] <- med.summary$n.avg
    result$coefCI_ratio_low[i] <- ifelse(length(med.summary$n.avg.ci) == 2, med.summary$n.avg.ci[1], NA)  
    result$coefCI_ratio_high[i] <- ifelse(length(med.summary$n.avg.ci) == 2, med.summary$n.avg.ci[2], NA)  
    
    
    print(paste("Iteration", i, "completed successfully."))
    
  }, error = function(e) {
    message(paste("Iteration", i, "failed with error:", e$message))
  })
}

getwd()
result$Qval_mediate=p.adjust(result$Pval_mediate,method = "BH")
############################################################################
# data input preparation 
    filtered_AUS <- result[result$Pval_mediate < 0.05, ]

    filtered_AUS <- filtered_AUS[order(abs(filtered_AUS[, 3]), decreasing = FALSE), ]

    filtered_AUS <- filtered_AUS[order(abs(filtered_AUS[, 3]), decreasing = FALSE), ]
    filtered_AUS <- filtered_AUS %>%
      mutate(
        species_label = paste(dt_species)
      )
    filtered_AUS$species_label <- factor(filtered_AUS$species_label, levels = rev(unique(filtered_AUS$species_label)))
    filtered_AUS <- filtered_AUS[filtered_AUS$outcome == "PFS", ]
    filtered_AUS$Pval_mediate
############################################################################
# mediation coefficient plot
    p_med <- ggplot(filtered_AUS, aes(x = coef_mediate, 
                                      y = species_label, 
                                      xmin = coefCI_mediate_low, 
                                      xmax = coefCI_mediate_high)) +
      geom_point(shape = 18, size = 8,color="#808080ff") +
      geom_errorbarh(height = 0.2,color="#808080ff") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#808080ff") +   
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black", size = 0.8),  
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),
        axis.text.x = element_text(size = 6),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black", size = 0.5),    
        axis.ticks.length = unit(0.2, "cm")                       
      ) +
      labs(
        x = "Mediation coefficient",
        y = NULL
      ) +
      scale_y_discrete(position = "left") 
    p_med

# ridge plot
    p <- ggplot(data=zll$nz, aes(group=factor(y), fill=factor(AUS_group_numeric), color=factor(AUS_group_numeric))) + 
      geom_ridgeline(aes(height=height, x=x, y=y), alpha = 0.75) +
      zeroplots+
      scale_fill_manual(values = c("NO" = "#0072B2", "Yes" = "#D55E00"), name="PFS") +
      scale_color_manual(values = c("NO" = "#0072B2", "Yes" = "#D55E00"), name="PFS")+
      labs(
        x = "Log10 (relative abundance to non-PFS median)",
        y = NULL,
        title = NULL
      ) +
      scale_y_continuous(expand=c(0, 0),
                         limits=c(0.5, 1+length(levels(df$feature)))) +
      scale_x_continuous(expand=c(0, 0), 
                         limits=c(min(limits[1], suppressWarnings(min(zll$z$xmin)-zw*0.35)), limits[2])) +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.6)
      ) 
    p

# combine plots
    Fig5a <- grid.arrange(p_med, p, ncol = 2, widths = c(1, 0.6))  
    Fig5a
