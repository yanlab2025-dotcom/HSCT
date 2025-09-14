###########This is data for Fig 6a_b#####################
rm(list=ls()) 
library(pROC)
library(ggplot2)
load("Fig6.RData")
###############################################################################
#ROC objects for training set models
roc_list <- list(
  "ALL_train_im" = Brm_ALL_train_im,
  "ALL_train" = Brm_ALL_train,
  "Low_train_im" = Brm_Low_train_im,
  "Low_train" = Brm_Low_train
)
roc_df_list <- list()
for (model_name in names(roc_list)) {
  roc_obj <- roc_list[[model_name]]
  auc_val <- round(auc(roc_obj), 2)
  ci_val <- ci.auc(roc_obj)
  label <- paste0(model_name, " (AUC=", auc_val,
                  ", 95%CI: ", round(ci_val[1], 2), "-", round(ci_val[3], 2), ")")
  df <- data.frame(
    tpr = roc_obj$sensitivities,
    fpr = 1 - roc_obj$specificities,
    model = label
  )
  roc_df_list[[model_name]] <- df
}
annotation_texts <- list()
y_positions <- c(0.25, 0.20, 0.15, 0.10)  
i <- 1  
for (model_name in names(roc_list)) {
  roc_obj <- roc_list[[model_name]]
  auc_val <- round(auc(roc_obj), 2)
  ci_val <- ci.auc(roc_obj)
  label <- paste0(model_name, " (AUC=", auc_val, ")")
  df <- data.frame(
    tpr = roc_obj$sensitivities,
    fpr = 1 - roc_obj$specificities,
    model = label
  )
  roc_df_list[[model_name]] <- df
  annotation_texts[[model_name]] <- list(
    text = label,
    y = y_positions[i]
  )
  i <- i + 1
}
roc_df_all <- bind_rows(roc_df_list)
roc_df_all <- roc_df_all[order(roc_df_all$tpr), ]
unique_models <- unique(roc_df_all$model)
model_colors <- setNames(c( "#CC5800", "#FED789FF","#51C3CC", "#A4BED5FF"), unique_models)#PFS

#ROC curve plot for training set
Fig6A <-ggplot(roc_df_all, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey80") +
  scale_color_manual(values = model_colors) +
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.5),  
    legend.position = "none",
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.72),  
    legend.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  labs(x = "False Positive Rate", y = "True Positive Rate", color = "Model") 

# add annotations      
for (model_name in names(annotation_texts)) {
  ann <- annotation_texts[[model_name]]
  Fig6A <- Fig6A + annotate("text", x = 0.6, y = ann$y, label = ann$text,
                            color = model_colors[ann$text], size = 3.2, hjust = 0)
}
Fig6a
###############################################################################
#ROC objects for testing set models
      roc_list <- list(
        "ALL_test_im" = roc_test_Brm_ALL_im,
        "ALL_test" = roc_test_Brm_ALL,
        "Low_test_im" = roc_test_brm_LOW_im,
        "Low_test" = roc_test_brm_LOW
      )
      roc_df_list <- list()
      for (model_name in names(roc_list)) {
        roc_obj <- roc_list[[model_name]]
        auc_val <- round(auc(roc_obj), 2)
        ci_val <- ci.auc(roc_obj)
        label <- paste0(model_name, " (AUC=", auc_val,
                        ", 95%CI: ", round(ci_val[1], 2), "-", round(ci_val[3], 2), ")")
        df <- data.frame(
          tpr = roc_obj$sensitivities,
          fpr = 1 - roc_obj$specificities,
          model = label
        )
        roc_df_list[[model_name]] <- df
      }
      annotation_texts <- list()
      y_positions <- c(0.25, 0.20, 0.15, 0.10)  
      i <- 1  
      for (model_name in names(roc_list)) {
        roc_obj <- roc_list[[model_name]]
        auc_val <- round(auc(roc_obj), 2)
        ci_val <- ci.auc(roc_obj)
        label <- paste0(model_name, " (AUC=", auc_val, ")")
        df <- data.frame(
          tpr = roc_obj$sensitivities,
          fpr = 1 - roc_obj$specificities,
          model = label
        )
        roc_df_list[[model_name]] <- df
        annotation_texts[[model_name]] <- list(
          text = label,
          y = y_positions[i]
        )
        i <- i + 1
      }
      roc_df_all <- bind_rows(roc_df_list)
      roc_df_all <- roc_df_all[order(roc_df_all$tpr), ]
      unique_models <- unique(roc_df_all$model)
      model_colors <- setNames(c( "#CC5800", "#FED789FF","#51C3CC", "#A4BED5FF"), unique_models)#PFS

#ROC curve plot for testing set
      Fig6B <- ggplot(roc_df_all, aes(x = fpr, y = tpr, color = model)) +
        geom_line(size = 1.3) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey80") +
        scale_color_manual(values = model_colors) +
        theme_minimal() +  
        theme(
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black", size = 0.5),  
          legend.position = "none",
          panel.border = element_rect(fill = NA, color = "black", linewidth = 0.72),  
          legend.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12)
        ) +
        labs(x = "False Positive Rate", y = "True Positive Rate", color = "Model") 

# add annotations      
       for (model_name in names(annotation_texts)) {
        ann <- annotation_texts[[model_name]]
        Fig6B <- Fig6B + annotate("text", x = 0.6, y = ann$y, label = ann$text,
                            color = model_colors[ann$text], size = 3.2, hjust = 0)
      }
      Fig6b
