################# This is data for Fig 6A,B ################# 

library(brms)
library(pROC)
library(ggplot2)
library(dplyr)
load("Fig6A_B.RData")

###############################################################################

# --- 01 Setting priors ---
custom_priors <- c(
  prior(double_exponential(0, 0.5), class = "b"), 
  prior(double_exponential(0, 1), class = "Intercept")
)

# --- 02 MICRO_LOW model ---
train_data_Low$Target <- as.factor(train_data_Low$Target)
Brm_Low <- brm(Target ~ ., data = train_data_Low, family = bernoulli(),
               prior = custom_priors, chains = 4, warmup = 1000, iter = 2000,
               seed = 12345, refresh = 0, backend = "cmdstanr")

Brm_Low_train <- roc(train_data_Low$Target, fitted(Brm_Low)[, "Estimate"], ci = TRUE)
roc_test_brm_LOW <- roc(test_data_Low$Target, fitted(Brm_Low, newdata = test_data_Low)[, "Estimate"], ci = TRUE)


# --- 03 MICRO_ALL model ---
train_data_ALL$Target <- as.factor(train_data_ALL$Target)
Brm_ALL <- brm(Target ~ ., data = train_data_ALL, family = bernoulli(),
               prior = custom_priors, chains = 4, warmup = 1000, iter = 2000,
               seed = 12345, refresh = 0, backend = "cmdstanr")

Brm_ALL_train <- roc(train_data_ALL$Target, fitted(Brm_ALL)[, "Estimate"], ci = TRUE)
roc_test_Brm_ALL <- roc(test_data_ALL$Target, fitted(Brm_ALL, newdata = test_data_ALL)[, "Estimate"], ci = TRUE)


# --- 04 MICRO+IMMUNE_LOW model ---
train_data_LOW_IM$Target <- as.factor(train_data_LOW_IM$Target)
Brm_Low_im <- brm(Target ~ ., data = train_data_LOW_IM, family = bernoulli(),
                  prior = custom_priors, chains = 4, warmup = 1000, iter = 2000,
                  seed = 12345, refresh = 0, backend = "cmdstanr")

Brm_Low_train_im <- roc(train_data_LOW_IM$Target, fitted(Brm_Low_im)[, "Estimate"], ci = TRUE)
roc_test_brm_LOW_im <- roc(test_data_LOW_IM$Target, fitted(Brm_Low_im, newdata = test_data_LOW_IM)[, "Estimate"], ci = TRUE)


# --- 05 MICRO+IMMUNE_ALL model ---
train_data_ALL_IM$Target <- as.factor(train_data_ALL_IM$Target)
Brm_ALL_im <- brm(Target ~ ., data = train_data_ALL_IM, family = bernoulli(),
                  prior = custom_priors, chains = 4, warmup = 1000, iter = 2000,
                  seed = 12345, refresh = 0, backend = "cmdstanr")

Brm_ALL_train_im <- roc(train_data_ALL_IM$Target, fitted(Brm_ALL_im)[, "Estimate"], ci = TRUE)
roc_test_Brm_ALL_im <- roc(test_data_ALL_IM$Target, fitted(Brm_ALL_im, newdata = test_data_ALL_IM)[, "Estimate"], ci = TRUE)


# --- 06 Merge dataset ---
rm(list = setdiff(ls(), c("Brm_ALL_train", "roc_test_Brm_ALL", "roc_test_brm_LOW", "Brm_Low_train",
                          "Brm_ALL_train_im", "roc_test_Brm_ALL_im", "roc_test_brm_LOW_im", "Brm_Low_train_im")))


# --- 07 Figure generation ---

plot_roc_custom <- function(roc_list, title_suffix = "Set") {
  
  roc_df_list <- list()
  annotation_texts <- list()
  y_positions <- c(0.25, 0.20, 0.15, 0.10)
  
  color_values <- c("#CC5800", "#FED789FF", "#51C3CC", "#A4BED5FF")
  
  for (i in seq_along(roc_list)) {
    model_name <- names(roc_list)[i]
    roc_obj <- roc_list[[model_name]]
    
    auc_val <- round(auc(roc_obj), 2)
    label <- paste0(model_name, " (AUC=", auc_val, ")")
    
    roc_df_list[[model_name]] <- data.frame(
      tpr = roc_obj$sensitivities,
      fpr = 1 - roc_obj$specificities,
      model = label
    )
    
    annotation_texts[[label]] <- list(text = label, y = y_positions[i], color = color_values[i])
  }
  
  roc_df_all <- bind_rows(roc_df_list) %>% arrange(tpr)
  unique_labels <- names(annotation_texts)
  model_colors <- setNames(color_values, unique_labels)
  
  # plot
  p <- ggplot(roc_df_all, aes(x = fpr, y = tpr, color = model)) +
    geom_line(size = 1.3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey80") +
    scale_color_manual(values = model_colors) +
    theme_minimal() +  
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "black", size = 0.5),   
      legend.position = "none",
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.72), 
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12)
    ) +
    labs(x = "False Positive Rate", y = "True Positive Rate", title = paste("ROC Curve -", title_suffix))
  
  # annotation
  for (label in names(annotation_texts)) {
    ann <- annotation_texts[[label]]
    p <- p + annotate("text", x = 0.55, y = ann$y, label = ann$text,
                      color = ann$color, size = 3.5, hjust = 0, fontface = "bold")
  }
  
  return(p)
}

# ========================== Fig 6A generation ==========================
train_roc_list <- list(
  "ALL_train_im" = Brm_ALL_train_im,
  "ALL_train"    = Brm_ALL_train,
  "Low_train_im" = Brm_Low_train_im,
  "Low_train"    = Brm_Low_train
)

Fig6A <- plot_roc_custom(train_roc_list, title_suffix = "Training")
print(Fig6A)

# ========================== Fig 6B generation ==========================
test_roc_list <- list(
  "ALL_test_im" = roc_test_Brm_ALL_im,
  "ALL_test"    = roc_test_Brm_ALL,
  "Low_test_im" = roc_test_brm_LOW_im,
  "Low_test"    = roc_test_brm_LOW
)

Fig6B <- plot_roc_custom(test_roc_list, title_suffix = "Test")
print(Fig6B)

