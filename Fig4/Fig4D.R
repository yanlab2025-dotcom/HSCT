###############This is data for Fig 4D####
rm(list=ls()) 
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
load("fig4D.RData")
##################################################
#Prepare data for figure
pre_points <- final_plot_data %>%
  filter(Var1 == "Pre_Discovery") %>%
  mutate(Study = gsub("^Pre_|^Post_", "", Var2),
         Type = ifelse(grepl("^Pre_", Var2), "Pre_Study", "Post_Study"))
#reorder factors
pre_result <- pre_result %>% arrange(mean_correlation)
pre_points$Study <- factor(pre_points$Study, levels = pre_result$Study)
pre_result$Study <- factor(pre_result$Study, levels = pre_result$Study)
##################################################
#figure generation
p1 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(data = pre_points, aes(x = Correlation, y = Study, color = Type), 
             size = 3.5, alpha = 0.9) +
  geom_point(data = pre_result, aes(x = mean_correlation, y = Study), 
             shape = 18, size = 5, color = "#B22222") +
  scale_color_manual(values = c("Pre_Study" = "#FFE082", "Post_Study" = "#FF8F00")) +
  labs(x = "Correlation Difference (Low - ALL)", 
       y = "", 
       title = "Pre_Discovery") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "none")
p1
##################################################
#Prepare data for figure
post_points <- final_plot_data %>%
  filter(Var1 == "Post_Discovery") %>%
  mutate(Study = gsub("^Pre_|^Post_", "", Var2),
         Type = ifelse(grepl("^Pre_", Var2), "Pre_Study", "Post_Study"))
#reorder factors
post_result <- post_result %>% arrange(mean_correlation)
post_points$Study <- factor(post_points$Study, levels = post_result$Study)
post_result$Study <- factor(post_result$Study, levels = post_result$Study)
##################################################
#figure generation
p2<-ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(data = post_points, aes(x = Correlation, y = Study, color = Type), 
             size = 3.5, alpha = 0.9) +
  geom_point(data = post_result, aes(x = mean_correlation, y = Study), 
             shape = 18, size = 5, color = "#B22222") +
  scale_color_manual(values = c("Pre_Study" = "#FFE082", "Post_Study" = "#FF8F00")) +
  labs(x = "Correlation Difference (Low - ALL)", 
       y = "", 
       title = "Post_Discovery") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.position = "right" )
p2
combined_plot <- c(p1 + p2)
print(combined_plot)

