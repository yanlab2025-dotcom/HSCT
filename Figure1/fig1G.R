###############This is data for Fig 1G#######
rm(list = ls())
library(dplyr)
library(ggplot2)
load("Fig1.RData")
#####################################################################################
#Calculate the AUS score
    merge$duration <- as.numeric(as.character(merge$duration))
    merge$Spectrum_score <- as.numeric(as.character(merge$Spectrum_score))
    merge$AUS_single <- merge$duration * merge$Spectrum_score
    summarized_data <- merge %>%
      group_by(PID) %>%
      summarise(AUS = sum(AUS_single, na.rm = TRUE)) 
    summarized_data <- rbind(summarized_data, new_rows) #For individuals who did not use antibiotics, AUS was assigned a value of 0
    AUS_median <- median(summarized_data$AUS, na.rm = TRUE)

#figure generation
    Fig1G <-ggplot(data = summarized_data, aes(x = AUS)) + 
      geom_histogram(aes(y = ..density..), binwidth = 200, fill = "#333333", alpha = 0.3) + 
      geom_density(fill = "#4682B4", alpha = 0.6) + 
      geom_vline(aes(xintercept = AUS_median), color = "#FF0000", linetype = "dashed") + 
      labs( x = "AUS", y = "Density") +  
      theme_minimal() +  
      theme(plot.title = element_text(hjust = 0.5),  
            axis.title = element_text(size = 12),     
            axis.text = element_text(size = 10),   
            axis.ticks = element_line(color = "black", size = 0.33),
            axis.ticks.length = unit(0.1, "cm"),
            axis.text.x = element_text(size = 10),
            axis.line.x = element_line(size = 0.33),  
            axis.line.y = element_line(size = 0.33),
            panel.grid = element_blank(),             
            axis.line = element_line(color = "black", linewidth = 0.8))
    Fig1G