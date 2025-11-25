###############This is data for Fig 4f#######
library(vegan)     
library(tidyr)
library(ggplot2)
library(lubridate) 
load("Fig4f.RData")
#####################################################################################
#data input preparation
    theme_bw <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                base_rect_size = base_size/22) 
    {theme_grey(base_size = base_size, base_family = base_family, 
                base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", colour = NA), 
                panel.border = element_rect(fill = NA, colour = "black"), panel.grid = element_line(colour = "grey92"), 
                panel.grid.minor = element_line(linewidth = rel(0.66)), 
                strip.background = element_rect(fill = "grey85", 
                colour = "black", size = 0.66), complete = TRUE)}

#figure generation
    Fig4e <- ggplot(acti_data_filtered, aes(x = start_diff, xend = stop_diff, y = Antibiotic_1, yend = Antibiotic_1, color = In_out)) +
                geom_segment(size = 1) +  
                geom_vline(aes(xintercept = 0, linetype = "Tx_date"), color = "darkgreen", size = 0.33) +  
                geom_vline(aes(xintercept = Sampling_date_diff, linetype = "Sampling_Date"), color = "purple", size = 0.33) +  # Sampling_Date
                scale_color_manual(
                values = c("In" = "#9cd2fbff", "Out" = "#55b0faff"),   #"In" represents "Pre-HSCT" "Out" represents "Post-HSCT"
                name = "In_out" ) +
    geom_point(data = subset(acti_data_filtered, start_diff == stop_diff),
                aes(x = start_diff, y = Antibiotic_1, color = In_out),
                shape = 18, size = 2)+
    scale_linetype_manual(
                values = c("Tx_date" = "dashed", "Sampling_Date" = "dotted"),  
                name = "date", 
                labels = c("Tx_date" = "Tx_date", "Sampling_Date" = "Sampling_Date")) +
    facet_wrap(~ PID, ncol = 4) +  
    theme_bw() +
    scale_x_continuous(breaks = c(-30, -15, 0, 15),
                labels = c("-30", "-15", "0", "15"))+
                labs(x = "Days") +
    theme(
                legend.position = "none", 
                legend.title = element_blank(), 
                legend.text = element_blank(), 
                axis.text.x = element_text(size = 9,color = "black"),  
                axis.text.y = element_text(size = 9,color = "black"),   
                axis.title.x = element_text(size = 9,color = "black"), 
                axis.title.y = element_blank(), 
                strip.text = element_text(size=9,color = "black"), 
                panel.grid = element_blank(),
                panel.border = element_rect(color = "black", size = 0.66), 
                axis.ticks.x = element_line(size = 0.33, colour = "black"),
                axis.ticks.y = element_line(size = 0.33, colour = "black"),   
                axis.ticks.length = unit(0.1, "cm") ) 
    print(Fig4e)

#####################################################################################
#data input preparation
    plot_taxa_flow <- function(subject_id, matched_rows_combined, custom_colors) {
    P_data <- matched_rows_combined[, matched_rows_combined["Subject_ID", ] == subject_id]
    P_data <- as.data.frame(P_data)
    colnames(P_data) <- P_data["In_out", ] 
    P_data <- P_data[-which(rownames(P_data) %in% c("Subject_ID", "In_out")), ]
    P_data$Taxa <- rownames(P_data)
    P_data <- P_data[, c("Taxa", setdiff(names(P_data), "Taxa"))]
    P_data$In <- as.numeric(P_data$In)
    P_data$Out <- as.numeric(P_data$Out)
    top_25_data <- P_data %>%
                arrange(desc(Out)) %>%
                head(25)
    final_data <- top_25_data
    rownames(final_data) <- final_data$Taxa
    colnames(final_data)[which(names(final_data) == "In")] <- "Weigh_In"
    colnames(final_data)[which(names(final_data) == "Out")] <- "Weigh_Out"
    total_height <- max(sum(final_data$Weigh_In, na.rm = TRUE), sum(final_data$Weigh_Out, na.rm = TRUE))

# prepare pre-HSCT data
    in_data <- final_data %>%
                arrange(desc(Weigh_In)) %>%
                mutate(
                Taxa = factor(Taxa, levels = sort(unique(Taxa))),
                Weigh_scaled = Weigh_In / sum(Weigh_In, na.rm = TRUE) * total_height,
                ymin = rev(cumsum(rev(Weigh_scaled))) - Weigh_scaled,
                ymax = ymin + Weigh_scaled,
                xmin = 0.4, xmax = 0.6)

# prepare post-HSCT data
    out_data <- final_data %>%
    mutate(Taxa = factor(Taxa, levels = in_data$Taxa)) %>%
    arrange(Taxa) %>%
    mutate(Weigh_scaled = Weigh_Out / sum(Weigh_Out, na.rm = TRUE) * total_height,
                ymin = rev(cumsum(rev(Weigh_scaled))) - Weigh_scaled,
                ymax = ymin + Weigh_scaled,
                xmin = 2.4, xmax = 2.6)
    connection_data <- full_join(in_data, out_data, by = "Taxa", suffix = c("_in", "_out")) %>%
                rowwise() %>% mutate(
                x = list(c(xmax_in, xmin_out, xmin_out, xmax_in)),
                y = list(c(ymax_in, ymax_out, ymin_out, ymin_in)),
                group = as.numeric(Taxa)) %>%unnest(c(x, y))

#figure generation
    p <- ggplot() +
                geom_polygon(data = connection_data, aes(x = x, y = y, group = group, fill = Taxa), alpha = 0.6, color = NA) +
                geom_rect(data = in_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Taxa), color = "black", size = 0.33) +
                geom_rect(data = out_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Taxa), color = "black", size = 0.33) +
                scale_fill_manual(values = custom_colors) +
                labs(x = "", y = "", title = subject_id) +
                theme_void() +
                theme(plot.title = element_text(hjust = 0.5, size = 10),
                legend.position = "right",
                legend.key.size = unit(0.4, "cm"))
   return(p) }

# generate plots for selected subjects
    subject_ids <- c("P131","P039","P077")
    plot_list <- list()
    for (subject_id in subject_ids) {
    p <- plot_taxa_flow(subject_id, matched_rows_combined, custom_colors = custom_colors_sorted)
    plot_list[[subject_id]] <- p}
    plot_list[["P131"]]
    plot_list[["P039"]]
    plot_list[["P077"]]
