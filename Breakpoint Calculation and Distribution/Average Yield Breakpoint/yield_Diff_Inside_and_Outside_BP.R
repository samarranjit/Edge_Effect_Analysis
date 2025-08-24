library(tidyverse)  # Includes ggplot2, dplyr, etc.
library(plotly)     # For interactive plots
library(DT)         # For interactive tables


Analyse_Edge_Difference_Inside_and_Outside_BreakPoint <- function() {
  

avg_yield_on_different_points_based_on_breakpoints <- read.xlsx(file.path(results_dir,"Results/Average Yield BreakPoint/BreakPoints_.xlsx"))
# View(avg_yield_on_different_points_based_on_breakpoints)
yield_data <- avg_yield_on_different_points_based_on_breakpoints

yield_data <- yield_data %>% filter(Crop %in% c("SOYBEANS", "CORN", "WHEAT"))

# Create a new column for yield difference (inside - outside)
yield_data <- yield_data %>%
  mutate(Yield_Difference = Yield_Inside_BreakPoint - Yield_Outside_BreakPoint)

# View(yield_data)
# Create a basic visualization function
create_yield_plots <- function(data, selected_crop = NULL) {
  
  # Filter by crop if specified
  if (!is.null(selected_crop)) {
    data <- data %>% filter(Crop == selected_crop)
  }
  
  # Create scatter plot showing the difference
  p <- ggplot(data, aes(x = BreakPoint, y = Yield_Difference, 
                        color = Crop, 
                        text = paste("Field:", FieldName,
                                     "<br>Crop:", Crop,
                                     "<br>Breakpoint:", BreakPoint,
                                     "<br>Inside Yield:", round(Yield_Inside_BreakPoint, 2),
                                     "<br>Outside Yield:", round(Yield_Outside_BreakPoint, 2),
                                     "<br>Difference:", round(Yield_Difference, 2)))) +
    geom_point(alpha = 1.2, size = 2.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", size = 1.2) +
    labs(title = "Yield Difference (Inside vs Outside Breakpoint)",
         subtitle =paste("Crop: " ,(if (is.null(selected_crop)) "All" else selected_crop) ) ,
         x = "Breakpoint Value",
         y = "Yield Difference (Inside - Outside)") +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#2C3E50", hjust = 0.5),
      plot.subtitle = element_text(size = 8, color = "#7F8C8D", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#BDC3C7", fill = NA)
    )+
    # scale_y_continuous(breaks = seq( round(min(data$Yield_Difference, na.rm = TRUE), 0), round(max(data$Yield_Difference, na.rm = TRUE), 0), by = 2)) +
    scale_x_continuous(breaks = seq(0, round(max(data$BreakPoint, na.rm = TRUE), 0), by = 10)) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text( size = 12 ,hjust = 0.5, angle = 30),
      axis.text.y = element_text(angle =0 , size = 12 ,hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold")
    )
  
  # Convert to interactive plotly
  p_interactive <- ggplotly(p, tooltip = "text")
  
  return(p)
}

# Create a summary plot showing average differences by crop
create_summary_plot <- function(data) {
  summary_data <- data %>% 
    group_by(Crop) %>%
    summarize(
      Avg_Diff = mean(Yield_Difference, na.rm = TRUE),
      Count = n(),
      Positive_Count = sum(Yield_Difference > 0, na.rm = TRUE),
      Positive_Percent = round(Positive_Count / Count * 100, 1)
    )
  
  p <- ggplot(summary_data, aes(x = reorder(Crop, Avg_Diff), y = Avg_Diff, 
                                fill = Crop,
                                text = paste("Crop:", Crop,
                                             "<br>Avg Difference:", round(Avg_Diff, 2),
                                             "<br>Fields:", Count,
                                             "<br>Fields with Positive Diff:", Positive_Count, 
                                             "(", Positive_Percent, "%)"))) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
    labs(title = "Average Yield Difference by Crop Type",
         x = "Crop",
         y = "Average Yield Difference (Inside - Outside)") +
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text( size = 12 ,hjust = 0.5, angle = 30),
      axis.text.y = element_text(angle =0 , size = 12 ,hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold")
    )+
    coord_flip()
  return(p)
}

# Usage examples:

# View summary of all crops
summary_plot <- create_summary_plot(yield_data)
summary_plot

# View all data points
all_crops_plot <- create_yield_plots(yield_data)
all_crops_plot

#================================================================================
# Writing to pdf
# pdf(file = "C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge_effect_analysis2/inner outer yield diff/CORN_Inside_VS_Outside_BreakPoint.pdf",
#     width = 10, height = 7)  # optional: set plot size
# 
# print(all_crops_plot)
# 
# dev.off()
#================================================================================
# View only CORN data
corn_plot <- create_yield_plots(yield_data, "CORN")
corn_plot

# View only SOYBEANS data
soybean_plot <- create_yield_plots(yield_data, "SOYBEANS")
soybean_plot



# View only WHEAT data
wheat_plot <- create_yield_plots(yield_data, "WHEAT")
wheat_plot

return (
  list(
    all_crops_plot = all_crops_plot,
    soybean_plot = soybean_plot,
    corn_plot = corn_plot,
    wheat_plot = wheat_plot
    
  )
)

}

