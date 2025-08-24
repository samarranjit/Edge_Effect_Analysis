#Playing with the breakpoint figures that we arleady have

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read Excel file
breakpoint_data <- read_excel(file.path(results_dir,"Results/Average Yield BreakPoint/BreakPoints_.xlsx"), sheet = 1)

View(breakpoint_data)


{
  # Remove missing or extreme BreakPoint values
  breakpoint_data_clean <- breakpoint_data %>%
    filter(!is.na(BreakPoint) & Crop %in% c("SOYBEANS", "CORN", "WHEAT")) %>%
    filter(BreakPoint < 50, BreakPoint >5)  # optional cutoff to remove extreme values

  # Make the boxplot#BreakPoint Step 3: Make the boxplot
  ggplot(breakpoint_data_clean, aes(x = Crop, y = BreakPoint)) +
    geom_boxplot(aes(fill = Crop), color = "black", outlier.color = "red", width = 0.75) +
    geom_point()+
    labs(title = "BreakPoint Distance by Crop",
         subtitle = "Breakpoints Obtained based on the Average Yield",
         x = "Crop",
         y = "BreakPoint Distance (m)") +
    scale_y_continuous(breaks = seq(0, max(breakpoint_data$BreakPoint), by = 10)) +
    scale_fill_manual(
      values = c(
        "SOYBEANS" = "skyblue",
        "CORN" = "lightgreen",
        "WHEAT" = "lightyellow"
      )
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.text.x = element_text( size = 12),
      axis.text.y  = element_text( size = 12),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.position = "none"
    )




  # Calculate summary statistics grouped by Crop
  summary_table <- breakpoint_data_clean %>%
    group_by(Crop) %>%
    summarise(
      Count = n(),
      Mean = mean(BreakPoint, na.rm = TRUE),
      Median = median(BreakPoint, na.rm = TRUE),
      SD = sd(BreakPoint, na.rm = TRUE),
      Min = min(BreakPoint, na.rm = TRUE),
      Max = max(BreakPoint, na.rm = TRUE)
    ) %>%
    arrange(desc(Count))  # optional: sort by sample size

  # View summary
  print(as.matrix(summary_table))
  
}



  
  
  {
    
    # Average Yield Difference
    breakpoint_data <- breakpoint_data %>% filter(!is.na(Yield_Inside_BreakPoint) & !is.na(Yield_Outside_BreakPoint)  & !is.na('Overall_Yield_Average')  & Crop %in% c("SOYBEANS", "CORN", "WHEAT")) %>%
      filter(BreakPoint < 50)


    breakpoint_data['Yield_Difference'] = breakpoint_data['Yield_Inside_BreakPoint'] - breakpoint_data['Yield_Outside_BreakPoint']

    breakpoint_data['relative_yield_difference'] = 100 * breakpoint_data['Yield_Difference'] / breakpoint_data['Overall_Yield_Average']

    View(breakpoint_data)

    ggplot(breakpoint_data, aes(x = Crop, y = Yield_Difference)) +
      geom_boxplot(aes(fill = Crop), color = "black", outlier.color = "red",alpha = 0.8, width =0.5) +
      geom_point()+
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 1.25) +
    scale_fill_manual(
      values = c(
        "SOYBEANS" = "skyblue",
        "CORN" = "lightgreen",
        "WHEAT" = "lightyellow"
      )
    ) +
      labs(title = "Relative Yield Difference",
           subtitle = "Relative Difference = 100 * (Yield Inside the Breakpoint - Yield Outside the Breakpoint) / Mean Yield   \n Breakpoints Obtained based on the Average Yield",
           x = "Crop",
           y = "Relative Yield Difference (% of Field Average)") +
      scale_y_continuous(breaks = seq( floor(min(breakpoint_data$Yield_Difference, na.rm = TRUE) / 10) * 10, ceiling(max(breakpoint_data$Yield_Difference, na.rm = TRUE) / 10) * 10, by = 10)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5, margin = margin(t = 10, b = 15), lineheight = 1.25),
        axis.text.x = element_text( size = 12),
        axis.text.y  = element_text( size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )



    # Calculate summary statistics grouped by Crop
    summary_table <- breakpoint_data %>%
      group_by(Crop) %>%
      summarise(
        Count = n(),
        "Mean Yield Difference "= mean(relative_yield_difference, na.rm = TRUE),
        "Median Yield Difference "= median(relative_yield_difference, na.rm = TRUE),
        SD = sd(relative_yield_difference, na.rm = TRUE),
        Min = min(relative_yield_difference, na.rm = TRUE),
        Max = max(relative_yield_difference, na.rm = TRUE)
      ) %>%
      arrange(desc(Count))  # optional: sort by sample size

    # View summary
    print(as.matrix(summary_table))

    library(openxlsx)
    write.xlsx(summary_table, file = "Z:/temp/tempdata.xlsx", sheetName = "Sheet2", rowNames = FALSE)
    
    }
  
  





