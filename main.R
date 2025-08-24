#========================================================================================================================================
# This file contains links to all the scripts. For more information on each script, please refer to the script files inside respective folders.

# Created: 2024-06-24
# Samar Ranjit 
#========================================================================================================================================

library(sf) # For spatial operations and buffering
library(raster) # For raster handling
library(terra)
library(dplyr) # For data manipulation
library(ggplot2) # For visualization
library(purrr) # For functional programming
library(lme4) # For mixed-effects modeling
library(patchwork) # For combining ggplots
library(gridExtra) # For displaying tables
library(grid)
library(performance)
library(multcompView)
library(segmented)
library(viridis)
library(leaflet)
library(openxlsx)
library(patchwork)
library(ggplot2)
library(concaveman)
library(dotenv)

readRenviron(".env")

#Load Data Path
data_path <- Sys.getenv("DATA_PATH")
results_path <- Sys.getenv("RESULTS_PATH")


if(dir.exists(paste0(results_path)) == FALSE || dir.exists(paste0(data_path)) == FALSE){
  stop("Data or Results path does not exist. Please check your .env file")
}

# ===================================================================================================================
# Data Overview
# ===================================================================================================================

#==================== Box Plot for all Fields Average ===========================
# source("Data Overview/ScriptToEvaluateMeanOfTop3CropsAndBoxPlot.R") # <--------------- Uncomment

#==================== Yearwise Favorability Analysis ===========================
# source("Data Overview/Year Favorability Study.R") #<---------------- Uncomment



#=====================================================================================================================
# Average Breakpoint Calculation and Analysis over the entire period
# Plots the Box Plot of the overall period Analysis, Detects breakpoint using segmented regression and davies test
# and returns the yield and breakpoint values
#=====================================================================================================================
#Uncomment the below line to run the average breakpoint analysis

{
  # source("Average Yield Breakpoint/Average_Breakpoint_Calculation.R")
  #   for (n in 1: nrow(summary_all_Field_data) ) {
  # 
  #     print(paste("Processing field:", summary_all_Field_data$field[n]))
  #   field_name = summary_all_Field_data$field[n]
  #   crop_name = summary_all_Field_data$crop[n]
  #     tryCatch({
  #       BreakPointResults <- run_average_breakpoint_analysis(base_dir= results_path ,field_name = field_name, crop_name = crop_name)
  #        dir.create(path =file.path(results_path, paste0("Yield at 5m distance/")) )
  #        ggsave(BreakPointResults$box_plot, file = file.path(results_path, paste0("Yield at 5m distance/",field_name, "_", crop_name, "_5mDistanceBins_boxplot.pdf")), device = pdf)
  # 
  #       # dir.create(path =file.path(results_path, paste0("Field Wise/",field_name)), showWarnings = FALSE, recursive = TRUE )
  #       # ggsave(BreakPointResults$box_plot, file = file.path(results_path, paste0("Field Wise/",field_name, "/",field_name, "_" ,crop_name, "_5mDistanceBins_boxplot.pdf")), device = pdf)
  # 
  #       if(!is.null(BreakPointResults$points_in_breakpoint_region)){
  #         # dir.create(path =file.path(results_path, paste0("Average Yield BreakPoint/",field_name)), showWarnings = FALSE, recursive = TRUE )
  #         # ggsave(BreakPointResults$breakpoint_plot, file = file.path(results_path, paste0("Average Yield BreakPoint/",field_name, "/",field_name, "_", crop_name, "_AverageYieldBreakPoint.pdf")), device = pdf)
  #         # ggsave(BreakPointResults$breakpoint_plot, file = file.path(results_path, paste0("Field Wise/",field_name, "/",field_name, "_" ,crop_name, "__AverageYieldBreakPoint.pdf")), device = pdf)
  # 
  #         message("🟢🟢🟢 Writing BreakPoint to the file")
  #         print(BreakPointResults)
  #         resultToSave=data.frame(
  #           FieldName = field_name,
  #           Crop = crop_name,
  #           BreakPoint = BreakPointResults$breakpoint_val,
  #           PValue = BreakPointResults$davies_pvalue,
  #           Yield_Inside_BreakPoint = BreakPointResults$avg_of_different_regions[["avg_points_inside_breakpoint_region"]],
  #           Yield_Outside_BreakPoint = BreakPointResults$avg_of_different_regions[["avg_points_outside_breakpoint_region"]],
  #           Overall_Yield_Average = BreakPointResults$overall_years_field_yield_average,
  #           Overall_Yield_Varaince = BreakPointResults$overall_years_field_yield_variance
  # 
  #         )
  #         existingRows <- read.xlsx(file.path(results_path, "Average Yield Breakpoint/BreakPoints_.xlsx"), colNames = TRUE)
  #         updatedData <- rbind(existingRows, resultToSave)
  #         write.xlsx(updatedData, file =file.path(results_path, "Average Yield Breakpoint/BreakPoints_.xlsx"), overwrite = TRUE )
  # 
  # 
  # 
  #       }
  #       else{
  #         message("🟡🟡🟡 No BreakPoint found")
  #         noBP = data.frame(
  #           FieldName = field_name,
  #           Crop = crop_name,
  #           PValue = BreakPointResults$davies_pvalue
  #         )
  #         existingRows <- read.xlsx(file.path(results_path, "Average Yield Breakpoint/NoBreakPoints_.xlsx"), colNames = TRUE)
  #         updatedData <- rbind(existingRows, noBP)
  #         write.xlsx(updatedData, file = file.path(results_path, "Average Yield Breakpoint/NoBreakPoints_.xlsx"), overwrite = TRUE )
  # 
  # 
  # 
  # 
  #       }
  #     }
  # 
  # , error = function(err){
  #   message <- paste0(
  #     "\n🔴🔴🔴ERROR LOG 🔴🔴🔴\n",
  #     "Time: ", Sys.time(), "\n",
  #     "Context: ", "EdgeDetectionthenLeaflet.R", "\n",
  #     "Field Name: ", summary_all_Field_data$field[n] , "\n",
  #     "Field Name: ", summary_all_Field_data$crop[n] , "\n",
  #     "Message: ", err, "\n",
  #     "-----------------------------------------------\n\n\n"
  #   )
  #   cat(message, file = "log.txt", append = TRUE)
  #   message("An error occurred. Logged to error_log.txt.")
  #   message(err$message)
  # })
  # 
  #   # stop()
  # 
  # }
}



#=====================================================================================================================
# Analysing how the breakpoints vary year on year; and analyse the yield differences at brekapoints
#=====================================================================================================================
{
  # source("Breakpoint Calculation and Distribution/Average Yield Breakpoint/Analysing the Average Yield Breakpoints and Yield Difference.R") 
  
  # source("Breakpoint Calculation and Distribution/Average Yield Breakpoint/yield_Diff_Inside_and_Outside_BP.R) <--------------- Uncomment
  # Analyse_Edge_Difference_Inside_and_Outside_BreakPoint <- Analyse_Edge_Difference_Inside_and_Outside_BreakPoint()
  # source(Analyse_Edge_Difference_Inside_and_Outside_BreakPoint/edgeDiffInsideandOutsideBP.R)
  #
  #
  # #================================================================================
  # #Writing to pdf
  # pdf(file = file.path(base_dir, "Yield Comparison between inside and outside of breakpoint/YieldComparisonWithBreakPoints.pdf"),
  #     width = 10, height = 7)  # optional: set plot size
  #
  # print(Analyse_Edge_Difference_Inside_and_Outside_BreakPoint$all_crops_plot)
  # print(Analyse_Edge_Difference_Inside_and_Outside_BreakPoint$corn_plot)
  # print(Analyse_Edge_Difference_Inside_and_Outside_BreakPoint$soybean_plot)
  # print(Analyse_Edge_Difference_Inside_and_Outside_BreakPoint$wheat_plot)
  #
  # dev.off()
  # #================================================================================
}



#=====================================================================================================================
# Calculating Annual Changes in breakpoints
#=====================================================================================================================


# Uncomment the below line to run the annual breakpoint analysis

{
  source("Breakpoint Calculation and Distribution/Annual BreakPoint/YearlyEdgeBreakPoint.R")
  # for (n in 1: nrow(summary_all_Field_data) ) {
  #   
  #   print(paste("Processing field:", summary_all_Field_data$field[n]))
  #   field_name = summary_all_Field_data$field[n]
  #   crop_name = summary_all_Field_data$crop[n]
  #   AllYears = summary_all_Field_data$years[n] %>% strsplit(",") %>% unlist() %>% trimws() %>%   as.numeric()
  #   AllYearsData = AllYears
  #   print("This is all years data that we have")
  #   print(AllYearsData)
  #   
  #   # tryCatch({
  #   
  #   AverageFunction <- run_average_breakpoint_analysis(base_dir = data_dir, field_name = field_name, crop_name =  crop_name)
  #   avg_yield_map_wtih_breakpoint <- AverageFunction$avg_yield_map_wtih_breakpoint
  #   shared_scale_components <- AverageFunction$shared_scale_components
  #   # Save average plot
  #   
  #   # dir.create(path = file.path("D:/study/Yearly Yield BreakPoint", field_name, crop_name), recursive = TRUE, showWarnings = FALSE)
  #   # dir.create(path =file.path("D:/study/Field Wise", field_name, "Yearly Yield BreakPoint", crop_name), recursive = TRUE, showWarnings = FALSE)
  #   
  #   # ggsave(avg_yield_map_wtih_breakpoint, filename = file.path("D:/study/Yearly Yield BreakPoint", field_name,crop_name, paste0(field_name, "_", crop_name, "_Average_Yield_BreakPoint.pdf")), device = "pdf")
  #   # ggsave(avg_yield_map_wtih_breakpoint, filename = file.path("D:/study/Field Wise", field_name, "Yearly Yield BreakPoint", crop_name, paste0(field_name, "_", crop_name, "_Average_Yield_BreakPoint.pdf")), device = "pdf")
  #   # print(create_discrete_yield_scale())
  #   
  #   yearlyBreakPointPlots <- list()
  #   
  #   for (year in AllYears) {
  #     print(paste(field_name, crop_name, year))
  #     raster_path =  get_yield_and_shapefile_for_field_year_crop(base_dir = data_dir, field_name = field_name, crop_name = crop_name, year = year)
  #     raster_path = raster_path$yield_path
  #     YearlyFunction <- YearlyEdgeBreakPoint(raster_path = raster_path, year= year, shared_scale_components= shared_scale_components )
  #     yearPlot <- YearlyFunction$yearPlot
  #     # output_path <- file.path("D:/study/Yearly Yield BreakPoint", field_name, crop_name, paste0(year, "_", field_name, "_", crop_name, "_Average_Yield_BreakPoint.pdf"))
  #     # ggsave(yearPlot, filename = output_path, device = "pdf")
  #     
  #     # output_path <-file.path("D:/study/Field Wise",field_name,"Yearly Yield BreakPoint",  crop_name, paste0(year, "_", field_name, "_", crop_name, "_Average_Yield_BreakPoint.pdf"))
  #     # ggsave(yearPlot, filename = output_path , device = "pdf")
  #     
  #     
  #     yearlyBreakPointPlots[[length(yearlyBreakPointPlots)+1]] <- yearPlot
  #     
  #     print("ooooooooooooooooooooooooooooooooooooooooooooo")
  #     print(YearlyFunction$bp_results$breakpoint_val)
  #     print(YearlyFunction$bp_results$points_in_breakpoint_region)
  #     print("ooooooooooooooooooooooooooooooooooooooooooooo")
  #     
  #     if(YearlyFunction$bp_results$success){
  #       
  #       message("🟢🟢🟢 Writing BreakPoint to the file")
  #       # print(BreakPointResults)
  #       resultToSave=data.frame(
  #         FieldName = field_name,
  #         Crop = crop_name,
  #         BreakPoint = YearlyFunction$bp_results$breakpoint_val,
  #         PValue = YearlyFunction$bp_results$davies_pvalue,
  #         Year = year,
  #         AllYears = paste(AllYears, collapse = ","),
  #         Yield_Inside_BreakPoint = YearlyFunction$bp_results$inside_mean,
  #         Yield_Outside_BreakPoint = YearlyFunction$bp_results$outside_mean,
  #         Yield_In_BreakPoint_Region = YearlyFunction$bp_results$breakpoint_mean,
  #         Slope_before = YearlyFunction$bp_results$slope_before,
  #         Slope_after = YearlyFunction$bp_results$slope_after,
  #         Breakpoint_strength = YearlyFunction$bp_results$breakpoint_strength,
  #         Field_Average_Yield = YearlyFunction$bp_results$mean_yield
  #         
  #       )
  #       output_path <- file.path("Z:/temp/YearlyBreakpoints2/WithFieldAverage/BreakPoints2.xlsx")
  #       existingRows <- read.xlsx(output_path, colNames = TRUE)
  #       updatedData <- rbind(existingRows, resultToSave)
  #       print(resultToSave)
  #       write.xlsx(updatedData, file =output_path, overwrite = TRUE )
  #       
  #       
  #       
  #     }
  #     else{
  #       message("🟡🟡🟡 No BreakPoint found")
  #       noBP = data.frame(
  #         FieldName = field_name,
  #         Crop = crop_name,
  #         Year = year,
  #         AllYears = paste(AllYears, collapse = ","),
  #         PValue = YearlyFunction$bp_results$davies_pvalue
  #       )
  #       output_path <- file.path("Z:/temp", "YearlyBreakpoints2/WithFieldAverage/NoBreakPoints.xlsx")
  #       existingRows <- read.xlsx(output_path, colNames = TRUE)
  #       updatedData <- rbind(existingRows, noBP)
  #       write.xlsx(updatedData, file = output_path, overwrite = TRUE )
  #     }
  #   }
  #   
  #   
  #   Sys.sleep(0.5)
  #   # stop()
  #   
  # }
}

