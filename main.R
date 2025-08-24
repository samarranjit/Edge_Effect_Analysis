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

# Data Overview

#==================== Box Plot for all Fields Average ===========================
# source("Data Overview/ScriptToEvaluateMeanOfTop3CropsAndBoxPlot.R") # <--------------- Uncomment

#==================== Yearwise Favorability Analysis ===========================
# source("Data Overview/Year Favorability Study.R") #<---------------- Uncomment
