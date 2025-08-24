# -------------------------------------------------------------
# Script: edge_buffer_analysis.R
# Purpose: Identify edge-specific buffer zones (with rotated west edge),
#          extract yield zones, and compute mean yield for each side.
#
# Uses `here::here()` to ensure relative paths work across environments.
#
# Author: Samar Ranjit
# Last Updated: 2025-05-31
# -------------------------------------------------------------

library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(here)

# ---------- 1. Update function to use relative path ----------
get_yield_and_shapefile_for_field_year_crop <- function(base_dir = data_dir,
                                                        year, field_name, crop_name) {
  cat(
    "Searching for yield and shapefile for:\n",
    "Year:", year, "\n",
    "Field:", field_name, "\n",
    "Crop:", crop_name, "\n"
  )

  year_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  year_folder <- year_folders[grepl(paste0("^", year, "_"), basename(year_folders))]

  if (length(year_folder) == 0) {
    stop("❌ No folder found for year: ", year)
  }

  field_folders <- list.dirs(year_folder, full.names = TRUE, recursive = FALSE)
  crop_folder <- field_folders[
    grepl(field_name, field_folders, ignore.case = TRUE) &
      grepl(crop_name, field_folders, ignore.case = TRUE)
  ]

  if (length(crop_folder) == 0) {
    stop("❌ No folder found for field and crop combination: ", field_name, " & ", crop_name)
  }

  tif_file <- list.files(crop_folder, pattern = "kriged\\.tif$", full.names = TRUE)
  shp_file <- list.files(crop_folder, pattern = "_field\\.shp$", full.names = TRUE)

  if (length(tif_file) == 0) tif_file <- NA
  if (length(shp_file) == 0) shp_file <- NA

  result <- data.frame(
    year = as.numeric(year),
    field = field_name,
    crop = crop_name,
    yield_path = tif_file[1],
    shapefile_path = shp_file[1],
    stringsAsFactors = FALSE
  )

  return(result)
}
