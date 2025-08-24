# -------------------------------------------------------------
# Script: extract_mean_yield.R
# Purpose: Extracts mean yield values from kriged raster files
#          across multiple years and locations for top 3 crops
#          (Corn, Soybeans, Wheat), and visualizes their yield
#          distribution as a boxplot.
#
# Functionality:
#   - Loops through a structured folder of raster yield data
#   - Extracts year, crop, and location info from folder names
#   - Computes mean yield from each raster
#   - Compiles data into a summary CSV
#   - Plots yield distribution by year and crop
#
# Inputs:
#   - TIFF files named '*kriged.tif' 
#
# Outputs:
#   - CSV file: Results/Top3_Crops_Yield_Analysis/Mean_Yield_by_Location_Year_Crop.csv
#   - Boxplot PDF: Results/Top3_Crops_Yield_Analysis/Top3_Crop_Yield_Boxplot.pdf
#
# Author: Samar Ranjit
# Last Updated: 2025-05-31
# -------------------------------------------------------------



# Load necessary libraries
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


# Define base directory relative to the project root
base_dir <- Sys.getenv("DATA_PATH")

# Define years and crops
years <- 2014:2024
crops <- toupper(c("SOYBEANS", "CORN", "WHEAT"))

# Create an empty list to store extracted data
data_list <- list()

# Loop through each year folder
for (year_folder in list.dirs(base_dir, recursive = FALSE)) {
  year <- as.numeric(str_extract(basename(year_folder), "^\\d{4}"))
  if (!is.na(year) && year %in% years) {
    location_crop_folders <- list.dirs(year_folder, recursive = FALSE)
    for (loc_crop_folder in location_crop_folders) {
      folder_name_parts <- unlist(strsplit(basename(loc_crop_folder), "_"))
      location <- paste(folder_name_parts[2:(length(folder_name_parts) - 3)], collapse = "_")
      crop_name <- toupper(folder_name_parts[length(folder_name_parts) - 1])
      if (crop_name %in% crops) {
        message(glue::glue("Processing: {location}, {year}, {crop_name}"))
        tif_file <- list.files(loc_crop_folder, pattern = "kriged\\.tif$", full.names = TRUE)
        if (length(tif_file) > 0) {
          raster_data <- raster(tif_file)
          mean_yield <- mean(values(raster_data), na.rm = TRUE)
          data_list[[length(data_list) + 1]] <- data.frame(
            Year = year, Crop = crop_name, Location = location, Mean_Yield = mean_yield
          )
        }
      }
    }
  }
}

# Combine all extracted data into one data frame
df <- bind_rows(data_list)
df$Year <- as.factor(df$Year)

# Save result
results_path <- Sys.getenv("RESULTS_PATH")

results_dir <- file.path(results_path,"Results", "Top3_Crops_Yield_Analysis")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

#================= Writing the Average Yield Data ( Required down the road) =================================
# write.csv(df, file.path(results_dir, "Mean_Yield_by_Location_Year_Crop.csv"), row.names = FALSE)

# View subsets
high_yield <- filter(df, Mean_Yield > 200)
crop_specific <- filter(df, Crop == "CORN")

# Plot
p <- ggplot(df, aes(x = Year, y = Mean_Yield, fill = Crop)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = "dodge") +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.5, size = 0.5
  ) +
  scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  scale_y_continuous(breaks = seq(0, max(df$Mean_Yield, na.rm = TRUE), by = 25)) +
  labs(
    title = "Year-wise Yield Distribution of Top 3 Crops (2014–2024)",
    x = "Year", y = "Mean Yield", fill = "Crop Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(p)

# ====================== Save plot ==================================
# ggsave(
#   filename = file.path(results_dir, "Top3_Crop_Yield_Boxplot.pdf"),
#   plot = p, width = 10, height = 6, dpi = 300
# )
