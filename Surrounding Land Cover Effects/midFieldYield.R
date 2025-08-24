#Calculating mid field yield, average yield of points outside the 20m buffer width

library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(openxlsx)
library(sf)        # For spatial operations and buffering
library(raster)     # For raster handling
library(terra)
library(dplyr)     # For data manipulation
library(ggplot2)   # For visualization
library(purrr)     # For functional programming
library(lme4)      # For mixed-effects modeling
library(patchwork) # For combining ggplots
library(gridExtra) # For displaying tables
library(grid)
library(performance)
library(multcompView)
library(segmented)
library(viridis)
library(leaflet)




midfield_yield_df <- data.frame(
  FieldName = character(),
  Crop = character(),
  Year = character(),
  MidFieldYield = numeric(),
  stringsAsFactors = FALSE
)
View(midfield_yield_df)

field_Data <- read.xlsx("../Surrounding Specifc Effect on Edge Yield/Results.xlsx")
View(field_Data)

for(n in 1:5){
  get_midField_yield(base_dir = data_dir, field_name = field_Data$FieldName[n], crop_name = field_Data$Crop[n], year = field_Data$Year[n])
}

get_midField_yield <- function(base_dir, field_name, crop_name, year) {
  # Function to find yield files
  
  # Get all yield files
  get_field_details <-  get_yield_and_shapefile_for_field_year_crop(base_dir = data_dir, year = year, field_name = field_name, crop_name = crop_name)
  print(get_field_details$yield_path)
  yield_files <- data.frame(
    year = year,
    file_path = get_field_details$yield_path,
    stringsAsFactors = FALSE
  )
  
  field_boundary <- get_field_details$shapefile_path
  
  # Process yield file function
  process_yield_file <- function(file_info) {
    year <- file_info$year
    file_path <- file_info$file_path
    print(file_path)
    
    # Read yield raster
    yield_raster <- raster(file_path)
    
    shp_file <- list.files(dirname(file_path), pattern = "_field\\.shp$", full.names = TRUE)
    
    if (length(shp_file) == 0) {
      return(NULL)
    }
    
    # Read the shapefile and transform to match raster CRS
    field_boundary <- st_read(field_boundary, quiet = TRUE)
    field_boundary <<- st_transform(field_boundary, crs(yield_raster))
    
    if (sf::st_is_longlat(field_boundary)) {
      return(NULL)
    }
    
    # Convert raster to data frame
    yield_points <- as.data.frame(yield_raster, xy = TRUE, na.rm = TRUE)
    if (nrow(yield_points) == 0) return(NULL)
    
    colnames(yield_points) <- c("x", "y", "yield")
    
    # Convert to sf and assign correct CRS
    yield_sf <- st_as_sf(yield_points, coords = c("x", "y"), crs = crs(yield_raster))
    yield_sf <- st_transform(yield_sf, st_crs(field_boundary))  # Ensure CRS match
    
    # Calculate distance from edge
    field_edges <- st_cast(st_union(field_boundary), "MULTILINESTRING")
    yield_sf$dist_to_edge <- as.numeric(st_distance(yield_sf, field_edges))
    
    # Add year info
    yield_sf$year <- as.factor(year)
    
    yield_sf$shapefile_used <- basename(shp_file[1])
    
    # Save field boundary for later use
    attr(yield_sf, "field_boundary") <- field_boundary
    
    return(yield_sf)
  }
  
  
  
  # Check if files were found
  print(class(yield_files))
  if (nrow(yield_files) == 0) {
    return(list(error = "No yield files found for the selected field and crop."))
  }
  
  # Make yield_files accessible in the parent environment for later use
  # assign("yield_files", yield_files, envir = parent.frame())
  
  
  # Process all yield files
  all_yield_data <- map(1:nrow(yield_files), function(i) {
    process_yield_file(yield_files[i, ])
  })
  
  all_yield_data <- compact(all_yield_data)  # Remove NULL values
  # View(all_yield_data)
  
  # Combine all processed data
  if (length(all_yield_data) > 0) {
    yield_data <<- do.call(rbind, all_yield_data)
    # View(yield_data)
    # Save field boundary from the first dataset
    field_boundary <- attr(all_yield_data[[1]], "field_boundary")
  } else {
    return(list(error = "No valid data extracted from yield files."))
  }
  
  #incProgress(0.4, detail = "Computing average yield")
  
  # Compute average yield per point
  yield_avg <- yield_data %>%
    group_by(geometry) %>%
    summarize(avg_yield = mean(yield, na.rm = TRUE), 
              dist_to_edge = mean(dist_to_edge), 
              .groups = "drop")
  overall_years_field_yield_average <- mean(yield_avg$avg_yield, na.rm = TRUE)
  overall_years_field_yield_variance <- sd(yield_avg$avg_yield, na.rm = TRUE)
  
  # Preparing for yield by aspect box plot
  
  yield_df_xy <- yield_avg %>%
    st_transform(4326) %>%
    mutate(x = st_coordinates(.)[, 1],
           y = st_coordinates(.)[, 2]) %>%
    as.data.frame()
  
  # plot(yield_df_xy)
  yield_far_points <- yield_data %>%
    filter(dist_to_edge > 20)
  library(ggplot2)
  library(sf)
  
  # Make sure both layers have the same CRS (4326 for mapping)
  field_boundary_plot <- st_transform(field_boundary, 4326)
  yield_far_points_plot <- st_transform(yield_far_points, 4326)
  
  # Plot
  p <- ggplot() +
    geom_sf(data = field_boundary_plot, fill = NA, color = "black", size = 1) +
    geom_sf(data = yield_far_points_plot, aes(color = yield), size = 1.5, alpha = 0.8) +
    scale_color_viridis_c(option = "plasma", name = "Yield") +
    theme_minimal() +
    labs(title = paste("Yield Points > 20m from Field Edge -", field_name, year),
         subtitle = crop_name)


  plot(p)
  
  mid_field_points <- yield_data %>% filter(dist_to_edge > 20)
  
  # Calculate the average yield
  mid_field_yield_value <- mean(mid_field_points$yield, na.rm = TRUE)
  
  print("Mid Field Yield :")
  print(mid_field_yield_value )
  
  # midfield_yield_df <<- rbind(midfield_yield_df, data.frame(
  #   FieldName = field_name,
  #   Crop = crop_name,
  #   Year = year,
  #   MidFieldYield = mean(yield_df_xy$avg_yield[yield_df_xy$dist_to_edge > 20], na.rm = TRUE),
  #   stringsAsFactors = FALSE
  # ))  
  
  }

# 
# field_Data_Combined <- cbind(field_Data, midfield_yield_df$MidFieldYield)
# write.xlsx(field_Data_Combined, file = "../Surrounding Specifc Effect on Edge Yield/ResultsWithMidFieldYield.xlsx",overwrite = TRUE)
