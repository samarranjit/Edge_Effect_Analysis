#A script that reads the file with the details of the fields' name, year and crops, get the files and then gets the coordinates of the edges

library(openxlsx)
library(dplyr)

field_details <- read.xlsx("Results/Edge Specific Analysis/RegularEdgedFieldsForAnalysis.xlsx")
View(field_details)

field_details %>% distinct(Field_Name) %>% count()
data_dir <- file.path(getwd(), "Data_Raw/v1.1")

for(n in 1:nrow(field_details)) {
  years <- trimws(unlist(strsplit(as.character(field_details$Years[n]), ",")))
  
  
  for (year in years) {
    
    print("=========================================================================================================")
   print(paste("📌Field:", field_details$Field_Name[n], "Year:", year, "crop:", field_details$Crop[n],"." )) 
   print("______")
   
   get_files <- get_yield_and_shapefile_for_field_year_crop(field_name = field_details$Field_Name[n] , crop_name = field_details$Crop[n], year = year, base_dir = data_dir)
   p <- plot_field_with_buffer(shapefile_path = get_files$shapefile_path, buffer_distance = 50)
   plot(p)
   
   edges_info <- extract_field_edges(shapefile_path = get_files$shapefile_path)
   print(edges_info$change_points$change_x[edges_info$change_points$Direction == "South"])
   
   resultToSave <- data.frame(
     FieldName = field_details$Field_Name[n],
     Crop = field_details$Crop[n],
     Year = year,
     Point1_x = ifelse(any(edges_info$change_points$from_direction == "North"),
                       edges_info$change_points$change_x[edges_info$change_points$from_direction == "North"], NA),
     Point1_y = ifelse(any(edges_info$change_points$from_direction == "North"),
                       edges_info$change_points$change_y[edges_info$change_points$from_direction == "North"], NA),
     Point2_x = ifelse(any(edges_info$change_points$from_direction == "East"),
                       edges_info$change_points$change_x[edges_info$change_points$from_direction == "East"], NA),
     Point2_y = ifelse(any(edges_info$change_points$from_direction == "East"),
                       edges_info$change_points$change_y[edges_info$change_points$from_direction == "East"], NA),
     Point3_x = ifelse(any(edges_info$change_points$from_direction == "South"),
                       edges_info$change_points$change_x[edges_info$change_points$from_direction == "South"], NA),
     Point3_y = ifelse(any(edges_info$change_points$from_direction == "South"),
                       edges_info$change_points$change_y[edges_info$change_points$from_direction == "South"], NA),
     Point4_x = ifelse(any(edges_info$change_points$from_direction == "West"),
                       edges_info$change_points$change_x[edges_info$change_points$from_direction == "West"], NA),
     Point4_y = ifelse(any(edges_info$change_points$from_direction == "West"),
                       edges_info$change_points$change_y[edges_info$change_points$from_direction == "West"], NA)
   )
   print(resultToSave)
   
   output_excel_file <- file.path(getwd(), "Results/Edge Specific Analysis/Results/20m_Phase_2/EdgeSpecificAnalysisFieldDetails.xlsx")
   
    # stop()
  #  existingRows <- read.xlsx(output_excel_file, colNames = TRUE)
  #  updatedData <- rbind(existingRows, resultToSave)
  #  write.xlsx(updatedData,file = output_excel_file, overwrite = TRUE )
   
   
   
   print(paste("✅ Done" ,  field_details$Field_Name[n], "Year:", year, "crop:", field_details$Crop[n],"."))
    print("=========================================================================================================")
    print("=========================================================================================================")
    
  }
  
}








extract_field_edges <- function(shapefile_path, simplify_convex = TRUE) {
  library(sf)
  library(dplyr)
  library(geosphere)
  
  # Load and optionally simplify field polygon
  field_poly <- st_read(shapefile_path, quiet = TRUE)
  
  # Reproject to WGS84 for bearing calculations
  field_poly <- st_transform(field_poly, crs = 4326)
  
  # Optional convex hull simplification to remove holes or inner rings
  if (simplify_convex) {
    field_poly <- st_convex_hull(field_poly)
  }
  
  # Convert boundary to LINESTRING and extract coordinates
  field_lines <- st_cast(st_boundary(field_poly), "LINESTRING")
  coords <- st_coordinates(field_lines)
  
  # Helper to convert bearing to direction
  get_direction_label <- function(bearing) {
    if (bearing < 0) bearing <- bearing + 360
    case_when(
      bearing >= 45 & bearing < 135 ~ "South",
      bearing >= 135 & bearing < 225 ~ "West", 
      bearing >= 225 & bearing < 315 ~ "North",
      TRUE ~ "East"  # 315-360 and 0-45
    )
  }
  
  # Create edge data in proper sequential order
  edge_data <- tibble()
  
  for (i in 1:(nrow(coords) - 1)) {
    x1 <- coords[i, "X"]
    y1 <- coords[i, "Y"]
    x2 <- coords[i + 1, "X"]
    y2 <- coords[i + 1, "Y"]
    
    b <- bearing(c(x1, y1), c(x2, y2))
    b <- ifelse(b < 0, b + 360, b)
    dir <- get_direction_label(b)
    seg <- paste0("P", i, "P", i + 1)
    
    edge_data <- bind_rows(edge_data, tibble(
      Segment = seg,
      x1 = x1, y1 = y1, x2 = x2, y2 = y2,
      Bearing = b,
      Direction = dir,
      segment_order = i  # Add ordering column
    ))
  }
  
  # CRITICAL FIX: Ensure data is in correct boundary order
  edge_data <- edge_data %>%
    arrange(segment_order) %>%  # Sort by the actual boundary sequence
    mutate(
      prev_direction = lag(Direction),
      next_direction = lead(Direction),
      is_change_point = Direction != prev_direction & !is.na(prev_direction)
    )
  
  # Handle the circular nature of the polygon (last direction back to first)
  first_direction <- edge_data$Direction[1]
  last_direction <- edge_data$Direction[nrow(edge_data)]
  
  # Get the actual change points (start of new direction)
  change_points <- edge_data %>%
    filter(is_change_point) %>%
    select(
      Segment,
      change_x = x1, 
      change_y = y1, 
      from_direction = prev_direction,
      to_direction = Direction,
      Bearing
    )
  
  # Add the closing change point if the last direction differs from the first
  if (last_direction != first_direction) {
    # The closing change point is at the START of the polygon (where we return to first direction)
    closing_change <- tibble(
      Segment = paste0("P", nrow(edge_data), "P1"),  # Last segment to first point
      change_x = edge_data$x1[1],  # First point coordinates  
      change_y = edge_data$y1[1],
      from_direction = last_direction,
      to_direction = first_direction,
      Bearing = edge_data$Bearing[1]
    )
    
    change_points <- bind_rows(change_points, closing_change)
    
    cat("\nAdded closing change point:", last_direction, "→", first_direction, "\n")
  } else {
    cat("\nNo closing change point needed - last direction matches first\n")
  }
  
  # Get summary of direction segments
  direction_summary <- edge_data %>%
    group_by(Direction) %>%
    summarise(
      segment_count = n(),
      first_segment = first(Segment),
      last_segment = last(Segment),
      bearing_range = paste(round(min(Bearing), 1), "-", round(max(Bearing), 1)),
      .groups = 'drop'
    )
  
  # Print diagnostics
  cat("Direction Summary:\n")
  print(direction_summary)
  
  cat("\nChange Points Found:\n")
  print(change_points)
  
  # Verify we have all 4 cardinal directions
  expected_directions <- c("North", "South", "East", "West")
  missing_directions <- setdiff(expected_directions, unique(edge_data$Direction))
  
  if(length(missing_directions) > 0) {
    cat("\nWarning: Missing directions:", paste(missing_directions, collapse = ", "), "\n")
  }
  
  cat("\nFirst few segments in boundary order:\n")
  print(head(edge_data %>% select(Segment, Direction, Bearing), 10))
  
  cat("\nLast few segments in boundary order:\n")
  print(tail(edge_data %>% select(Segment, Direction, Bearing), 5))
  
  return(list(
    edges = edge_data,
    change_points = change_points,
    direction_summary = direction_summary
  ))
}
# Example usage:
# edge_info <- extract_field_edges("path/to/shapefile.shp")
# north_edges <- filter(edge_info$edges, Direction == "North")
# change_pts <- edge_info$change_points


plot_field_with_buffer <- function(shapefile_path, buffer_distance = 20) {
  library(sf)
  library(ggplot2)
  library(maptiles)
  library(terra)
  library(tidyterra)
  library(dplyr)
  
  field_poly <- st_read(shapefile_path, quiet = TRUE)
  field_poly <- st_transform(field_poly, crs = 4326)
  field_buffer <- st_buffer(field_poly, dist = buffer_distance)
  
  # Get satellite tile for the buffer area (don't crop to buffer boundary)
  tile <- get_tiles(field_buffer, provider = "Esri.WorldImagery", crop = TRUE)
  
  # Extract directional edges
  edge_info <- extract_field_edges(shapefile_path)
  edge_segments <- edge_info$edges
  
  print("Plotting Function :")
  edge_info$change_points %>% View()
  
  ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile) +
    geom_segment(data = edge_segments,
                 aes(x = x1, y = y1, xend = x2, yend = y2, color = Direction),
                 size = 1.2) +
    geom_sf(data = field_poly, fill = NA, color = "blue", size = 1) +
    geom_sf(data = field_buffer, fill = NA, color = "red", size = 1.2) +
    geom_point(data = edge_info$change_points,
               aes(x = change_x, y = change_y),
               size = 3, shape = 21, fill = "black", color = "white") +
    labs(title = "Satellite Image: Field Edges and 20m Buffer",
         x = "Longitude", y = "Latitude", color = "Edge Direction") +
    coord_sf() 
}


# Example usage:
# plot_field_with_buffer("path/to/shapefile.shp")
