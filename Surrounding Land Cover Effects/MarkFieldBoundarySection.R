
#Get Directional Edge Yield for a field specific to a particular side based on the long lat that you give
#=================logs=======================================
#7/1/2025: Making the changes suggested by Gebba: we need yield loss at field edges so
# I am looking to calculate the yield excluding the buffer zone too; right now we only have the yield inside the breakpoint 
# and average of the entire field.

library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(openxlsx)



#============================================================




allFieldsData <- read.xlsx(file.path(getwd(), "/Results/Edge Specific Analysis/Results/10m1.1/10mFieldsDetailsWithEdgesPoints.xlsx"))
View(allFieldsData)


runAnalysis <- function(){
  
  for(n in 1:nrow(allFieldsData)) {
  # for(n in 1:5) {
    # years <- trimws(unlist(strsplit(allFieldsData$Years[n], ",")))
    # for (year in years) {
    
    year <- allFieldsData$Year[n]
      print("---------------------------------------------------------------------------------------------------------------")
      print("---------------------------------------------------------------------------------------------------------------")
      print(paste("📌 We are working on", allFieldsData[n,"FieldName"],"for year", year))
      
      tryCatch({
        
        
      DirectionalYield_data <- getBoundarySpecificYield(field_name = allFieldsData[n,"FieldName"], crop_name = allFieldsData[n,"Crop"], year= year, field_data =allFieldsData[n,] )
      print("---------------------------------------------------------------------------------------------------------------")
      # print(Yield_data)
      
      resultToSave <- data.frame(
        FieldName=allFieldsData$FieldName[n],
        Crop=allFieldsData$Crop[n] ,
        Year= year,
        # North=allFieldsData$North[n],
        # East=allFieldsData$East[n],
        # South=allFieldsData$South[n],
        # West=allFieldsData$West[n],
        # Confidence=allFieldsData$Confidence[n],
        Point1_x= allFieldsData$Point1_x[n],
        Point1_y= allFieldsData$Point1_y[n],
        Point2_x= allFieldsData$Point2_x[n],
        Point2_y= allFieldsData$Point2_y[n] ,
        Point3_x= allFieldsData$Point3_x[n],
        Point3_y= allFieldsData$Point3_y[n],
        Point4_x= allFieldsData$Point4_x[n],
        Point4_y= allFieldsData$Point4_y[n],
        North_Yield= DirectionalYield_data$North_Yield,
        East_Yield= DirectionalYield_data$East_Yield,
        South_Yield= DirectionalYield_data$South_Yield,
        West_Yield= DirectionalYield_data$West_Yield ,
        Original_Yield= DirectionalYield_data$Original_Yield 
        
        
      )
      
      message(paste("Done for ", allFieldsData$FieldName[n], "\n Here are the Results:- \n"))
      print(resultToSave)
      existingRows <- read.xlsx(file.path(getwd(), "Results/Edge Specific Analysis/Results/10m1.1/10mFieldsDetailsWithEdgesYield.xlsx"), colNames = TRUE)
      updatedData <- rbind(existingRows, resultToSave)
      write.xlsx(updatedData, file =file.path(getwd(), "Results/Edge Specific Analysis/Results/10m1.1/10mFieldsDetailsWithEdgesYield.xlsx"), overwrite = TRUE )
      print(paste("✅ We have done working on: ", allFieldsData[n,"FieldName"],"for year", year))
      print("---------------------------------------------------------------------------------------------------------------")
      print("---------------------------------------------------------------------------------------------------------------")
      },
      error = function(err){
        
        errortoSave = data.frame(
          Field_Name=allFieldsData$FieldName[n],
          Crop=allFieldsData$Crop[n] ,
          Year= year,
          error = err$message
        )
        print("🔴🔴🔴 We detected an error")
        existingRows <- read.xlsx(file.path(getwd(), "Results/Edge Specific Analysis/Results/10m1.1/error.xlsx"), colNames = TRUE)
        updatedData <- rbind(existingRows, errortoSave)
        write.xlsx(updatedData, file =file.path(getwd(), "Results/Edge Specific Analysis/Results/10m1.1/error.xlsx"), overwrite = TRUE )
        
      }
      )
  
      
      # stop()
      
    # }
    
  }
  
}

get_yield_and_shapefile_for_field_year_crop <- function(base_dir="C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Data_Raw/v1.1",
                                                        year, field_name, crop_name) {
  cat("Searching for yield and shapefile for:\n",
      "Year:", year, "\n",
      "Field:", field_name, "\n",
      "Crop:", crop_name, "\n")
  
  # Find folders like "2014_farmcrew_n27_v1.1"
  year_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  year_folder <- year_folders[grepl(paste0("^", year, "_"), basename(year_folders))]
  
  if (length(year_folder) == 0) {
    stop("❌ No folder found for year: ", year)
  }
  
  # Look for matching crop folder inside year folder
  field_folders <- list.dirs(year_folder, full.names = TRUE, recursive = FALSE)
  
  crop_folder <- field_folders[
    grepl(field_name, field_folders, ignore.case = TRUE) &
      grepl(crop_name, field_folders, ignore.case = TRUE)
  ]
  
  if (length(crop_folder) == 0) {
    stop("❌ No folder found for field and crop combination: ", field_name, " & ", crop_name)
  }
  
  # Get kriged .tif file and _field.shp
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



getBoundarySpecificYield <- function(field_name, year, crop_name, field_data ){
  buffer_distance = 10
  
  getfiles <- get_yield_and_shapefile_for_field_year_crop(year = year, field_name= field_name , crop_name = crop_name, base_dir = base_dir)
  
  
  if(field_name %in% c("East Farm_5-21")){
    
    north_boundary <- mark_field_boundary_section2(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                                buffer_distance =buffer_distance,
                                                point1_x= field_data$Point1_x,
                                                point1_y= field_data$Point1_y,
                                                point2_x= field_data$Point2_x, 
                                                point2_y= field_data$Point2_y,
                                                field_name = field_name,
                                                crop_name = crop_name,
                                                year= year
                                                
  )
  }
  else{
    north_boundary <- mark_field_boundary_section(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                                  buffer_distance =buffer_distance,
                                                  point1_x= field_data$Point1_x,
                                                  point1_y= field_data$Point1_y,
                                                  point2_x= field_data$Point2_x, 
                                                  point2_y= field_data$Point2_y,
                                                  field_name = field_name,
                                                  crop_name = crop_name,
                                                  year= year
                                                  
    )
  }
  east_boundary <- mark_field_boundary_section(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                               buffer_distance= buffer_distance,
                                               point1_x= field_data$Point2_x,
                                               point1_y= field_data$Point2_y,
                                               point2_x= field_data$Point3_x, 
                                               point2_y= field_data$Point3_y,
                                               field_name = field_name,
                                               crop_name = crop_name,
                                               year= year
  )
  if(field_name %in% c("Linkage Farm_EB-3")){
    
  
  south_boundary <- mark_field_boundary_section2(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                                buffer_distance = buffer_distance,
                                                point1_x= field_data$Point3_x,
                                                point1_y= field_data$Point3_y,
                                                point2_x= field_data$Point4_x, 
                                                point2_y= field_data$Point4_y,
                                                field_name = field_name,
                                                crop_name = crop_name,
                                                year= year
  )
  }
  else{
    if(field_name %in% c("Central Farm_2-32A", "Central Farm_2-32C", "North Farm_NG-1-6")){
      
      
    south_boundary <- mark_field_boundary_section2(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                                   buffer_distance = buffer_distance,
                                                   point1_x= field_data$Point3_x,
                                                   point1_y= field_data$Point3_y,
                                                   point2_x= field_data$Point4_x, 
                                                   point2_y= field_data$Point4_y,
                                                   field_name = field_name,
                                                   crop_name = crop_name,
                                                   year= year
    )
    }
    else{
      south_boundary <- mark_field_boundary_section(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                                     buffer_distance = buffer_distance,
                                                     point1_x= field_data$Point3_x,
                                                     point1_y= field_data$Point3_y,
                                                     point2_x= field_data$Point4_x, 
                                                     point2_y= field_data$Point4_y,
                                                     field_name = field_name,
                                                     crop_name = crop_name,
                                                     year= year
      )
    }
  }
  
  if(field_name %in% c("Central Farm_1-33", "Central Farm_1-20C", "East Farm_6-64")){
    west_boundary <- mark_field_boundary_section2(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                                 buffer_distance =buffer_distance,
                                                 point1_x= field_data$Point4_x,
                                                 point1_y= field_data$Point4_y,
                                                 point2_x= field_data$Point1_x, 
                                                 point2_y= field_data$Point1_y,
                                                 field_name = field_name,
                                                 crop_name = crop_name,
                                                 year= year
    )
  }
  else{
    
  west_boundary <- mark_field_boundary_section(raster_file = getfiles$yield_path, shapefile_path = getfiles$shapefile_path, 
                                               buffer_distance =buffer_distance,
                                               point1_x= field_data$Point4_x,
                                               point1_y= field_data$Point4_y,
                                               point2_x= field_data$Point1_x, 
                                               point2_y= field_data$Point1_y,
                                               field_name = field_name,
                                               crop_name = crop_name,
                                               year= year
  )
  }
  
  North_Yield <- mean(values(north_boundary$raster_zones$buffer_zone), na.rm = TRUE)
  East_Yield <- mean(values(east_boundary$raster_zones$buffer_zone), na.rm = TRUE)
  South_Yield <-mean(values(south_boundary$raster_zones$buffer_zone), na.rm = TRUE)
  West_Yield <- mean(values(west_boundary$raster_zones$buffer_zone), na.rm = TRUE)
  Original_Yield <- mean(values(north_boundary$raster_zones$original), na.rm = TRUE)
  
  plot_result <- ggplot() +
    # Background raster (areas not in buffer)
    geom_spatraster(data = north_boundary$raster_zones$background, alpha = 0.6) +
    scale_fill_viridis_c(name = "Yield", na.value = "transparent") +
    
    # Add buffer zone as colored overlay
    geom_sf(data = north_boundary$section_buffer, fill = "red", alpha = 0.4, color = "darkred", size = 0.5) +
    geom_sf(data = east_boundary$section_buffer, fill = "pink", alpha = 0.4, color = "hotpink", size = 0.5) +
    geom_sf(data = south_boundary$section_buffer, fill = "red", alpha = 0.4, color = "darkred", size = 0.5) +
    geom_sf(data = west_boundary$section_buffer, fill = "pink", alpha = 0.4, color = "hotpink", size = 0.5) +
    
    # Add field boundary
    geom_sf(data = north_boundary$field_boundary, fill = NA, color = "black", size = 1) +
    
    # Add the selected boundary section (highlighted)
    geom_sf(data = north_boundary$boundary_section, color = "blue", size = 3) +
    geom_sf(data = east_boundary$boundary_section, color = "purple", size = 3) +
    geom_sf(data = south_boundary$boundary_section, color = "violet", size = 3) +
    geom_sf(data = west_boundary$boundary_section, color = "skyblue", size = 3) +
    
    
    # Add closest points on boundary
    geom_point(aes(x = north_boundary$input_points$closest_x[1], y = north_boundary$input_points$closest_y[1]), color = "green", size = 4, shape = 19) +
    geom_point(aes(x = north_boundary$input_points$closest_x[2], y = north_boundary$input_points$closest_y[2]), color = "green", size = 4, shape = 19) +

    geom_point(aes(x = south_boundary$input_points$closest_x[1], y = south_boundary$input_points$closest_y[1]), color = "green", size = 4, shape = 19) +
    geom_point(aes(x = south_boundary$input_points$closest_x[2], y = south_boundary$input_points$closest_y[2]), color = "green", size = 4, shape = 19) +

    geom_point(aes(x = east_boundary$input_points$closest_x[1], y = east_boundary$input_points$closest_y[1]), color = "yellow", size = 2, shape = 19) +
    geom_point(aes(x = east_boundary$input_points$closest_x[2], y = east_boundary$input_points$closest_y[2]), color = "yellow", size = 2, shape = 19) +

    geom_point(aes(x = west_boundary$input_points$closest_x[1], y = west_boundary$input_points$closest_y[1]), color = "yellow", size = 2, shape = 19) +
    geom_point(aes(x = west_boundary$input_points$closest_x[2], y = west_boundary$input_points$closest_y[2]), color = "yellow", size = 2, shape = 19) +

    
    labs(title = paste0("Field Boundary Section with ", buffer_distance, "m Buffer"), x = NULL, y = NULL,
    subtitle = paste0("Field Name: ", field_name, " Crop:  ", crop_name))+
    # caption = "Blue line = selected boundary section, Yellow = input points, Green = closest boundary points, Red = buffer zone") +
    theme_minimal() +
    theme(axis.text = element_text(size = 8),
          legend.position = "bottom")
  output_dir <- file.path(getwd(), "Results/Edge Specific Analysis/Results/10m1.1/BoundaryIllustrations")
  file_name <- paste0(field_name,"_", crop_name,"_" ,year,"boundary.pdf")
  this_file <- file.path(output_dir, file_name)
  plot(plot_result)
  ggsave(
    filename = this_file,
    plot = plot_result,
    width = 10, height = 8, units = "in"
  )
  print("✅Saved  the illustration of the border")
  
  
  return(
    data.frame(
      North_Yield = North_Yield,
      East_Yield = East_Yield,
      South_Yield = South_Yield,
      West_Yield = West_Yield,
      Original_Yield = Original_Yield
    )
  )
  
  
  
}

mark_field_boundary_section <- function(raster_file, shapefile_path, point1_x, point1_y, point2_x, point2_y, 
                                        buffer_distance, input_crs = NULL, output_plot = TRUE, field_name, crop_name, year) {
  # Load the raster and shapefile
  raster_data <- rast(raster_file)
  field_boundary <- st_read(shapefile_path)
  
  # Ensure both datasets have the same CRS
  field_boundary <- st_transform(field_boundary, crs(raster_data))
  
  # Determine input CRS for the points
  if(is.null(input_crs)) {
    # Auto-detect: if coordinates are between -180/180 and -90/90, assume WGS84
    if(abs(point1_x) <= 180 && abs(point1_y) <= 90 && abs(point2_x) <= 180 && abs(point2_y) <= 90) {
      input_crs <- 4326  # WGS84
      cat("Auto-detected input coordinates as WGS84 (longitude/latitude)\n")
    } else {
      input_crs <- st_crs(field_boundary)  # Use same as shapefile
      cat("Using shapefile CRS for input coordinates\n")
    }
  }
  
  # Create points from the coordinates with appropriate CRS
  point1 <- st_point(c(point1_x, point1_y))
  point2 <- st_point(c(point2_x, point2_y))
  target_points <- st_sf(geometry = st_sfc(point1, point2, crs = input_crs))
  
  # Transform points to match field boundary CRS if necessary
  if(st_crs(target_points) != st_crs(field_boundary)) {
    target_points <- st_transform(target_points, st_crs(field_boundary))
    cat("Transformed input coordinates from CRS", input_crs, "to", st_crs(field_boundary)$input, "\n")
  }
  
  # Extract the boundary as linestring
  if(st_geometry_type(field_boundary)[1] == "POLYGON") {
    boundary_line <- st_cast(st_boundary(field_boundary), "LINESTRING")
  } else if(st_geometry_type(field_boundary)[1] == "MULTIPOLYGON") {
    boundary_line <- st_cast(st_boundary(field_boundary), "LINESTRING")
  } else {
    boundary_line <- field_boundary
  }
  
  # Find closest points on boundary to the specified coordinates
  closest_point1 <- st_nearest_points(target_points[1,], boundary_line)
  closest_point2 <- st_nearest_points(target_points[2,], boundary_line)
  
  # Extract the actual coordinates of closest points
  coords1 <- st_coordinates(st_cast(closest_point1, "POINT")[1])
  coords2 <- st_coordinates(st_cast(closest_point2, "POINT")[2])
  
  # Function to extract section of boundary between two points
  extract_boundary_section <- function(boundary, coord1, coord2) {
    # Get all coordinates of the boundary
    boundary_coords <- st_coordinates(boundary)[,1:2]
    
    # Check if we have valid boundary coordinates
    if(nrow(boundary_coords) == 0) {
      stop("No boundary coordinates found")
    }
    
    # Find indices of points closest to our target coordinates
    dist1 <- sqrt((boundary_coords[,1] - coord1[1])^2 + (boundary_coords[,2] - coord1[2])^2)
    dist2 <- sqrt((boundary_coords[,1] - coord2[1])^2 + (boundary_coords[,2] - coord2[2])^2)
    
    idx1 <- which.min(dist1)
    idx2 <- which.min(dist2)
    
    # Print distances to help debug
    cat("Closest distance to point 1:", min(dist1, na.rm = TRUE), "meters\n")
    cat("Closest distance to point 2:", min(dist2, na.rm= TRUE), "meters\n")
    
    n_coords <- nrow(boundary_coords)
    
    # Path from idx1 to idx2 (forward)
    if (idx1 <= idx2) {
      section1 <- boundary_coords[idx1:idx2, , drop = FALSE]
    } else {
      section1 <- rbind(boundary_coords[idx1:n_coords, , drop = FALSE],
                        boundary_coords[1:idx2, , drop = FALSE])
    }
    
    # Path from idx2 to idx1 (reverse)
    if (idx2 <= idx1) {
      section2 <- boundary_coords[idx2:idx1, , drop = FALSE]
    } else {
      section2 <- rbind(boundary_coords[idx2:n_coords, , drop = FALSE],
                        boundary_coords[1:idx1, , drop = FALSE])
    }
    
    # # Compare which path is shorter
    # length1 <- sum(sqrt(rowSums((section1[-1, ] - section1[-nrow(section1), ])^2)))
    # length2 <- sum(sqrt(rowSums((section2[-1, ] - section2[-nrow(section2), ])^2)))
    # print(paste("Length1:" ,length1, "Length2: ", length2))
    # 
    # section_coords <- if (length1 <= length2) section1 else section2
    
    
    
    # Compare which path is shorter - CORRECTED VERSION
    length1 <- if (nrow(section1) >= 2) {
      diffs <- section1[-1, , drop = FALSE] - section1[-nrow(section1), , drop = FALSE]
      if(nrow(diffs) > 0 && ncol(diffs) >= 2) {
        sum(sqrt(rowSums(diffs^2, na.rm = TRUE)), na.rm = TRUE)
      } else {
        0  # If no differences to calculate, distance is 0
      }
    } else {
      0  # Single point, no distance
    }
    
    length2 <- if (nrow(section2) >= 2) {
      diffs <- section2[-1, , drop = FALSE] - section2[-nrow(section2), , drop = FALSE]
      if(nrow(diffs) > 0 && ncol(diffs) >= 2) {
        sum(sqrt(rowSums(diffs^2, na.rm = TRUE)), na.rm = TRUE)
      } else {
        0  # If no differences to calculate, distance is 0
      }
    } else {
      0  # Single point, no distance
    }
    
    print(paste("Length1:", length1, "Length2:", length2))
    
    section_coords <- if (length1 <= length2) section1 else section2
    
    
    print(section_coords)
    
    # Extract the section between these points
    # section_coords <- boundary_coords[idx1:idx2, , drop = FALSE]
    
    cat("Section coordinates extracted:", nrow(section_coords), "points\n")
    
    # Create linestring from section
    if(nrow(section_coords) >= 2) {
      section_line <- st_linestring(section_coords)
      return(st_sf(geometry = st_sfc(section_line, crs = st_crs(boundary))))
    } else if(nrow(section_coords) == 1) {
      # If only one point, create a small line segment using the closest boundary points
      cat("Only one point found, creating line between closest boundary points\n")
      section_line <- st_linestring(rbind(boundary_coords[idx1,], boundary_coords[idx1,]))
      return(st_sf(geometry = st_sfc(section_line, crs = st_crs(boundary))))
    } else {
      # If no points, create a direct line between the target points
      cat("No boundary points found between targets, creating direct line\n")
      section_line <- st_linestring(rbind(coord1, coord2))
      return(st_sf(geometry = st_sfc(section_line, crs = st_crs(boundary))))
    }
  }
  
  # Extract the boundary section
  boundary_section <- extract_boundary_section(boundary_line, coords1, coords2)
  
  # Create buffer around the boundary section
  section_buffer <- st_buffer(boundary_section, dist = buffer_distance)
  
  # Crop and mask raster to field boundary
  raster_cropped <- crop(raster_data, field_boundary)
  raster_masked <- mask(raster_cropped, field_boundary)
  
  # Create raster for the buffer zone
  buffer_raster <- rasterize(section_buffer, raster_masked, field = 1)
  
  # Create raster zones
  buffer_zone_raster <- raster_masked
  buffer_zone_raster[is.na(buffer_raster)] <- NA
  
  no_buffer_raster <- raster_masked
  no_buffer_raster[!is.na(buffer_raster)] <- NA
  
  # Calculate distance and angle between the two points for information
  distance_between_points <- sqrt((coords2[1] - coords1[1])^2 + (coords2[2] - coords1[2])^2)
  angle_of_section <- atan2(coords2[2] - coords1[2], coords2[1] - coords1[1]) * 180 / pi
  
  # Create the plot if requested
  if(output_plot) {
    plot_result <- ggplot() +
      # Background raster (areas not in buffer)
      geom_spatraster(data = no_buffer_raster, alpha = 0.6) +
      scale_fill_viridis_c(name = "Yield", na.value = "transparent") +
      
      # Add buffer zone as colored overlay
      geom_sf(data = section_buffer, fill = "red", alpha = 0.4, color = "darkred", size = 0.5) +
      
      # Add field boundary
      geom_sf(data = field_boundary, fill = NA, color = "black", size = 1) +
      
      # Add the selected boundary section (highlighted)
      geom_sf(data = boundary_section, color = "blue", size = 3) +
      
      # Add the target points
      geom_sf(data = target_points, color = "yellow", size = 3, shape = 21, fill = "yellow") +
      
      # Add closest points on boundary
      geom_point(aes(x = coords1[1], y = coords1[2]), color = "green", size = 4, shape = 19) +
      geom_point(aes(x = coords2[1], y = coords2[2]), color = "green", size = 4, shape = 19) +
      
      labs(title = paste0("Field Boundary Section with ", buffer_distance, "m Buffer", "; Field= " , field_name, " Crop = ", crop_name, " Year= ", year))+
      # subtitle = paste0("Section length: ", round(distance_between_points, 1), "m | Angle: ", round(angle_of_section, 1), "°"),
      # caption = "Blue line = selected boundary section, Yellow = input points, Green = closest boundary points, Red = buffer zone") +
      theme_minimal() +
      theme(axis.text = element_text(size = 8),
            legend.position = "bottom")
    
    print(plot_result)
  }
  
  # Return results
  result <- list(
    field_boundary = field_boundary,
    boundary_section = boundary_section,
    section_buffer = section_buffer,
    input_points = data.frame(
      point = c("Point 1", "Point 2"),
      input_x = c(point1_x, point2_x),
      input_y = c(point1_y, point2_y),
      closest_x = c(coords1[1], coords2[1]),
      closest_y = c(coords1[2], coords2[2])
    ),
    section_stats = data.frame(
      length_meters = distance_between_points,
      angle_degrees = angle_of_section,
      buffer_distance = buffer_distance
    ),
    raster_zones = list(
      original = raster_masked,
      background = no_buffer_raster,
      buffer_zone = buffer_zone_raster
    ),
    plot = if(output_plot) plot_result else NULL
  )
  
  return(result)
}



mark_field_boundary_section2 <- function(raster_file, shapefile_path, point1_x, point1_y, point2_x, point2_y, 
                                        buffer_distance, input_crs = NULL, output_plot = TRUE, field_name, crop_name, year) {
  # Load the raster and shapefile
  raster_data <- rast(raster_file)
  field_boundary <- st_read(shapefile_path)
  
  # Ensure both datasets have the same CRS
  field_boundary <- st_transform(field_boundary, crs(raster_data))
  
  # Determine input CRS for the points
  if(is.null(input_crs)) {
    # Auto-detect: if coordinates are between -180/180 and -90/90, assume WGS84
    if(abs(point1_x) <= 180 && abs(point1_y) <= 90 && abs(point2_x) <= 180 && abs(point2_y) <= 90) {
      input_crs <- 4326  # WGS84
      cat("Auto-detected input coordinates as WGS84 (longitude/latitude)\n")
    } else {
      input_crs <- st_crs(field_boundary)  # Use same as shapefile
      cat("Using shapefile CRS for input coordinates\n")
    }
  }
  
  # Create points from the coordinates with appropriate CRS
  point1 <- st_point(c(point1_x, point1_y))
  point2 <- st_point(c(point2_x, point2_y))
  target_points <- st_sf(geometry = st_sfc(point1, point2, crs = input_crs))
  
  # Transform points to match field boundary CRS if necessary
  if(st_crs(target_points) != st_crs(field_boundary)) {
    target_points <- st_transform(target_points, st_crs(field_boundary))
    cat("Transformed input coordinates from CRS", input_crs, "to", st_crs(field_boundary)$input, "\n")
  }
  
  # Extract the boundary as linestring
  if(st_geometry_type(field_boundary)[1] %in% c("POLYGON", "MULTIPOLYGON")) {
    # Create a convex hull or simplified version without holes
    field_boundary_simple <- st_convex_hull(field_boundary)
    cat("Original field had", length(st_geometry(field_boundary)[[1]]), "rings\n")
    cat("Simplified field has", length(st_geometry(field_boundary_simple)[[1]]), "rings\n")
    
    # Use the simplified version for boundary extraction
    boundary_line <- field_boundary_simple
  }
  
  cat("Extracted outer boundary with", nrow(st_coordinates(boundary_line)), "points\n")
  # plot(boundary_line)
  # stop("boundary line met")
  
  # Find closest points on boundary to the specified coordinates
  closest_point1 <- st_nearest_points(target_points[1,], boundary_line)
  closest_point2 <- st_nearest_points(target_points[2,], boundary_line)
  
  # Extract the actual coordinates of closest points
  coords1 <- st_coordinates(st_cast(closest_point1, "POINT")[1])
  coords2 <- st_coordinates(st_cast(closest_point2, "POINT")[2])
  
  # Function to extract section of boundary between two points
  extract_boundary_section <- function(boundary, coord1, coord2) {
    # Get all coordinates of the boundary
    boundary_coords <- st_coordinates(boundary)[,1:2]
    
    # Check if we have valid boundary coordinates
    if(nrow(boundary_coords) == 0) {
      stop("No boundary coordinates found")
    }
    
    # Find indices of points closest to our target coordinates
    dist1 <- sqrt((boundary_coords[,1] - coord1[1])^2 + (boundary_coords[,2] - coord1[2])^2)
    dist2 <- sqrt((boundary_coords[,1] - coord2[1])^2 + (boundary_coords[,2] - coord2[2])^2)
    
    idx1 <- which.min(dist1)
    idx2 <- which.min(dist2)
    
    # Print distances to help debug
    cat("Closest distance to point 1:", min(dist1, na.rm = TRUE), "meters\n")
    cat("Closest distance to point 2:", min(dist2, na.rm= TRUE), "meters\n")
    
    n_coords <- nrow(boundary_coords)
    
    # Path from idx1 to idx2 (forward)
    if (idx1 <= idx2) {
      section1 <- boundary_coords[idx1:idx2, , drop = FALSE]
    } else {
      section1 <- rbind(boundary_coords[idx1:n_coords, , drop = FALSE],
                        boundary_coords[1:idx2, , drop = FALSE])
    }
    
    # Path from idx2 to idx1 (reverse)
    if (idx2 <= idx1) {
      section2 <- boundary_coords[idx2:idx1, , drop = FALSE]
    } else {
      section2 <- rbind(boundary_coords[idx2:n_coords, , drop = FALSE],
                        boundary_coords[1:idx1, , drop = FALSE])
    }
    
    # Compare which path is shorter
    length1 <- sum(sqrt(rowSums((section1[-1, ] - section1[-nrow(section1), ])^2)))
    length2 <- sum(sqrt(rowSums((section2[-1, ] - section2[-nrow(section2), ])^2)))
    print(paste("Length1:" ,length1, "Length2: ", length2))
    
    section_coords <- if (length1 <= length2) section1 else section2
    
    # Extract the section between these points
    # section_coords <- boundary_coords[idx1:idx2, , drop = FALSE]
    
    cat("Section coordinates extracted:", nrow(section_coords), "points\n")
    
    # Create linestring from section
    if(nrow(section_coords) >= 2) {
      section_line <- st_linestring(section_coords)
      return(st_sf(geometry = st_sfc(section_line, crs = st_crs(boundary))))
    } else if(nrow(section_coords) == 1) {
      # If only one point, create a small line segment using the closest boundary points
      cat("Only one point found, creating line between closest boundary points\n")
      section_line <- st_linestring(rbind(boundary_coords[idx1,], boundary_coords[idx1,]))
      return(st_sf(geometry = st_sfc(section_line, crs = st_crs(boundary))))
    } else {
      # If no points, create a direct line between the target points
      cat("No boundary points found between targets, creating direct line\n")
      section_line <- st_linestring(rbind(coord1, coord2))
      return(st_sf(geometry = st_sfc(section_line, crs = st_crs(boundary))))
    }
  }
  
  # Extract the boundary section
  boundary_section <- extract_boundary_section(boundary_line, coords1, coords2)
  
  # Create buffer around the boundary section
  section_buffer <- st_buffer(boundary_section, dist = buffer_distance)
  
  # Crop and mask raster to field boundary
  raster_cropped <- crop(raster_data, field_boundary)
  raster_masked <- mask(raster_cropped, field_boundary)
  
  # Create raster for the buffer zone
  buffer_raster <- rasterize(section_buffer, raster_masked, field = 1)
  
  # Create raster zones
  buffer_zone_raster <- raster_masked
  buffer_zone_raster[is.na(buffer_raster)] <- NA
  
  no_buffer_raster <- raster_masked
  no_buffer_raster[!is.na(buffer_raster)] <- NA
  
  # Calculate distance and angle between the two points for information
  distance_between_points <- sqrt((coords2[1] - coords1[1])^2 + (coords2[2] - coords1[2])^2)
  angle_of_section <- atan2(coords2[2] - coords1[2], coords2[1] - coords1[1]) * 180 / pi
  
  # Create the plot if requested
  if(output_plot) {
    plot_result <- ggplot() +
      # Background raster (areas not in buffer)
      geom_spatraster(data = no_buffer_raster, alpha = 0.6) +
      scale_fill_viridis_c(name = "Yield", na.value = "transparent") +
      
      # Add buffer zone as colored overlay
      geom_sf(data = section_buffer, fill = "red", alpha = 0.4, color = "darkred", size = 0.5) +
      
      # Add field boundary
      geom_sf(data = field_boundary, fill = NA, color = "black", size = 1) +
      
      # Add the selected boundary section (highlighted)
      geom_sf(data = boundary_section, color = "blue", size = 3) +
      
      # Add the target points
      geom_sf(data = target_points, color = "yellow", size = 3, shape = 21, fill = "yellow") +
      
      # Add closest points on boundary
      geom_point(aes(x = coords1[1], y = coords1[2]), color = "green", size = 4, shape = 19) +
      geom_point(aes(x = coords2[1], y = coords2[2]), color = "green", size = 4, shape = 19) +
      
      labs(title = paste0("Field Boundary Section with ", buffer_distance, "m Buffer", "; Field= " , field_name, " Crop = ", crop_name, " Year= ", year))+
      # subtitle = paste0("Section length: ", round(distance_between_points, 1), "m | Angle: ", round(angle_of_section, 1), "°"),
      # caption = "Blue line = selected boundary section, Yellow = input points, Green = closest boundary points, Red = buffer zone") +
      theme_minimal() +
      theme(axis.text = element_text(size = 8),
            legend.position = "bottom")
    
    print(plot_result)
  }
  
  # Return results
  result <- list(
    field_boundary = field_boundary,
    boundary_section = boundary_section,
    section_buffer = section_buffer,
    input_points = data.frame(
      point = c("Point 1", "Point 2"),
      input_x = c(point1_x, point2_x),
      input_y = c(point1_y, point2_y),
      closest_x = c(coords1[1], coords2[1]),
      closest_y = c(coords1[2], coords2[2])
    ),
    section_stats = data.frame(
      length_meters = distance_between_points,
      angle_degrees = angle_of_section,
      buffer_distance = buffer_distance
    ),
    raster_zones = list(
      original = raster_masked,
      background = no_buffer_raster,
      buffer_zone = buffer_zone_raster
    ),
    plot = if(output_plot) plot_result else NULL
  )
  
  return(result)
}



