#=================================================================================================================================
# Breakpoint Calculation and Visualization Script

#Samar Ranjit
#4/22/20025

#=================================================================================================================================


# Load necessary libraries
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

base_dir <- Sys.getenv("DATA_PATH")
field_name <- "Central Farm_2-71"
crop_name <- "SOYBEANS"


run_average_breakpoint_analysis <- function(base_dir, field_name, crop_name) {
  # Function to find yield files
  find_yield_files <- function(base_dir , field_name, crop_name) {
    year_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
    year_folders <- year_folders[grep("^\\d{4}_", basename(year_folders))]
    
    results <- list()
    
    for (year_folder in year_folders) {
      year <- substr(basename(year_folder), 1, 4)
      
      field_folders <- list.dirs(year_folder, full.names = TRUE, recursive = FALSE)
      
      crop_folders <- field_folders[grepl(field_name, field_folders, ignore.case = TRUE) &
                                      grepl(crop_name, field_folders, ignore.case = TRUE)]

      
      
      
      for (folder in crop_folders) {
        tif_files <- list.files(folder, pattern = "kriged\\.tif$", full.names = TRUE)
        
        for (tif_file in tif_files) {
          results[[length(results) + 1]] <- list(
            year = as.numeric(year),
            field = field_name,
            crop = crop_name,
            file_path = tif_file
          )
        }
      }
    }
    
    results_df <- do.call(rbind, lapply(results, as.data.frame))
    return(results_df)
  }
  
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
    field_boundary <- st_read(shp_file[1], quiet = TRUE)
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
  
  # Get all yield files
  yield_files <- find_yield_files(base_dir, field_name, crop_name)
  
  # Check if files were found
  if (nrow(yield_files) == 0) {
    return(list(error = "No yield files found for the selected field and crop."))
  }
  
  # Make yield_files accessible in the parent environment for later use
  assign("yield_files", yield_files, envir = parent.frame())
  
  ## incProgress(0.3, detail = "Extracting yield data")
  
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
  
  
  
  max_distance <- ceiling(max(yield_df_xy$dist_to_edge, na.rm = TRUE))
  
  # Categorize yield points into 5m bins
  if (max_distance > 10) {
    yield_df_xy$distance_category <- cut(yield_df_xy$dist_to_edge, 
                                         breaks = seq(0, max_distance + 5, by = 5),
                                         labels = paste0(seq(5, max_distance + 5, by = 5), "m"),
                                         include.lowest = TRUE,
                                         right = TRUE)
  } else {
    stop("Error: Not enough variation in distance for ANOVA. Check yield data distribution.")
  }
  mean_data <-yield_df_xy %>% group_by(distance_category) %>% summarise(mean_yield = mean(avg_yield, na.rm = TRUE), .groups = "drop")
  

  
  # Box Plot: Yield vs. Distance from Edge
  box_plot <- ggplot(yield_df_xy, aes(x = distance_category, y = avg_yield)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    geom_line(data = mean_data,
              aes(x = distance_category, y = mean_yield),
              # position = position_dodge(width = 0.75),
              linewidth = 1,
              alpha = 0.9) +
    # geom_point(data = mean_data,
    #            aes(x = distance_category, y = mean_yield,),
    #            # position = position_dodge(width = 0.75),
    #            size = 2,
    #            shape = 21,
    #            fill = "white") +
    labs(title = "Edge Effect Analysis: Yield vs. Distance from Edge",
         subtitle = paste(field_name , crop_name, sep = " , "),
         x = "Distance from Edge (5m bins)",
         y = "Average Yield  (bu/acre)") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 8))+
    theme(
      plot.title = element_text(size = 10, face = "bold", color = "#2C3E50", hjust = 0.5),
      plot.subtitle = element_text(size = 8, color = "#7F8C8D", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#BDC3C7", fill = NA)
    )
  
  
  plot(box_plot)
  
  # Load and align rasters for the leaflet map
  rasters <- map(yield_files$file_path, ~ rast(.x))
  
  
  raster_stack <- rast(rasters)
  yield_avg_raster <- app(raster_stack, mean, na.rm = TRUE)
  
  # Fit base linear model for breakpoint detection
  yield_sf <- st_as_sf(yield_avg)
  
  # Compute max distance safely
  max_distance <- ceiling(max(yield_sf$dist_to_edge, na.rm = TRUE))
  
  # Categorize yield points into 5m bins
  if (max_distance > 10) {
    yield_sf$distance_category <- cut(yield_sf$dist_to_edge, 
                                      breaks = seq(0, max_distance + 5, by = 5),
                                      labels = paste0(seq(5, max_distance + 5, by = 5), "m"),
                                      include.lowest = TRUE,
                                      right = TRUE)
  } else {
    return(list(error = "Not enough variation in distance for analysis."))
  }
  
  
  #  Fit base linear model
  lm_base <- lm(avg_yield ~ dist_to_edge, data = yield_sf)
  
  #  Fit segmented model with an initial guess for breakpoint at 20m
  segmented_model <- tryCatch({
    segmented(lm_base, seg.Z = ~ dist_to_edge, psi = list(dist_to_edge = 20))
  }, error = function(e) {
    return(NULL)
  })
  
  #  Run Davies Test to check for a significant breakpoint
  davies_result <- tryCatch({
    davies.test(lm_base, seg.Z = ~ dist_to_edge)
  }, error = function(e) {
    return(NULL)
  })
  
  # Initialize variables
  breakpoint_val <- NULL
  breakpoint_plot <- NULL
  
  # Step 4: Check significance and visualize
  if (!is.null(segmented_model) && 
      !is.null(davies_result) && 
      davies_result$p.value < 0.05 &&
      !is.null(segmented_model$psi)) {
    
    # Use the first breakpoint estimate
    breakpoint_val <- segmented_model$psi[1, "Est."]
    
    # Add segmented prediction to dataset
    yield_sf$segmented_pred <- predict(segmented_model)
    
    # Create breakpoint plot
    breakpoint_plot <- ggplot(yield_sf, aes(x = dist_to_edge, y = avg_yield)) +
      geom_point(alpha = 0.4, color = "gray30") +
      geom_line(aes(y = predict(lm_base)), color = "red", linetype = "dashed") +
      geom_line(aes(y = segmented_pred), color = "blue", linewidth = 1) +
      geom_vline(xintercept = breakpoint_val, linetype = "dotted", color = "darkgreen", linewidth = 1) +
      labs(
        title = paste("Segmented Regression Analysis \n", 
                      field_name, " , ", crop_name),
        subtitle = paste("Breakpoint Val:", round(breakpoint_val, 3)),
        x = "Distance from the Edge (m)",        
        y = "Average Yield (bu/acre)"
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "#7F8C8D", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "#BDC3C7", fill = NA)
      )
  } else {
    breakpoint_val <- NA
  }
  
  # Always define these, even if there's no breakpoint
  avg_points_inside_breakpoint_region <- NA
  avg_points_outside_breakpoint_region <- NA
  points_in_breakpoint_region <- NULL
  
  
  # Calculate breakpoint region
  if (!is.na(breakpoint_val)) {
    lower <- floor(breakpoint_val / 5) * 5
    upper <- ceiling(breakpoint_val / 5) * 5
    
    yield_df <<- yield_avg %>%
      mutate(
        x = st_coordinates(.)[, 1],
        y = st_coordinates(.)[, 2],
        is_breakpoint = dist_to_edge >= lower & dist_to_edge <= upper
      )
    
    points_in_breakpoint_region <- yield_avg %>% 
      filter(dist_to_edge > lower & dist_to_edge < upper)
    
    points_outside_breakpoint_region <- yield_avg %>% 
      filter(dist_to_edge > 0 & dist_to_edge < lower)
    
    avg_points_outside_breakpoint_region <- mean(points_outside_breakpoint_region$avg_yield)
    
    points_inside_breakpoint_region <- yield_avg %>% 
      filter(dist_to_edge > upper)
    
    avg_points_inside_breakpoint_region <- mean(points_inside_breakpoint_region$avg_yield)
    
  } else {
    points_in_breakpoint_region <- NULL
  }
  
  
  rasters <- map(yield_files$file_path, ~ rast(.x))
  
  
  #===========================================================================================================================
  
  # Build raster stack and calculate average
  raster_stack <- rast(rasters)
  yield_avg_raster <- app(raster_stack, mean, na.rm = TRUE)
  
  # Calculate global yield range across all rasters
  yield_range <- range(values(yield_avg_raster), na.rm = TRUE)
  all_rasters <- c(list(yield_avg_raster), rasters)
  yield_range <- range(unlist(lapply(all_rasters, function(r) values(r))), na.rm = TRUE)
  
  # REUSABLE COMPONENTS FOR CONSISTENT SCALING
  # Create 10 equal breaks - EXPORTABLE for other maps
  yield_breaks <- seq(yield_range[1], yield_range[2], length.out = 11)
  
  # Create discrete colors - EXPORTABLE for other maps  
  discrete_colors <- viridis(length(yield_breaks) - 1)
  
  # Create nice labels for legend
  yield_labels <- paste0(round(yield_breaks[-length(yield_breaks)]), " - ", 
                         round(yield_breaks[-1]))
  
  # SHARED PALETTES for different map types
  # For leaflet maps
  common_yield_palette <- colorNumeric(
    palette = viridis(100),
    domain = yield_range,
    na.color = "transparent"
  )
  
  discrete_legend_pal <- colorBin(
    palette = viridis(length(yield_breaks) - 1),
    domain = yield_range,
    bins = yield_breaks,
    na.color = "transparent"
  )
  
  # Prepare raster data for plotting
  raster_df <- as.data.frame(yield_avg_raster, xy = TRUE, na.rm = TRUE)
  colnames(raster_df) <- c("x", "y", "avg_yield")
  
  # Create discrete bins for proper legend alignment
  raster_df$yield_bin <- cut(raster_df$avg_yield, 
                             breaks = yield_breaks, 
                             include.lowest = TRUE,
                             labels = yield_labels)
  
  # Convert to factor with ALL levels to ensure all bins show colors
  raster_df$yield_bin <- factor(raster_df$yield_bin, levels = yield_labels)
  
  # Add dummy rows for missing levels to force all colors to appear
  missing_levels <- setdiff(yield_labels, unique(raster_df$yield_bin))
  if(length(missing_levels) > 0) {
    dummy_rows <- data.frame(
      x = rep(NA, length(missing_levels)),
      y = rep(NA, length(missing_levels)), 
      avg_yield = rep(NA, length(missing_levels)),
      yield_bin = factor(missing_levels, levels = yield_labels)
    )
    raster_df <- rbind(raster_df, dummy_rows)
  }
  
  # REUSABLE SCALE FUNCTION for consistent mapping
  create_discrete_yield_scale <- function() {
    scale_fill_manual(
      name = "Avg Yield (bu/acre)",
      values = setNames(discrete_colors, yield_labels),
      na.value = "transparent",
      drop = FALSE,  # Show all levels even if not present
      breaks = yield_labels,  # Explicitly specify breaks
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        ncol = 1,
        # keywidth = unit(2, "cm"),
        # keyheight = unit(1, "cm"),
        label.theme = element_text(size = 9),
        title.theme = element_text(size = 11, face = "bold"),
        override.aes = list(color = "black", size = 0.2, alpha = 1),  # Ensure all keys are visible
        reverse = TRUE  # Reverse order so highest values are on top
      )
    )
  }
  
  # Create the improved plot
  avg_yield_map_wtih_breakpoint <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = yield_bin)) +
    geom_sf(data = field_boundary, fill= "transparent", color = "black", linewidth= 1.25)+
    create_discrete_yield_scale() +
    labs(
      title = paste(field_name, "-", crop_name),
      subtitle = "Average Yield Map (All Years)",
      caption = if(!is.null(points_in_breakpoint_region)) {
        paste("Breakpoint detected:", round(breakpoint_val,2), "m")
      } else {
        "No BreakPoint Detected"
      }
    ) +
    # Add breakpoint region if it exists
    {if(!is.null(points_in_breakpoint_region)) 
      geom_sf(data = points_in_breakpoint_region, color = "red", size = 0.8, alpha = 0.9, inherit.aes = FALSE)} +
    coord_sf(expand = FALSE) +
    theme_void() +  # Use theme_void for cleaner look
    theme(
      # Plot titles
      plot.title = element_text(size = 14, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(size = 10, color = "#95A5A6", hjust = 0.5, margin = margin(t = 5)),
      
      # Panel
      panel.border = element_rect(color = "#BDC3C7", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      # Legend positioning
      legend.position = "right",
      legend.margin = margin(l = 15)
    )
  
  # Display the plot
  print(avg_yield_map_wtih_breakpoint)
  
  
  shared_scale_components <- list(
    yield_breaks = yield_breaks,
    yield_labels = yield_labels,
    create_discrete_yield_scale = create_discrete_yield_scale
  )
  
  
  
  
  # Return all needed results
  return(list(
    yield_avg_raster = yield_avg_raster,
    field_boundary = field_boundary,
    points_in_breakpoint_region = points_in_breakpoint_region,
    breakpoint_val = breakpoint_val,
    breakpoint_plot = breakpoint_plot,
    box_plot = box_plot,
    avg_of_different_regions = c("avg_points_inside_breakpoint_region"=avg_points_inside_breakpoint_region, "avg_points_outside_breakpoint_region"=avg_points_outside_breakpoint_region),
    davies_pvalue = if(!is.null(davies_result)) davies_result$p.value else NA,
    avg_yield_map_wtih_breakpoint = avg_yield_map_wtih_breakpoint,
    overall_years_field_yield_average = overall_years_field_yield_average,
    overall_years_field_yield_variance = overall_years_field_yield_variance,
    shared_scale_components= shared_scale_components,
    error = NULL
  ))
}
