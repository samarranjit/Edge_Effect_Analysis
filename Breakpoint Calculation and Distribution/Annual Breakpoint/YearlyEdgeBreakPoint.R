# 7/25/2025 We are now tring to analyse the strength at the breakpoints

YearlyEdgeBreakPoint <- function(raster_path, year, shared_scale_components){
  #Get Field Boundary
  #==========================================================
  get_field_boundary <- function(raster_path)
      {
        resolution = 5
        concavity = -1
        # Step 1: Load and convert data
        r <- rast(raster_path)
        
        yield_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
        colnames(yield_df) <- c("x", "y", "avg_yield")
        
        # Use the correct EPSG code from the original raster
        original_crs <- crs(r)
        yield_points <- vect(yield_df, geom = c("x", "y"), crs = original_crs)
        sf_points <- st_as_sf(yield_df, coords = c("x", "y"), crs = st_crs(original_crs))
        
        # Step 2: Group points into clusters
        # This step is crucial - we'll first group nearby points
        message("Clustering points...")
        # Convert to a matrix for clustering
        point_coords <- st_coordinates(sf_points)
        
        cluster_distance <- resolution * 2
        
        # Use hierarchical clustering
        clusters <- hclust(dist(point_coords), method = "single")
        # Cut the dendrogram to create clusters
        # This height determines how far apart points can be to be in the same cluster
        cluster_groups <- cutree(clusters, h = cluster_distance)
        
        # Add cluster info to the points
        sf_points$cluster <- cluster_groups
        
        # Step 3: Create boundaries for each cluster separately
        message("Creating boundaries for each cluster...")
        all_boundaries <- list()
        
        for (cluster_id in unique(sf_points$cluster)) {
          # Get points in this cluster
          cluster_points <- sf_points[sf_points$cluster == cluster_id, ]
          
          # Only proceed if there are enough points to form a polygon
          if (nrow(cluster_points) > 3) {
            # Use concaveman with higher concavity to follow points more closely
            # Higher concavity values create tighter boundaries around points
            try({
              cluster_boundary <- concaveman(cluster_points, 
                                             concavity = concavity, 
                                             length_threshold = resolution/2)
              all_boundaries[[length(all_boundaries) + 1]] <- cluster_boundary
            }, silent = TRUE)
          }
        }
        
        # Combine all cluster boundaries
        if (length(all_boundaries) > 0) {
          field_boundary <- do.call(rbind, all_boundaries)
        } else {
          # Fallback if clustering approach fails
          message("Clustering approach failed, trying direct concaveman...")
          field_boundary <- concaveman(sf_points, concavity = concavity, length_threshold = resolution/2)
        }
        
        # plot(field_boundary)
        return(field_boundary)
      }
  
  
  #==========================================================
  
  
  
  resolution = 5
  # # Step 1: Load and convert data
  # #incProgress(0.1, detail = "Loading raster data")
  r <- rast(raster_path)
  # 
  yield_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(yield_df) <- c("x", "y", "avg_yield")
  # 
  # # Use the correct EPSG code from the original raster
  original_crs <- crs(r)
  #incProgress(0.2, detail = "Converting coordinate systems")
  yield_points <- vect(yield_df, geom = c("x", "y"), crs = original_crs)
  sf_points <- st_as_sf(yield_df, coords = c("x", "y"), crs = st_crs(original_crs))
  
  # Step 2: Group points into clusters
  # #incProgress(0.3, detail = "Clustering points") 
  # Convert to a matrix for clustering
  point_coords <- st_coordinates(sf_points)
  
  # Define clustering distance
  cluster_distance <- resolution * 2
  
  # Use hierarchical clustering
  clusters <- hclust(dist(point_coords), method = "single")
  # Cut the dendrogram to create clusters
  cluster_groups <- cutree(clusters, h = cluster_distance)
  
  # Add cluster info to the points
  sf_points$cluster <- cluster_groups
  
  # Step 3: Create boundaries for each cluster separately
  #incProgress(0.4, detail = "Creating field boundaries")
  
  # Convert to terra vector format
  field_boundary <- get_field_boundary(raster_path)
  field_boundary_v <- vect(field_boundary)
  
  # Step 4: Template raster
  #incProgress(0.6, detail = "Creating template raster")
  r_template <- rast(ext(field_boundary_v), resolution = resolution)
  crs(r_template) <- original_crs
  
  # Step 5: Rasterize and mask
  #incProgress(0.7, detail = "Rasterizing data")
  r_yield <- rasterize(yield_points, r_template, field = "avg_yield", fun = mean)
  r_yield_masked <- mask(r_yield, field_boundary_v)
  mean_field_yield <- global(r_yield_masked, mean, na.rm = TRUE)[1,1]
  print(paste("Mean field yield:", round(mean_field_yield, 2)))
  #=============================================================================================
  # Perform breakpoint analysis
  #=============================================================================================
  #incProgress(0.8, detail = "Running breakpoint analysis")
  
  yield_sf <- sf_points
  
  # Determining distance to edge for each pixel
  field_edges <- st_cast(st_union(field_boundary), "MULTILINESTRING")
  yield_sf$dist_to_edge <- as.numeric(st_distance(yield_sf, field_edges))
  
  # Compute max distance safely
  max_distance <- ceiling(max(yield_sf$dist_to_edge, na.rm = TRUE))
  
  # Categorize yield points into 5m bins
  if (max_distance > 10) {
    yield_sf$distance_category <- cut(yield_sf$dist_to_edge, 
                                      breaks = seq(0, max_distance + 5, by = 5),
                                      labels = paste0(seq(5, max_distance + 5, by = 5), "m"),
                                      #include.lowest = TRUE,
                                      right = TRUE)
  } else {
    warning("Not enough variation in distance for ANOVA. Check yield data distribution.")
  }
  
  # Segmented Regression and Davies Test
  # Step 1: Fit base linear model
  lm_base <- lm(avg_yield ~ dist_to_edge, data = yield_sf)
  
  # Step 2: Fit segmented model with an initial guess for breakpoint at 20m
  segmented_model <- tryCatch({
    segmented(lm_base, seg.Z = ~ dist_to_edge, psi = list(dist_to_edge = 20))
  }, error = function(e) {
    message("⚠️ Segmented model failed:", e$message)
    return(NULL)
  })
  
  # Step 3: Run Davies Test to check for a significant breakpoint
  davies_result <- tryCatch({
    davies.test(lm_base, seg.Z = ~ dist_to_edge)
  }, error = function(e) {
    message("⚠️ Davies test failed:", e$message)
    return(NULL)
  })
  
  bp_result<- list()
  
  # Step 4: Process results
  if (!is.null(segmented_model) && 
      !is.null(davies_result) && 
      davies_result$p.value < 0.05 &&
      !is.null(segmented_model$psi)) {
    
    # Use the first breakpoint estimate
    breakpoint_val <- segmented_model$psi[1, "Est."]
    
    #=======================================================================================
    # Calculate slopes before and after the breakpoint
    slopes <- slope(segmented_model)$dist_to_edge
    slope_before <- slopes[1]
    slope_after <- slopes[2]
    print(paste("Slope before breakpoint:", slope_before))
    print(paste("Slope after breakpoint:", slope_after))
    # Calculate breakpoint strength (gradient change)
    breakpoint_strength <- abs(slope_after - slope_before)
    
    print(paste("Breakpoint detected at", round(breakpoint_val, 2), "m with strength", round(breakpoint_strength, 2)))
    #=======================================================================================
    
    #Calculate the R^2 for the lines before and after the breakpoint:
    
    # Split yield data into segments before and after the breakpoint
    segment_before <- yield_sf %>%
      filter(dist_to_edge <= breakpoint_val)
    
    segment_after <- yield_sf %>%
      filter(dist_to_edge > breakpoint_val)
    
    # Fit linear models to each segment
    lm_before <- lm(avg_yield ~ dist_to_edge, data = segment_before)
    lm_after <- lm(avg_yield ~ dist_to_edge, data = segment_after)
    
    # Get R² values
    r2_before <- summary(lm_before)$r.squared
    r2_after <- summary(lm_after)$r.squared
    print(paste("R² before breakpoint:", round(r2_before, 3)))
    print(paste("R² after breakpoint:", round(r2_after, 3)))
    
    
    
    
    #=========================================================================================
    
    # Add segmented prediction to dataset
    yield_sf$segmented_pred <- predict(segmented_model)
    
    # Create breakpoint plot
    bp_plot <- ggplot(yield_sf, aes(x = dist_to_edge, y = avg_yield)) +
      geom_point(alpha = 0.4, color = "gray30") +
      geom_line(aes(y = predict(lm_base)), color = "red", linetype = "dashed") +
      geom_line(aes(y = segmented_pred), color = "blue", linewidth = 1) +
      geom_vline(xintercept = breakpoint_val, linetype = "dotted", color = "darkgreen", linewidth = 1) +
      labs(
        title = paste0("Segmented Regression: Yield vs Distance", " - ", field_name, " - ", crop_name),
        subtitle = paste("Breakpoint at", round(breakpoint_val, 2), "m"),
        x = "Distance from Edge (m)", y = "Average Yield"
      ) +
      theme_minimal()
    
    plot(bp_plot)
    
    # Save breakpoint to your dataset
    yield_sf$breakpoint <- breakpoint_val
    
    # Convert to dataframe with coordinates for calculation
    yield_df <- yield_sf %>%
      mutate(x = st_coordinates(.)[, 1],
             y = st_coordinates(.)[, 2])
    
    # Nearest lower and upper multiples of 5
    lower <- floor(breakpoint_val / 5) * 5
    upper <- ceiling(breakpoint_val / 5) * 5
    
    yield_df <- yield_df %>%
      mutate(
        is_breakpoint = dist_to_edge >= lower & dist_to_edge <= upper
      )
    
    # Calculate stats for different regions
    points_outside_breakpoint_region <- yield_df %>% 
      filter(dist_to_edge > 0 & dist_to_edge < breakpoint_val)
    outside_mean <- mean(points_outside_breakpoint_region$avg_yield, na.rm = TRUE)
    
    points_inside_breakpoint_region <- yield_df %>% 
      filter(dist_to_edge > breakpoint_val & dist_to_edge < max_distance)
    inside_mean <- mean(points_inside_breakpoint_region$avg_yield, na.rm = TRUE)
    
    points_in_breakpoint_region <- yield_df %>% 
      filter(dist_to_edge > lower & dist_to_edge < upper)
    breakpoint_mean <- mean(points_in_breakpoint_region$avg_yield, na.rm = TRUE)
    
    # Store breakpoint results
    bp_results <- list(
      success = TRUE,
      breakpoint_val = breakpoint_val,
      lower = lower,
      upper = upper,
      davies_pvalue = davies_result$p.value,
      outside_mean = outside_mean,
      inside_mean = inside_mean,
      breakpoint_mean = breakpoint_mean,
      bp_plot = bp_plot,
      points_in_breakpoint_region = st_transform(points_in_breakpoint_region, crs = 4326),
      slope_before = slope_before,
      slope_after = slope_after,
      breakpoint_strength = breakpoint_strength,
      mean_yield = mean_field_yield
    )
    
  } else {
    # No significant breakpoint found
    bp_results <- list(
      success = FALSE,
      davies_pvalue = ifelse(!is.null(davies_result), davies_result$p.value, NA)
    )
    bp_plot <- ggplot(yield_sf, aes(x = dist_to_edge, y = avg_yield)) +
      geom_point(alpha = 0.4, color = "gray30")
  }
  
  #===============================================================================================
  library(mgcv)
  
  # Fit a smooth yield vs distance model
  fit <- gam(avg_yield ~ s(dist_to_edge), data = yield_sf)
  
  # Predict the linear predictor matrix
  X <- predict(fit, type = "lpmatrix")  # lpmatrix = "linear predictor matrix"
  
  # Extract model coefficients
  b <- coef(fit)
  
  # Compute fitted values
  fitted_vals <- X %*% b
  
  # Approximate the derivative (change in yield per meter)
  dx <- diff(yield_sf$dist_to_edge)
  dy <- diff(as.vector(fitted_vals))
  gradient <- dy / dx
  
  # Optionally plot
  plot(yield_sf$dist_to_edge[-1], gradient, type = "l", main = "Estimated Yield Gradient", ylab = "dy/dx", xlab = "Distance to Edge")
  fit <- gam(avg_yield ~ s(dist_to_edge), data = yield_sf)
  
  # Compute pseudo-R²
  fitted_vals <- predict(fit)
  residuals <- yield_sf$avg_yield - fitted_vals
  
  ss_total <- sum((yield_sf$avg_yield - mean(yield_sf$avg_yield))^2)
  ss_resid <- sum(residuals^2)
  
  pseudo_r2 <- 1 - (ss_resid / ss_total)
  print(paste0("R2 Vlaue:",pseudo_r2))
  
  print(paste0("GCV Value: ",summary(fit)$dev.expl))
  
  
  #===============================================================================================
  
  
  # Store results in reactive value
  # breakpoint_results(bp_results)
  
  # Step 6: Plot on Leaflet
  #incProgress(0.9, detail = "Creating map")
  
  # Project to WGS84 for leaflet
  r_yield_masked <- project(r_yield_masked, "EPSG:4326")
  field_boundary_t <- st_transform(field_boundary, 4326)
  
  # Create color palette
  pal <- colorNumeric(palette = viridis(7), domain = values(r_yield_masked), na.color = "transparent")
  
  raster_df <- as.data.frame(r_yield_masked, xy = TRUE, na.rm = TRUE)
  colnames(raster_df) <- c("x", "y", "avg_yield")
  
  # Convert field boundary to WGS84 for consistent plotting
  field_boundary_t <- st_transform(field_boundary, 4326)
  
  # Extract components from the shared scale
  yield_breaks <- shared_scale_components$yield_breaks
  yield_labels <- shared_scale_components$yield_labels
  create_discrete_yield_scale <- shared_scale_components$create_discrete_yield_scale
  
  # ✅ ADD THIS SECTION TO BIN THE DATA:
  # BIN THE DATA using the same breaks as the main function
  raster_df$yield_bin <- cut(raster_df$avg_yield, 
                             breaks = yield_breaks, 
                             include.lowest = TRUE,
                             labels = yield_labels)
  
  # Convert to factor with ALL levels
  raster_df$yield_bin <- factor(raster_df$yield_bin, levels = yield_labels)
  
  # Add dummy rows for missing levels to ensure all colors show
  missing_levels <- setdiff(yield_labels, unique(raster_df$yield_bin[!is.na(raster_df$yield_bin)]))
  if(length(missing_levels) > 0) {
    dummy_rows <- data.frame(
      x = rep(NA, length(missing_levels)),
      y = rep(NA, length(missing_levels)), 
      avg_yield = rep(NA, length(missing_levels)),
      yield_bin = factor(missing_levels, levels = yield_labels)
    )
    raster_df <- rbind(raster_df, dummy_rows)
  }
  
  # ✅ NOW CHANGE THIS LINE:
  # Base ggplot map with raster
  gg_yield_map <- ggplot() +
    geom_raster(data = raster_df, aes(x = x, y = y, fill = yield_bin)) +  # ✅ Use yield_bin instead of avg_yield
    create_discrete_yield_scale()+
    geom_sf(data = field_boundary_t, fill = NA, color = "black", size = 0.8) +
    labs(title = year,
         subtitle = paste(field_name, ",", crop_name),
         caption = if (!is.null(bp_results$points_in_breakpoint_region)) {
           paste("Breakpoint detected:", round(bp_results$breakpoint_val, 2), "m")
         } else {
           "No BreakPoint Detected"
         },
         x = "", y = "") +
    coord_sf(expand = FALSE)+
  theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "#2C3E50", hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "#7F8C8D", hjust = 0.5),
      plot.caption = element_text(size = 10, color = "#95A5A6", hjust = 0.5, margin = margin(t = 5)),
      # panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#BDC3C7", fill = NA),
      axis.text.x = element_blank(),    # removes longitude numbers
      axis.text.y = element_blank(),    # removes latitude numbers
      axis.ticks.x = element_blank(),   # removes x-axis ticks
      axis.ticks.y = element_blank(),   # removes y-axis ticks
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.margin = margin(l = 15)
    )
  
  # Add breakpoint region if available
  if (!is.null(bp_results$success) && bp_results$success && !is.null(bp_results$points_in_breakpoint_region)) {
    gg_yield_map <- gg_yield_map +
      geom_sf(data = bp_results$points_in_breakpoint_region, 
              color = "red", size = 0.6, alpha = 0.9)
  }
  
  # Print the plot
  print(gg_yield_map)
  
  
  
  return ( list(
    bp_results = bp_results,
    yearPlot = gg_yield_map
  ))
}
