#Sctipt to get the field neightbour automatically labeled

#Requires the extract directional edge function and get field files function
  

allFieldsData <- read.xlsx("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge Specific Analysis/Results/20m_Phase_2/Results.xlsx")
  View(allFieldsData)

runAnalysis <- function(){
  
  for(n in 1:nrow(allFieldsData)) {
    
    year <- allFieldsData$Year[n]
    print("---------------------------------------------------------------------------------------------------------------")
    print("---------------------------------------------------------------------------------------------------------------")
    print(paste("📌 We are working on", allFieldsData[n,"FieldName"],"for year", year))
    
    tryCatch({
      
      resultToSave <- automate_surrounding_labeling(field_name = allFieldsData$FieldName[n], crop_name = allFieldsData$Crop[n], year = allFieldsData$Year[n])
       message(paste("Done for ", allFieldsData$FieldName[n], "\n Here are the Results:- \n"))
      print(resultToSave)
      existingRows <- read.xlsx("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge Specific Analysis/Results/automated surrounding labeling/AutomatedLabels.xlsx", colNames = TRUE)
      updatedData <- rbind(existingRows, resultToSave)
      write.xlsx(updatedData, file ="C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge Specific Analysis/Results/automated surrounding labeling/AutomatedLabels.xlsx", overwrite = TRUE )
      print(paste("✅ We have done working on: ", allFieldsData[n,"FieldName"],"for year", year))
      print("---------------------------------------------------------------------------------------------------------------")
      print("---------------------------------------------------------------------------------------------------------------")
     
  }
  ,
             error = function(err){

               errortoSave = data.frame(
                 FieldName=allFieldsData$FieldName[n],
                 Crop=allFieldsData$Crop[n] ,
                 Year= year,
                 error = err$message
               )
               print("🔴🔴🔴 We detected an error")
               existingRows <- read.xlsx("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge Specific Analysis/Results/automated surrounding labeling/error.xlsx", colNames = TRUE)
               updatedData <- rbind(existingRows, errortoSave)
               write.xlsx(updatedData, file ="C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge Specific Analysis/Results/automated surrounding labeling/error.xlsx", overwrite = TRUE )

             })
    
    # stop()
  }
}




automate_surrounding_labeling <- function (field_name, crop_name, year){
  library(sf)
  data_dir <- file.path(getwd(), "Data_Raw/v1.1")
  field_shp_file <- get_yield_and_shapefile_for_field_year_crop(field_name = field_name, crop_name = crop_name, year = year, base_dir = data_dir)$shapefile_path
  field_boundary <- st_read(field_shp_file);
  
  create_buffer_leaflet_map <- function(field_boundary, buffer_zone, bbox_polygon, 
                                        buffer_name = "Buffer Zone", zoom_level = 16) {
    
    library(leaflet)
    library(sf)
    library(htmltools)
    
    # Transform all objects to WGS84 (required for leaflet)
    field_wgs84 <- st_transform(field_boundary, crs = 4326)
    buffer_wgs84 <- st_transform(buffer_zone, crs = 4326)
    bbox_wgs84 <- st_transform(bbox_polygon, crs = 4326)
    
    # Calculate center point for map
    buffer_center <- st_centroid(buffer_wgs84)
    center_coords <- st_coordinates(buffer_center)
    
    # Calculate areas for popup info
    field_area <- round(as.numeric(st_area(field_wgs84)), 0)
    buffer_area <- round(as.numeric(st_area(buffer_wgs84)), 0)
    bbox_area <- round(as.numeric(st_area(bbox_wgs84)), 0)
    
    # Create the leaflet map
    map <- leaflet() %>%
      # Add OpenStreetMap base layer
      addTiles(group = "OpenStreetMap") %>%
      
      # Alternative base layers
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      
      # Set initial view
      setView(lng = center_coords[1], lat = center_coords[2], zoom = zoom_level) %>%
      
      # Add field boundary
      addPolygons(
        data = field_wgs84,
        fillColor = "green",
        fillOpacity = 0.3,
        color = "darkgreen",
        weight = 2,
        opacity = 0.8,
        group = "Field Boundary",
        popup = paste0(
          "<b>Field Boundary</b><br>",
          "Area: ", format(field_area, big.mark = ","), " m²<br>",
          "Area: ", round(field_area / 10000, 2), " hectares"
        ),
        label = "Field Boundary"
      ) %>%
      
      # Add buffer zone
      addPolygons(
        data = buffer_wgs84,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "blue",
        weight = 2,
        opacity = 0.8,
        group = buffer_name,
        popup = paste0(
          "<b>", buffer_name, "</b><br>",
          "Area: ", format(buffer_area, big.mark = ","), " m²<br>",
          "Area: ", round(buffer_area / 10000, 4), " hectares<br>",
          "% of field: ", round((buffer_area/field_area)*100, 1), "%"
        ),
        label = buffer_name
      ) %>%
      
      # Add bounding box
      addPolygons(
        data = bbox_wgs84,
        fillColor = "transparent",
        fillOpacity = 0,
        color = "red",
        weight = 2,
        opacity = 0.8,
        dashArray = "10,10",  # Dashed line effect
        group = "OSM Bounding Box",
        popup = paste0(
          "<b>OSM Query Bounding Box</b><br>",
          "Area: ", format(bbox_area, big.mark = ","), " m²<br>",
          "Area: ", round(bbox_area / 10000, 2), " hectares<br>",
          "Efficiency: ", round((buffer_area/bbox_area)*100, 1), "%<br>",
          "<i>(How much of query area is actually useful)</i>"
        ),
        label = "OSM Query Area"
      ) %>%
      
      # Add layer control
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite", "Light"),
        overlayGroups = c("Field Boundary", buffer_name, "OSM Bounding Box"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      # Add scale bar
      addScaleBar(position = "bottomleft") %>%
      
      # Add mini map
      addMiniMap(
        tiles = providers$OpenStreetMap,
        position = "bottomright",
        width = 120, height = 80
      ) %>%
      
      # Add legend
      addLegend(
        position = "topright",
        colors = c("green", "blue", "red"),
        labels = c("Field Boundary", buffer_name, "OSM Query Box"),
        opacity = 0.7,
        title = "Map Layers"
      )
    
    return(map)
  }
  
  
  get_west_edge_buffer_zone <- function(shapefile_path, buffer_distance = 50) {
    library(sf)
    library(dplyr)
    
    # Read field and transform to WGS84
    field_poly <- st_read(shapefile_path, quiet = TRUE)#
    original_crs <- st_crs(field_poly)
    
    field_poly <- st_transform(field_poly, crs = 3857)  # Or appropriate UTM zone
    
    # Get directional edges
    edge_info <- extract_field_edges(shapefile_path)
    west_edges <- edge_info$edges %>% filter(Direction == "West")
    
    if (nrow(west_edges) == 0) {
      stop("No West edge found in this field.")
    }
    
    # Convert edges to sf LINESTRINGs
    west_lines <- west_edges %>%
      rowwise() %>%
      mutate(
        geometry = st_sfc(
          st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2, byrow = FALSE)),
          crs = 4326
        )
      ) %>%
      st_as_sf()
    
    # ADD THIS LINE HERE:
    west_lines <- st_transform(west_lines, crs = st_crs(field_poly))
    
    # Merge all west segments into one geometry
    merged_west_line <- st_union(west_lines)
    
    # Create buffer zone to the LEFT of the west edge
    offset_vec <- c(-buffer_distance/2, 0)  # Shift westward
    shifted_line <- st_geometry(merged_west_line) + offset_vec
    st_crs(shifted_line) <- st_crs(field_poly)  # Reassign CRS
    
    # Build final polygon zone
    buffer_zone <- st_buffer(shifted_line, dist = buffer_distance / 2)
    
    return(buffer_zone)
  }
  
  
  get_east_edge_buffer_zone <- function(shapefile_path, buffer_distance = 50) {
    library(sf)
    library(dplyr)
    
    # Read field and transform to WGS84
    field_poly <- st_read(shapefile_path, quiet = TRUE)#
    original_crs <- st_crs(field_poly)
    
    field_poly <- st_transform(field_poly, crs = 3857)  # Or appropriate UTM zone
    
    # Get directional edges
    edge_info <- extract_field_edges(shapefile_path)
    east_edges <- edge_info$edges %>% filter(Direction == "East")
    
    if (nrow(east_edges) == 0) {
      stop("No East edge found in this field.")
    }
    
    # Convert edges to sf LINESTRINGs
    east_lines <- east_edges %>%
      rowwise() %>%
      mutate(
        geometry = st_sfc(
          st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2, byrow = FALSE)),
          crs = 4326
        )
      ) %>%
      st_as_sf()
    
    east_lines <- st_transform(east_lines, crs = st_crs(field_poly))
    
    # Merge all east segments into one geometry
    merged_east_line <- st_union(east_lines)
    
    # Create buffer zone to the RIGHT of the east edge
    offset_vec <- c(buffer_distance/2, 0)  # Shift eastward (positive direction)
    shifted_line <- st_geometry(merged_east_line) + offset_vec
    st_crs(shifted_line) <- st_crs(field_poly)  # Reassign CRS
    
    # Build final polygon zone
    buffer_zone <- st_buffer(shifted_line, dist = buffer_distance / 2)
    
    return(buffer_zone)
  }
  
  
  get_north_edge_buffer_zone <- function(shapefile_path, buffer_distance = 50) {
    library(sf)
    library(dplyr)
    
    # Read field and transform to WGS84
    field_poly <- st_read(shapefile_path, quiet = TRUE)#
    original_crs <- st_crs(field_poly)
    
    field_poly <- st_transform(field_poly, crs = 3857)  # Or appropriate UTM zone
    
    # Get directional edges
    edge_info <- extract_field_edges(shapefile_path)
    north_edges <- edge_info$edges %>% filter(Direction == "North")
    
    if (nrow(north_edges) == 0) {
      stop("No East edge found in this field.")
    }
    
    # Convert edges to sf LINESTRINGs
    north_lines <- north_edges %>%
      rowwise() %>%
      mutate(
        geometry = st_sfc(
          st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2, byrow = FALSE)),
          crs = 4326
        )
      ) %>%
      st_as_sf()
    
    north_lines <- st_transform(north_lines, crs = st_crs(field_poly))
    
    # Merge all east segments into one geometry
    merged_north_line <- st_union(north_lines)
    
    # Create buffer zone to the RIGHT of the east edge
    offset_vec <- c(0, buffer_distance/2)  # Shift eastward (positive direction)
    shifted_line <- st_geometry(merged_north_line) + offset_vec
    st_crs(shifted_line) <- st_crs(field_poly)  # Reassign CRS
    
    # Build final polygon zone
    buffer_zone <- st_buffer(shifted_line, dist = buffer_distance / 2)
    
    return(buffer_zone)
  }
  
  
  get_south_edge_buffer_zone <- function(shapefile_path, buffer_distance = 50) {
    library(sf)
    library(dplyr)
    
    # Read field and transform to WGS84
    field_poly <- st_read(shapefile_path, quiet = TRUE)#
    original_crs <- st_crs(field_poly)
    
    field_poly <- st_transform(field_poly, crs = 3857)  # Or appropriate UTM zone
    
    # Get directional edges
    edge_info <- extract_field_edges(shapefile_path)
    south_edges <- edge_info$edges %>% filter(Direction == "South")
    
    if (nrow(south_edges) == 0) {
      stop("No East edge found in this field.")
    }
    
    # Convert edges to sf LINESTRINGs
    south_lines <- south_edges %>%
      rowwise() %>%
      mutate(
        geometry = st_sfc(
          st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2, byrow = FALSE)),
          crs = 4326
        )
      ) %>%
      st_as_sf()
    
    south_lines <- st_transform(south_lines, crs = st_crs(field_poly))
    
    # Merge all east segments into one geometry
    merged_south_line <- st_union(south_lines)
    
    # Create buffer zone to the RIGHT of the east edge
    offset_vec <- c(0, - buffer_distance/2)  # Shift eastward (positive direction)
    shifted_line <- st_geometry(merged_south_line) + offset_vec
    st_crs(shifted_line) <- st_crs(field_poly)  # Reassign CRS
    
    # Build final polygon zone
    buffer_zone <- st_buffer(shifted_line, dist = buffer_distance / 2)
    
    return(buffer_zone)
  }
  
  
  #Labeling zones
  east_zone <- get_east_edge_buffer_zone(shapefile_path = field_shp_file)
  west_zone <- get_west_edge_buffer_zone(shapefile_path = field_shp_file)
  north_zone <- get_north_edge_buffer_zone(shapefile_path = field_shp_file)
  south_zone <- get_south_edge_buffer_zone(shapefile_path = field_shp_file)
  
  # Convert your buffer zone to a proper sf object
  west_zone_sf <- st_sf(
    zone_type = "west_buffer",
    buffer_distance = 50,
    geometry = west_zone
  )
  east_zone_sf <- st_sf(
    zone_type = "east_buffer",
    buffer_distance = 50,
    geometry = east_zone
  )
  north_zone_sf <- st_sf(
    zone_type = "north_buffer",
    buffer_distance = 50,
    geometry = north_zone
  )
  south_zone_sf <- st_sf(
    zone_type = "south_buffer",
    buffer_distance = 50,
    geometry = south_zone
  )
  
  
  
  
  
  
  print("==================================================Lets look what each buffer looks like=========================================================")
  # create_buffer_leaflet_map(field_boundary = field_shp_file, buffer = west_zone, bbox_polygon =  west_zone_sf)
  # create_buffer_leaflet_map(field_boundary = field_shp_file,  buffer = east_zone,  bbox_polygon =  east_zone_sf)
  # create_buffer_leaflet_map(field_boundary = field_shp_file, buffer = north_zone,  bbox_polygon =  north_zone_sf)
  # create_buffer_leaflet_map(field_boundary = field_shp_file, buffer = south_zone,  bbox_polygon =  south_zone_sf)
  # 
  
  
  
  
  
  
  analyze_buffer_pixels_classification <- function(buffer_zone_sf, 
                                                   pixel_resolution = 1, 
                                                   verbose = TRUE) {
    library(osmdata)
    library(sf)
    library(dplyr)
    library(terra)
    library(stars)
    
    # Convert to projected CRS for accurate pixel analysis
    buffer_utm <- st_transform(buffer_zone_sf, crs = st_crs(paste0("+proj=utm +zone=", 
                                                                   floor((st_coordinates(st_centroid(st_transform(buffer_zone_sf, 4326)))[1] + 180) / 6) + 1, 
                                                                   " +datum=WGS84")))
    
    # Also keep WGS84 for OSM queries
    buffer_wgs84 <- st_transform(buffer_zone_sf, crs = 4326)
    bbox_wgs84 <- st_bbox(buffer_wgs84)
    
    if (verbose) {
      cat("=== PIXEL-BASED CLASSIFICATION ANALYSIS ===\n")
      cat("Buffer area:", round(as.numeric(st_area(buffer_utm)), 2), "square meters\n")
      cat("Pixel resolution:", pixel_resolution, "meters\n")
    }
    
    # Create pixel grid
    buffer_extent <- st_bbox(buffer_utm)
    n_cols <- ceiling((buffer_extent$xmax - buffer_extent$xmin) / pixel_resolution)
    n_rows <- ceiling((buffer_extent$ymax - buffer_extent$ymin) / pixel_resolution)
    
    # Create template raster
    template_raster <- rast(
      xmin = buffer_extent$xmin, xmax = buffer_extent$xmax,
      ymin = buffer_extent$ymin, ymax = buffer_extent$ymax,
      ncols = n_cols, nrows = n_rows,
      crs = st_crs(buffer_utm)$wkt
    )
    
    # Create buffer mask
    buffer_mask <- rasterize(vect(buffer_utm), template_raster, field = 1)
    total_pixels <- sum(!is.na(values(buffer_mask)))
    
    if (verbose) cat("Total pixels in buffer:", total_pixels, "\n\n")
    
    # Initialize pixel classification raster (will hold classification IDs)
    pixel_classification <- template_raster
    values(pixel_classification) <- NA
    
    # Classification tracking
    classification_lookup <- data.frame(
      ID = integer(),
      Category = character(),
      Subcategory = character(),
      Detailed_Classification = character(),
      Priority = integer(),
      stringsAsFactors = FALSE
    )
    next_id <- 1
    
    # Define feature categories with priorities (higher number = higher priority)
    feature_categories <- list(
      # Water features (highest priority - most definitive)
      waterways = list(
        key = "waterway", name = "Water", priority = 100,
        subcategory_tags = c("width", "navigable", "intermittent"),
        geometry_buffer = list(points = 15, lines = 8, polygons = 0)
      ),
      natural_water = list(
        key = "natural", name = "Natural", priority = 95,
        values = c("water", "wetland", "lake", "pond"),
        subcategory_tags = c("water", "intermittent"),
        geometry_buffer = list(points = 20, lines = 10, polygons = 0)
      ),
      
      # Buildings (high priority - physical structures)
      buildings = list(
        key = "building", name = "Buildings", priority = 90,
        subcategory_tags = c("building:use", "amenity", "shop", "office", "building:levels"),
        geometry_buffer = list(points = 25, lines = 15, polygons = 0)
      ),
      
      # Transportation infrastructure
      railways = list(
        key = "railway", name = "Railways", priority = 85,
        subcategory_tags = c("service", "usage", "electrified"),
        geometry_buffer = list(points = 20, lines = 6, polygons = 0)
      ),
      highways = list(
        key = "highway", name = "Roads", priority = 80,
        subcategory_tags = c("surface", "lanes", "access", "bicycle"),
        geometry_buffer = list(points = 15, lines = 4, polygons = 0)
      ),
      
      # Amenities and services
      amenities = list(
        key = "amenity", name = "Amenities", priority = 75,
        subcategory_tags = c("cuisine", "healthcare", "capacity"),
        geometry_buffer = list(points = 30, lines = 20, polygons = 0)
      ),
      
      # Power infrastructure
      power = list(
        key = "power", name = "Power", priority = 70,
        subcategory_tags = c("voltage", "cables"),
        geometry_buffer = list(points = 10, lines = 3, polygons = 0)
      ),
      
      # Leisure and tourism
      leisure = list(
        key = "leisure", name = "Leisure", priority = 65,
        subcategory_tags = c("sport", "surface", "access"),
        geometry_buffer = list(points = 25, lines = 15, polygons = 0)
      ),
      tourism = list(
        key = "tourism", name = "Tourism", priority = 60,
        subcategory_tags = c("accommodation", "stars"),
        geometry_buffer = list(points = 25, lines = 15, polygons = 0)
      ),
      
      # Commercial
      shops = list(
        key = "shop", name = "Shops", priority = 55,
        subcategory_tags = c("brand", "operator"),
        geometry_buffer = list(points = 25, lines = 15, polygons = 0)
      ),
      
      # Agriculture (medium priority)
      agriculture = list(
        key = "landuse", name = "Agriculture", priority = 50,
        values = c("farmland", "orchard", "vineyard", "farmyard"),
        subcategory_tags = c("crop", "produce", "trees"),
        geometry_buffer = list(points = 30, lines = 20, polygons = 0)
      ),
      
      # Land use (lower priority - broad categories)
      landuse_general = list(
        key = "landuse", name = "Land Use", priority = 45,
        subcategory_tags = c("operator", "access"),
        geometry_buffer = list(points = 30, lines = 20, polygons = 0)
      ),
      
      # Natural features (lowest priority - background)
      natural_general = list(
        key = "natural", name = "Natural", priority = 52,
        subcategory_tags = c("leaf_type", "wood", "surface"),
        geometry_buffer = list(points = 25, lines = 15, polygons = 0)
      )
    )
    
    # Function to safely query OSM
    safe_osm_query <- function(bbox, key, values = NULL) {
      tryCatch({
        if (is.null(values)) {
          opq(bbox, timeout = 60) %>%
            add_osm_feature(key = key) %>%
            osmdata_sf()
        } else {
          opq(bbox, timeout = 60) %>%
            add_osm_feature(key = key, value = values) %>%
            osmdata_sf()
        }
      }, error = function(e) {
        if (verbose) cat("  Warning: Could not query", key, "\n")
        return(list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL))
      })
    }
    
    # Function to extract detailed classification
    extract_classification <- function(feature, category_config) {
      primary_key <- category_config$key
      subcategory_tags <- category_config$subcategory_tags
      
      # Get primary classification
      primary_value <- if (primary_key %in% names(feature) && !is.na(feature[[primary_key]])) {
        feature[[primary_key]]
      } else {
        "unspecified"
      }
      
      # Get subcategory details
      subcategory_parts <- c()
      if (!is.null(subcategory_tags)) {
        for (tag in subcategory_tags) {
          if (tag %in% names(feature) && !is.na(feature[[tag]]) && feature[[tag]] != "") {
            subcategory_parts <- c(subcategory_parts, paste0(tag, "=", feature[[tag]]))
          }
        }
      }
      
      subcategory <- if (length(subcategory_parts) > 0) {
        paste(subcategory_parts, collapse = "; ")
      } else {
        primary_value
      }
      
      detailed_classification <- paste(category_config$name, "-", primary_value)
      if (length(subcategory_parts) > 0) {
        detailed_classification <- paste0(detailed_classification, " (", 
                                          paste(subcategory_parts, collapse = "; "), ")")
      }
      
      return(list(
        category = category_config$name,
        subcategory = primary_value,
        detailed = detailed_classification
      ))
    }
    
    # Function to rasterize features with appropriate geometry handling
    rasterize_feature_layer <- function(features, template, buffer_utm, category_config, geom_type) {
      if (is.null(features) || nrow(features) == 0) return(NULL)
      
      # Transform to UTM
      features_utm <- st_transform(features, crs = st_crs(template))
      
      # Clip to buffer
      features_clipped <- tryCatch({
        st_intersection(features_utm, buffer_utm)
      }, error = function(e) {
        st_crop(features_utm, buffer_utm)
      })
      
      if (nrow(features_clipped) == 0) return(NULL)
      
      # Apply geometry-specific buffering
      buffer_dist <- category_config$geometry_buffer[[gsub("osm_", "", geom_type)]]
      
      if (geom_type == "osm_polygons") {
        # Polygons - use as is
        features_for_raster <- features_clipped
      } else if (geom_type == "osm_lines") {
        # Lines - apply line buffer
        features_for_raster <- st_buffer(features_clipped, dist = buffer_dist)
      } else { # osm_points
        # Points - apply point buffer  
        features_for_raster <- st_buffer(features_clipped, dist = buffer_dist)
      }
      
      # Extract classifications for each feature
      classifications <- list()
      for (i in 1:nrow(features_clipped)) {
        classifications[[i]] <- extract_classification(features_clipped[i, ], category_config)
      }
      
      # Create separate rasters for each unique classification
      unique_classifications <- unique(sapply(classifications, function(x) x$detailed))
      
      results <- list()
      for (classification in unique_classifications) {
        # Find features with this classification
        matching_indices <- which(sapply(classifications, function(x) x$detailed == classification))
        matching_features <- features_for_raster[matching_indices, ]
        
        # Rasterize
        feature_raster <- rasterize(vect(matching_features), template, field = 1)
        
        if (!is.null(feature_raster)) {
          # Get the classification details
          class_details <- classifications[[matching_indices[1]]]
          
          results[[classification]] <- list(
            raster = feature_raster,
            category = class_details$category,
            subcategory = class_details$subcategory,
            detailed = class_details$detailed,
            priority = category_config$priority
          )
        }
      }
      
      return(results)
    }
    
    # Process each feature category in priority order
    all_classification_rasters <- list()
    
    for (category_name in names(feature_categories)) {
      category_config <- feature_categories[[category_name]]
      
      if (verbose) cat("Processing:", category_config$name, "(priority:", category_config$priority, ")...\n")
      
      # Query OSM data
      osm_data <- safe_osm_query(bbox_wgs84, category_config$key, category_config$values)
      
      # Process each geometry type
      for (geom_type in c("osm_polygons", "osm_lines", "osm_points")) {
        if (!is.null(osm_data[[geom_type]]) && nrow(osm_data[[geom_type]]) > 0) {
          
          if (verbose) cat("  Processing", nrow(osm_data[[geom_type]]), geom_type, "...\n")
          
          # Rasterize with detailed classifications
          raster_results <- rasterize_feature_layer(
            osm_data[[geom_type]], template_raster, buffer_utm, category_config, geom_type
          )
          
          if (!is.null(raster_results)) {
            for (classification in names(raster_results)) {
              all_classification_rasters[[classification]] <- raster_results[[classification]]
            }
          }
        }
      }
      
      Sys.sleep(0.5)
    }
    
    # Assign pixels to classifications based on priority
    if (verbose) cat("\nAssigning pixels to classifications based on priority...\n")
    
    # Sort by priority (highest first)
    sorted_classifications <- all_classification_rasters[
      order(sapply(all_classification_rasters, function(x) x$priority), decreasing = TRUE)
    ]
    
    for (classification_name in names(sorted_classifications)) {
      class_info <- sorted_classifications[[classification_name]]
      
      # Add to lookup table
      classification_lookup <- rbind(classification_lookup, data.frame(
        ID = next_id,
        Category = class_info$category,
        Subcategory = class_info$subcategory,
        Detailed_Classification = class_info$detailed,
        Priority = class_info$priority,
        stringsAsFactors = FALSE
      ))
      
      # Mask to buffer and assign to unassigned pixels
      masked_raster <- mask(class_info$raster, buffer_mask)
      
      # Only assign to pixels that haven't been classified yet
      available_pixels <- is.na(pixel_classification) & !is.na(masked_raster)
      pixel_classification[available_pixels] <- next_id
      
      next_id <- next_id + 1
    }
    
    # Calculate pixel counts and percentages
    if (verbose) cat("Calculating pixel statistics...\n")
    
    pixel_counts <- table(values(pixel_classification), useNA = "no")
    
    pixel_summary <- data.frame(
      Classification_ID = as.integer(names(pixel_counts)),
      Pixel_Count = as.integer(pixel_counts),
      stringsAsFactors = FALSE
    ) %>%
      left_join(classification_lookup, by = c("Classification_ID" = "ID")) %>%
      mutate(
        Percentage = round((Pixel_Count / total_pixels) * 100, 3)
      ) %>%
      arrange(desc(Percentage))
    
    # Create category-level summary
    category_summary <- pixel_summary %>%
      group_by(Category) %>%
      summarise(
        Total_Pixels = sum(Pixel_Count),
        Total_Percentage = round(sum(Percentage), 2),
        Unique_Subcategories = n_distinct(Subcategory),
        Max_Subcategory_Percentage = round(max(Percentage), 2),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Percentage))
    
    # Create subcategory summary
    subcategory_summary <- pixel_summary %>%
      group_by(Category, Subcategory) %>%
      summarise(
        Total_Pixels = sum(Pixel_Count),
        Total_Percentage = round(sum(Percentage), 2),
        Detailed_Classifications = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Percentage))
    
    # Calculate coverage statistics
    classified_pixels <- sum(pixel_summary$Pixel_Count)
    unclassified_pixels <- total_pixels - classified_pixels
    coverage_percentage <- (classified_pixels / total_pixels) * 100
    
    if (verbose) {
      cat("\n=== PIXEL CLASSIFICATION ANALYSIS COMPLETE ===\n")
      cat("Total pixels:", total_pixels, "\n")
      cat("Classified pixels:", classified_pixels, "\n")
      cat("Unclassified pixels:", unclassified_pixels, "\n")
      cat("Coverage percentage:", round(coverage_percentage, 2), "%\n")
      cat("Unique classifications found:", nrow(pixel_summary), "\n\n")
      
      cat("Top 10 classifications by pixel coverage:\n")
      print(head(pixel_summary[c("Category", "Subcategory", "Percentage", "Detailed_Classification")], 10))
    }
    
    return(list(
      pixel_classification_raster = pixel_classification,
      buffer_mask = buffer_mask,
      template_raster = template_raster,
      
      # Summary tables
      detailed_pixel_summary = pixel_summary,
      category_summary = category_summary,
      subcategory_summary = subcategory_summary,
      classification_lookup = classification_lookup,
      
      # Statistics
      total_pixels = total_pixels,
      classified_pixels = classified_pixels,
      unclassified_pixels = unclassified_pixels,
      coverage_percentage = coverage_percentage,
      pixel_resolution = pixel_resolution,
      buffer_area_m2 = as.numeric(st_area(buffer_utm)),
      
      # Raw data
      all_classification_rasters = all_classification_rasters
    ))
  }
  
  
  
  
  
  
  
  print("===============================Reading Edges for East====================================================")
  east_result <- analyze_buffer_pixels_classification(east_zone_sf)
  
  print("===============================Reading Edges for West====================================================")
 west_result <- analyze_buffer_pixels_classification(west_zone_sf)
  
  print("===============================Reading Edges for North====================================================")
  north_result <- analyze_buffer_pixels_classification(north_zone_sf)
  
  print("===============================Reading Edges for South====================================================")
  south_result <- analyze_buffer_pixels_classification(south_zone_sf)
  
  
  print("==============================Results ====================================================================")
  print("East")
  print(east_result$subcategory_summary$Category[1])
  print(east_result$subcategory_summary$Subcategory[1])
  print(east_result$subcategory_summary$Total_Percentage[1])
  print("============================================================================================================")
  print("West")
  print(west_result$subcategory_summary$Category[1])
  print(west_result$subcategory_summary$Subcategory[1])
  print(west_result$subcategory_summary$Total_Percentage[1])
  print("============================================================================================================")
  print("North")
  
  print(north_result$subcategory_summary$Category[1])
  print(north_result$subcategory_summary$Subcategory[1])
  print(north_result$subcategory_summary$Total_Percentage[1])
  print("============================================================================================================")
  print("South")
  print(south_result$subcategory_summary$Category[1])
  print(south_result$subcategory_summary$Subcategory[1])
  print(south_result$subcategory_summary$Total_Percentage[1])
  
  
  resultToSave <- data.frame(
    FieldName = field_name,
    Crop = crop_name,
    Year = year,
    # North_Category = north_result$subcategory_summary$Category[1] || "NA",
    # North_SubCategory = north_result$subcategory_summary$Subcategory[1] || "NA",
    # North_Percentage = north_result$subcategory_summary$Total_Percentage[1] || "NA",
    # East_Category = east_result$subcategory_summary$Category[1] || "NA",
    # East_SubCategory = east_result$subcategory_summary$Subcategory[1] || "NA",
    # East_Percentage = east_result$subcategory_summary$Total_Percentage[1] || "NA",
    # South_Category = south_result$subcategory_summary$Category[1] || "NA",
    # South_SubCategory = south_result$subcategory_summary$Subcategory[1] || "NA",
    # South_Percentage = south_result$subcategory_summary$Total_Percentage[1] || "NA",
    # West_Category = west_result$subcategory_summary$Category[1] || "NA",
    # West_SubCategory = west_result$subcategory_summary$Subcategory[1] || "NA",
    # West_Percentage = west_result$subcategory_summary$Total_Percentage[1] || "NA",
    
    North_Category = ifelse(is.null(north_result$subcategory_summary$Category[1]) || length(north_result$subcategory_summary$Category[1]) == 0, "NA", north_result$subcategory_summary$Category[1]),
    North_SubCategory = ifelse(is.null(north_result$subcategory_summary$Subcategory[1]) || length(north_result$subcategory_summary$Subcategory[1]) == 0, "NA", north_result$subcategory_summary$Subcategory[1]),
    North_Percentage = ifelse(is.null(north_result$subcategory_summary$Total_Percentage[1]) || length(north_result$subcategory_summary$Total_Percentage[1]) == 0, "NA", north_result$subcategory_summary$Total_Percentage[1]),
    East_Category = ifelse(is.null(east_result$subcategory_summary$Category[1]) || length(east_result$subcategory_summary$Category[1]) == 0, "NA", east_result$subcategory_summary$Category[1]),
    East_SubCategory = ifelse(is.null(east_result$subcategory_summary$Subcategory[1]) || length(east_result$subcategory_summary$Subcategory[1]) == 0, "NA", east_result$subcategory_summary$Subcategory[1]),
    East_Percentage = ifelse(is.null( east_result$subcategory_summary$Total_Percentage[1]) || length( east_result$subcategory_summary$Total_Percentage[1]) == 0, "NA", east_result$subcategory_summary$Total_Percentage[1]),
    South_Category = ifelse(is.null(south_result$subcategory_summary$Category[1]) || length(south_result$subcategory_summary$Category[1]) == 0, "NA", south_result$subcategory_summary$Category[1]),
    South_SubCategory = ifelse(is.null(south_result$subcategory_summary$Subcategory[1]) || length(south_result$subcategory_summary$Subcategory[1]) == 0, "NA", south_result$subcategory_summary$Subcategory[1]),
    South_Percentage = ifelse(is.null(south_result$subcategory_summary$Total_Percentage[1]) || length(south_result$subcategory_summary$Total_Percentage[1]) == 0, "NA", south_result$subcategory_summary$Total_Percentage[1]),
    West_Category = ifelse(is.null(west_result$subcategory_summary$Category[1] ) || length(west_result$subcategory_summary$Category[1] ) == 0, "NA", west_result$subcategory_summary$Category[1]),
    West_SubCategory = ifelse(is.null(west_result$subcategory_summary$Subcategory[1]) || length(west_result$subcategory_summary$Subcategory[1]) == 0, "NA", west_result$subcategory_summary$Subcategory[1]),
    West_Percentage = ifelse(is.null(west_result$subcategory_summary$Total_Percentage[1]) || length(west_result$subcategory_summary$Total_Percentage[1]) == 0, "NA", west_result$subcategory_summary$Total_Percentage[1])
    
  )
  print(resultToSave)
  
  return (resultToSave)
  
}

west_zone <- get_west_edge_buffer_zone("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Data_Raw/v1.1/2014_farmcrew_n27_v1.1/USDA_Central Farm_1-20B_2014_SOYBEANS_1/CentralFarm_1-20B_2014_SOYB_sph_proc_field.shp")
plot(st_geometry(west_zone), col = 'lightblue', main = "West Edge + 50m Buffer Zone")




# Example usage:
east_zone <- get_east_edge_buffer_zone("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Data_Raw/v1.1/2014_farmcrew_n27_v1.1/USDA_Central Farm_1-20B_2014_SOYBEANS_1/CentralFarm_1-20B_2014_SOYB_sph_proc_field.shp")

# Plot the eastern buffer zone
plot(st_geometry(east_zone), col = 'lightcoral', main = "East Edge + 50m Buffer Zone")

plot(st_geometry("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Data_Raw/v1.1/2014_farmcrew_n27_v1.1/USDA_Central Farm_1-20B_2014_SOYBEANS_1/CentralFarm_1-20B_2014_SOYB_sph_proc_field.shp"))


west_zone_sf <- st_transform(west_zone_sf, crs = 4326)

# Load the osmdata library
library(osmdata)

# Get the bounding box of your buffer zone
bbox <- st_bbox(south_zone_sf)
print(bbox)


bbox_polygon <- st_as_sfc(bbox, crs = st_crs(west_zone_sf))






analyze_buffer_zone_features <- function(buffer_zone_sf, verbose = TRUE) {
  library(osmdata)
  library(sf)
  library(dplyr)
  
  # Convert to WGS84 for OSM queries
  buffer_wgs84 <- st_transform(buffer_zone_sf, crs = 4326)
  bbox_wgs84 <- st_bbox(buffer_wgs84)
  
  if (verbose) cat("Querying OpenStreetMap data...\n")
  
  # Initialize results
  inventory <- list()
  summary_table <- data.frame(
    Feature_Type = character(),
    Subtype = character(),
    Count = integer(),
    Details = character(),
    stringsAsFactors = FALSE
  )
  
  # Define feature categories to query
  feature_queries <- list(
    # Transportation
    highways = list(key = "highway", name = "Roads & Paths"),
    railways = list(key = "railway", name = "Railways"),
    
    # Buildings & Infrastructure
    buildings = list(key = "building", name = "Buildings"),
    amenities = list(key = "amenity", name = "Amenities"),
    
    # Land Use
    landuse = list(key = "landuse", name = "Land Use"),
    natural = list(key = "natural", name = "Natural Features"),
    
    # Water features
    waterway = list(key = "waterway", name = "Waterways"),
    
    # Agriculture & Rural
    agriculture = list(key = "agricultural", name = "Agricultural"),
    
    # Utilities
    power = list(key = "power", name = "Power Infrastructure"),
    telecommunications = list(key = "telecom", name = "Telecommunications"),
    
    # Leisure & Tourism
    leisure = list(key = "leisure", name = "Leisure"),
    tourism = list(key = "tourism", name = "Tourism")
  )
  
  # Helper function to safely query OSM
  safe_osm_query <- function(bbox, key, value = NULL) {
    tryCatch({
      if (is.null(value)) {
        result <- opq(bbox) %>%
          add_osm_feature(key = key) %>%
          osmdata_sf()
      } else {
        result <- opq(bbox) %>%
          add_osm_feature(key = key, value = value) %>%
          osmdata_sf()
      }
      return(result)
    }, error = function(e) {
      if (verbose) cat("  Warning: Could not query", key, "\n")
      return(list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL))
    })
  }
  
  # Helper function to filter features within buffer
  filter_within_buffer <- function(osm_features, buffer_zone) {
    if (is.null(osm_features) || nrow(osm_features) == 0) return(NULL)
    
    # Transform to same CRS as buffer
    osm_features <- st_transform(osm_features, crs = st_crs(buffer_zone))
    
    # Find features that intersect with buffer
    intersecting <- st_intersects(osm_features, buffer_zone, sparse = FALSE)
    
    if (any(intersecting)) {
      return(osm_features[intersecting[,1], ])
    } else {
      return(NULL)
    }
  }
  
  # Query each feature type
  for (feature_name in names(feature_queries)) {
    if (verbose) cat("Querying:", feature_queries[[feature_name]]$name, "...\n")
    
    # Get OSM data
    osm_data <- safe_osm_query(bbox_wgs84, feature_queries[[feature_name]]$key)
    
    # Process each geometry type
    for (geom_type in c("osm_points", "osm_lines", "osm_polygons")) {
      if (!is.null(osm_data[[geom_type]])) {
        # Filter to features actually within buffer
        features_in_buffer <- filter_within_buffer(osm_data[[geom_type]], buffer_wgs84)
        
        if (!is.null(features_in_buffer) && nrow(features_in_buffer) > 0) {
          # Store the data
          inventory[[paste0(feature_name, "_", geom_type)]] <- features_in_buffer
          
          # Analyze subtypes
          subtype_col <- feature_queries[[feature_name]]$key
          if (subtype_col %in% names(features_in_buffer)) {
            subtype_counts <- table(features_in_buffer[[subtype_col]], useNA = "ifany")
            
            for (subtype in names(subtype_counts)) {
              summary_table <- dplyr::bind_rows(summary_table, data.frame(
                Feature_Type = feature_queries[[feature_name]]$name,
                Subtype = ifelse(is.na(subtype) || subtype == "", "Unspecified", subtype),
                Count = as.integer(subtype_counts[subtype]),
                Details = paste(geom_type, "geometry"),
                stringsAsFactors = FALSE
              ))
            }
          } else {
            # No subtype information available
            summary_table <- dplyr::bind_rows(summary_table, data.frame(
              Feature_Type = feature_queries[[feature_name]]$name,
              Subtype = "All types",
              Count = nrow(features_in_buffer),
              Details = paste(geom_type, "geometry"),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
    
    # Add small delay to be nice to OSM servers
    Sys.sleep(0.5)
  }
  
  # Clean up and organize summary
  if (nrow(summary_table) > 0) {
    summary_table <- summary_table %>%
      arrange(Feature_Type, desc(Count)) %>%
      mutate(
        Feature_Type = factor(Feature_Type),
        Total_by_Type = ave(Count, Feature_Type, FUN = sum)
      )
  }
  
  # Create high-level summary
  type_summary <- summary_table %>%
    group_by(Feature_Type) %>%
    summarise(
      Total_Features = sum(Count),
      Unique_Subtypes = n_distinct(Subtype),
      .groups = 'drop'
    ) %>%
    arrange(desc(Total_Features))
  
  if (verbose) {
    cat("\n=== BUFFER ZONE INVENTORY COMPLETE ===\n")
    cat("Buffer zone area:", round(as.numeric(st_area(buffer_wgs84)), 2), "square meters\n")
    cat("Total feature categories found:", nrow(type_summary), "\n")
    cat("Total individual features:", sum(type_summary$Total_Features), "\n\n")
  }
  
  return(list(
    detailed_inventory = inventory,
    detailed_summary = summary_table,
    type_summary = type_summary,
    buffer_area_m2 = as.numeric(st_area(buffer_wgs84)),
    bbox_used = bbox_wgs84
  ))
}

# Convenience function to display results nicely
display_inventory_results <- function(results) {
  cat("=== BUFFER ZONE FEATURE INVENTORY ===\n\n")
  
  # High-level summary
  cat("HIGH-LEVEL SUMMARY:\n")
  cat("Buffer area:", round(results$buffer_area_m2, 2), "square meters\n")
  cat("Total features found:", sum(results$type_summary$Total_Features), "\n\n")
  
  # By category
  cat("BY CATEGORY:\n")
  print(results$type_summary)
  
  cat("\nDETAILED BREAKDOWN:\n")
  # Show top features
  top_features <- head(results$detailed_summary, 20)
  print(top_features)
  
  if (nrow(results$detailed_summary) > 20) {
    cat("\n... and", nrow(results$detailed_summary) - 20, "more feature types\n")
  }
  
  cat("\nTo access individual features, use: results$detailed_inventory\n")
  cat("Available datasets:", names(results$detailed_inventory), "\n")
}





analyze_buffer_zone_coverage <- function(buffer_zone_sf, pixel_size = 10, verbose = TRUE) {
  library(osmdata)
  library(sf)
  library(dplyr)
  library(raster)
  library(stars)
  
  # Convert to projected CRS for accurate area calculations
  # Use UTM or appropriate local projection
  buffer_proj <- st_transform(buffer_zone_sf, crs = 3857) # Web Mercator for general use
  
  # Convert to WGS84 for OSM queries
  buffer_wgs84 <- st_transform(buffer_zone_sf, crs = 4326)
  bbox_wgs84 <- st_bbox(buffer_wgs84)
  
  if (verbose) cat("Creating raster grid with", pixel_size, "meter pixels...\n")
  
  # Create raster template
  buffer_extent <- st_bbox(buffer_proj)
  ncols <- ceiling((buffer_extent$xmax - buffer_extent$xmin) / pixel_size)
  nrows <- ceiling((buffer_extent$ymax - buffer_extent$ymin) / pixel_size)
  
  # Create base raster
  base_raster <- raster(
    extent(buffer_extent$xmin, buffer_extent$xmax, 
           buffer_extent$ymin, buffer_extent$ymax),
    ncols = ncols, nrows = nrows,
    crs = st_crs(buffer_proj)$wkt
  )
  
  # Create buffer mask
  buffer_raster <- rasterize(buffer_proj, base_raster, field = 1)
  total_pixels <- cellStats(buffer_raster, sum, na.rm = TRUE)
  
  if (verbose) cat("Buffer zone contains", total_pixels, "pixels\n")
  if (verbose) cat("Querying OpenStreetMap data...\n")
  
  # Initialize results
  coverage_results <- data.frame(
    Feature_Type = character(),
    Subtype = character(),
    Pixels_Covered = integer(),
    Percentage_Coverage = numeric(),
    Area_m2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Define feature categories with priority order (higher priority overwrites lower)
  feature_queries <- list(
    # Lower priority (background features)
    natural = list(key = "natural", name = "Natural Features", priority = 1),
    landuse = list(key = "landuse", name = "Land Use", priority = 2),
    leisure = list(key = "leisure", name = "Leisure", priority = 3),
    
    # Medium priority
    waterway = list(key = "waterway", name = "Waterways", priority = 4),
    agriculture = list(key = "agricultural", name = "Agricultural", priority = 4),
    
    # Higher priority (infrastructure)
    buildings = list(key = "building", name = "Buildings", priority = 5),
    amenities = list(key = "amenity", name = "Amenities", priority = 5),
    
    # Highest priority (transportation)
    railways = list(key = "railway", name = "Railways", priority = 6),
    highways = list(key = "highway", name = "Roads & Paths", priority = 7),
    
    # Utilities (medium-high priority)
    power = list(key = "power", name = "Power Infrastructure", priority = 5),
    telecommunications = list(key = "telecom", name = "Telecommunications", priority = 5),
    tourism = list(key = "tourism", name = "Tourism", priority = 3)
  )
  
  # Helper function to safely query OSM
  safe_osm_query <- function(bbox, key, value = NULL) {
    tryCatch({
      if (is.null(value)) {
        result <- opq(bbox) %>%
          add_osm_feature(key = key) %>%
          osmdata_sf()
      } else {
        result <- opq(bbox) %>%
          add_osm_feature(key = key, value = value) %>%
          osmdata_sf()
      }
      print(paste(key, "=============================="))
      print(result)
      return(result)
    }, error = function(e) {
      if (verbose) cat("  Warning: Could not query", key, "\n")
      return(list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL))
    })
  }
  
  # Helper function to filter and transform features
  prepare_features <- function(osm_features, buffer_zone) {
    if (is.null(osm_features) || nrow(osm_features) == 0) return(NULL)
    
    # Transform to same CRS as buffer
    osm_features <- st_transform(osm_features, crs = st_crs(buffer_zone))
    
    # Find features that intersect with buffer
    intersecting <- st_intersects(osm_features, buffer_zone, sparse = FALSE)
    
    if (any(intersecting)) {
      features_in_buffer <- osm_features[intersecting[,1], ]
      # Clip to buffer boundary
      return(st_intersection(features_in_buffer, buffer_zone))
    } else {
      return(NULL)
    }
  }
  
  # Create master raster to track coverage with priorities
  coverage_raster <- base_raster
  values(coverage_raster) <- 0
  priority_raster <- base_raster
  values(priority_raster) <- 0
  
  # Store all features for final analysis
  all_features <- list()
  
  # Query each feature type
  for (feature_name in names(feature_queries)) {
    if (verbose) cat("Processing:", feature_queries[[feature_name]]$name, "...\n")
    
    # Get OSM data
    osm_data <- safe_osm_query(bbox_wgs84, feature_queries[[feature_name]]$key)
    
    # Combine all geometries for this feature type
    combined_features <- NULL
    
    # Process polygons first (most accurate for area)
    if (!is.null(osm_data$osm_polygons)) {
      polys <- prepare_features(osm_data$osm_polygons, buffer_proj)
      if (!is.null(polys)) {
        combined_features <- polys
      }
    }
    
    # Process lines (with buffer for roads, etc.)
    if (!is.null(osm_data$osm_lines)) {
      lines <- prepare_features(osm_data$osm_lines, buffer_proj)
      if (!is.null(lines)) {
        # Buffer lines to give them area (road width estimation)
        line_width <- case_when(
          feature_name == "highways" ~ 8, # meters
          feature_name == "railways" ~ 10,
          feature_name == "waterway" ~ 5,
          feature_name == "power" ~ 20,
          TRUE ~ 3
        )
        lines_buffered <- st_buffer(lines, dist = line_width)
        
        if (is.null(combined_features)) {
          combined_features <- lines_buffered
        } else {
          lines_buffered <- st_sf(geometry = st_geometry(lines_buffered), crs = st_crs(lines_buffered)) #
          combined_features <- dplyr::bind_rows(combined_features, lines_buffered)
        }
      }
    }
    
    # Process points (with small buffer)
    if (!is.null(osm_data$osm_points)) {
      points <- prepare_features(osm_data$osm_points, buffer_proj)
      if (!is.null(points)) {
        # Buffer points to give them area
        point_buffer <- case_when(
          feature_name == "buildings" ~ 50, # meters
          feature_name == "amenities" ~ 30,
          feature_name == "power" ~ 15,
          TRUE ~ 10
        )
        points_buffered <- st_buffer(points, dist = point_buffer)
        
        if (is.null(combined_features)) {
          combined_features <- points_buffered
        } else {
          points_buffered <- st_sf(geometry = st_geometry(points_buffered), crs = st_crs(points_buffered)) #
          combined_features <- dplyr::bind_rows(combined_features, points_buffered)
        }
      }
    }
    
    # Rasterize features if any were found
    if (!is.null(combined_features) && nrow(combined_features) > 0) {
      all_features[[feature_name]] <- combined_features
      
      # Create raster for this feature type
      feature_raster <- rasterize(combined_features, base_raster, field = 1, fun = 'first')
      
      # Apply priority system - only update pixels where this feature has higher priority
      current_priority <- feature_queries[[feature_name]]$priority
      
      # Find pixels where this feature should be applied
      update_mask <- !is.na(feature_raster) & 
        (is.na(priority_raster) | priority_raster < current_priority)
      
      # Update coverage and priority rasters
      coverage_raster[update_mask] <- match(feature_name, names(feature_queries))
      priority_raster[update_mask] <- current_priority
      
      # Calculate coverage by subtype
      subtype_col <- feature_queries[[feature_name]]$key
      if (subtype_col %in% names(combined_features)) {
        subtypes <- unique(combined_features[[subtype_col]])
        subtypes <- subtypes[!is.na(subtypes)]
        
        for (subtype in subtypes) {
          subtype_features <- combined_features[combined_features[[subtype_col]] == subtype, ]
          subtype_raster <- rasterize(subtype_features, base_raster, field = 1)
          subtype_raster[is.na(subtype_raster)] <- 0
          subtype_raster <- subtype_raster * buffer_raster # Apply buffer mask
          
          pixels_covered <- cellStats(subtype_raster, sum, na.rm = TRUE)
          percentage <- (pixels_covered / total_pixels) * 100
          area_m2 <- pixels_covered * (pixel_size^2)
          
          coverage_results <- dplyr::bind_rows(coverage_results, data.frame(
            Feature_Type = feature_queries[[feature_name]]$name,
            Subtype = subtype,
            Pixels_Covered = pixels_covered,
            Percentage_Coverage = percentage,
            Area_m2 = area_m2,
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # No subtype information
        feature_raster[is.na(feature_raster)] <- 0
        feature_raster <- feature_raster * buffer_raster # Apply buffer mask
        
        pixels_covered <- cellStats(feature_raster, sum, na.rm = TRUE)
        percentage <- (pixels_covered / total_pixels) * 100
        area_m2 <- pixels_covered * (pixel_size^2)
        
        coverage_results <- dplyr::bind_rows(coverage_results, data.frame(
          Feature_Type = feature_queries[[feature_name]]$name,
          Subtype = "All types",
          Pixels_Covered = pixels_covered,
          Percentage_Coverage = percentage,
          Area_m2 = area_m2,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Add delay to be nice to OSM servers
    Sys.sleep(0.5)
  }
  
  # Calculate uncovered area
  coverage_raster[is.na(coverage_raster)] <- 0
  coverage_raster <- coverage_raster * buffer_raster # Apply buffer mask
  covered_pixels <- cellStats(coverage_raster > 0, sum, na.rm = TRUE)
  uncovered_pixels <- total_pixels - covered_pixels
  uncovered_percentage <- (uncovered_pixels / total_pixels) * 100
  uncovered_area <- uncovered_pixels * (pixel_size^2)
  
  # Add uncovered area to results
  coverage_results <- dplyr::bind_rows(coverage_results, data.frame(
    Feature_Type = "Uncovered/Unknown",
    Subtype = "Empty space",
    Pixels_Covered = uncovered_pixels,
    Percentage_Coverage = uncovered_percentage,
    Area_m2 = uncovered_area,
    stringsAsFactors = FALSE
  ))
  
  # Create summary by feature type
  type_summary <- coverage_results %>%
    group_by(Feature_Type) %>%
    summarise(
      Total_Pixels = sum(Pixels_Covered),
      Total_Percentage = sum(Percentage_Coverage),
      Total_Area_m2 = sum(Area_m2),
      Unique_Subtypes = n_distinct(Subtype),
      .groups = 'drop'
    ) %>%
    arrange(desc(Total_Percentage))
  
  if (verbose) {
    cat("\n=== PIXEL COVERAGE ANALYSIS COMPLETE ===\n")
    cat("Buffer zone area:", round(as.numeric(st_area(buffer_proj)), 2), "square meters\n")
    cat("Total pixels analyzed:", total_pixels, "\n")
    cat("Pixel size:", pixel_size, "meters\n")
    cat("Covered pixels:", covered_pixels, "(", round((covered_pixels/total_pixels)*100, 1), "%)\n")
    cat("Uncovered pixels:", uncovered_pixels, "(", round(uncovered_percentage, 1), "%)\n\n")
  }
  
  return(list(
    pixel_coverage = coverage_results,
    type_summary = type_summary,
    coverage_raster = coverage_raster,
    buffer_raster = buffer_raster,
    all_features = all_features,
    total_pixels = total_pixels,
    pixel_size = pixel_size,
    buffer_area_m2 = as.numeric(st_area(buffer_proj))
  ))
}

# Function to display coverage results
display_coverage_results <- function(results) {
  cat("=== BUFFER ZONE PIXEL COVERAGE ANALYSIS ===\n\n")
  
  # High-level summary
  cat("COVERAGE SUMMARY:\n")
  cat("Buffer area:", round(results$buffer_area_m2, 2), "square meters\n")
  cat("Total pixels:", results$total_pixels, "\n")
  cat("Pixel size:", results$pixel_size, "meters\n\n")
  
  # By category
  cat("COVERAGE BY FEATURE TYPE:\n")
  print(results$type_summary)
  
  cat("\nDETAILED BREAKDOWN (Top 20):\n")
  # Show top features by coverage
  top_features <- head(arrange(results$pixel_coverage, desc(Percentage_Coverage)), 20)
  print(top_features[, c("Feature_Type", "Subtype", "Percentage_Coverage", "Area_m2")])
  
  # Summary statistics
  cat("\nSUMMARY STATISTICS:\n")
  cat("Most dominant feature:", 
      results$type_summary$Feature_Type[1], 
      "(", round(results$type_summary$Total_Percentage[1], 1), "%)\n")
  
  total_covered <- sum(results$type_summary$Total_Percentage[results$type_summary$Feature_Type != "Uncovered/Unknown"])
  cat("Total area with identified features:", round(total_covered, 1), "%\n")
  
  cat("\nTo visualize results, use the coverage_raster in the results\n")
}

# Function to create coverage visualization
plot_coverage_map <- function(results, feature_colors = NULL) {
  library(rasterVis)
  library(RColorBrewer)
  
  # Create color palette
  n_features <- length(unique(results$pixel_coverage$Feature_Type))
  if (is.null(feature_colors)) {
    colors <- rainbow(n_features, alpha = 0.7)
  } else {
    colors <- feature_colors
  }
  
  # Plot the coverage raster
  plot(results$coverage_raster, 
       main = "Buffer Zone Feature Coverage",
       col = colors,
       legend = TRUE)
  
  # Add buffer boundary
  plot(st_geometry(st_as_sf(results$buffer_raster, merge = TRUE)), 
       add = TRUE, border = "black", lwd = 2, fill = FALSE)
}





# Create interactive leaflet map function






# Function to get specific category breakdown
get_category_breakdown <- function(analysis_result, category_name) {
  library(dplyr)
  
  breakdown <- analysis_result$detailed_pixel_summary %>%
    filter(Category == category_name) %>%
    select(Subcategory, Detailed_Classification, Percentage, Pixel_Count) %>%
    arrange(desc(Percentage))
  
  if (nrow(breakdown) == 0) {
    cat("No data found for category:", category_name, "\n")
    cat("Available categories:", paste(unique(analysis_result$detailed_pixel_summary$Category), collapse = ", "), "\n")
    return(NULL)
  }
  
  total_category_percentage <- sum(breakdown$Percentage)
  
  cat("=== BREAKDOWN FOR", toupper(category_name), "===\n")
  cat("Total coverage:", total_category_percentage, "% of buffer\n")
  cat("Number of subcategories:", nrow(breakdown), "\n\n")
  
  print(breakdown)
  return(breakdown)
}

# Function to visualize pixel classification
plot_pixel_classification <- function(analysis_result, top_n = 15) {
  library(ggplot2)
  library(dplyr)
  
  if (nrow(analysis_result$detailed_pixel_summary) == 0) {
    cat("No classifications to plot\n")
    return(NULL)
  }
  
  # Create a readable label
  plot_data <- head(analysis_result$detailed_pixel_summary, top_n) %>%
    mutate(
      Label = paste0(Category, " - ", Subcategory),
      Label = ifelse(nchar(Label) > 40, paste0(substr(Label, 1, 37), "..."), Label)
    )
  
  ggplot(plot_data, aes(x = reorder(Label, Percentage), y = Percentage, fill = Category)) +
    geom_col(alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Classifications by Pixel Coverage"),
      subtitle = paste("Total classified:", round(analysis_result$coverage_percentage, 1), 
                       "% of", analysis_result$total_pixels, "pixels"),
      x = "Classification",
      y = "Percentage of Buffer Zone (%)",
      fill = "Category"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))
}

# Function to export results summary
export_classification_summary <- function(analysis_result, filename = "buffer_classification_summary.csv") {
  
  summary_export <- analysis_result$detailed_pixel_summary %>%
    select(Category, Subcategory, Detailed_Classification, Pixel_Count, Percentage) %>%
    arrange(desc(Percentage))
  
  write.csv(summary_export, filename, row.names = FALSE)
  cat("Classification summary exported to:", filename, "\n")
  
  # Also create a category summary
  category_file <- gsub(".csv", "_by_category.csv", filename)
  write.csv(analysis_result$category_summary, category_file, row.names = FALSE)
  cat("Category summary exported to:", category_file, "\n")
  
  return(list(
    detailed_file = filename,
    category_file = category_file
  ))
}