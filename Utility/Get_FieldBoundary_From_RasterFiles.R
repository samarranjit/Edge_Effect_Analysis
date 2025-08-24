# -------------------------------------------------------------
# File: get_field_boundary.R
# Purpose: Extracts the field boundary from a yield raster by
#          clustering yield points and generating concave hulls
#          for each cluster.
#
# Description:
#   - Converts raster yield data into spatial points
#   - Groups points into spatial clusters using hierarchical clustering
#   - Generates concave polygons (using concaveman) for each cluster
#   - Combines them into a final field boundary polygon (sf object)
#   - Falls back to single concave hull if clustering fails
#
# Inputs:
#   - raster_path: File path to yield raster (.tif)
#
# Outputs:
#   - A spatial feature (sf) representing the field boundary
#
# Dependencies:
#   - terra, sf, concaveman, stats
#
# Author: Samar Ranjit
# Date: 2025-05-26
# -------------------------------------------------------------


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
  sf_points <- st_as_sf(yield_df,
                        coords = c("x", "y"),
                        crs = st_crs(original_crs))
  
  # Group points into clusters: This step is crucial - we'll first group nearby points
  message("Clustering points...")
  # Convert to a matrix for clustering
  point_coords <- st_coordinates(sf_points)
  
  cluster_distance <- resolution * 2
  
  # Use hierarchical clustering
  # Cut the dendrogram to create clusters
  # This height determines how far apart points can be to be in the same cluster
  clusters <- hclust(dist(point_coords), method = "single")
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
                                       length_threshold = resolution / 2)
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
    field_boundary <- concaveman(sf_points,
                                 concavity = concavity,
                                 length_threshold = resolution / 2)
  }
  
  # plot(field_boundary) #If you want to plot the Field Boundary
  return(field_boundary)
}