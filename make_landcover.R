# Load required libraries
library(raster)
library(sf)

make_landcover <- function(site,state){
  
  if (state == 0){source <- "original_"} else {source <-""}
  
  # Set the file paths
  raster_file <- paste0(local_path,site,"/from_lidar/dsm.tif")
  shapefile_path <- paste0(local_path,site,"/from_siteplan/",source,"landcover.shp")
  output_raster_file <- paste0(local_path,site,"/from_siteplan/",source,"landcover.tif")
  
  # Read the raster
  r <- raster(raster_file)
  
  # Read the shapefile
  shapefile <- st_read(shapefile_path)
  shapefile <- st_transform(shapefile, crs(r))
  
  # Check and convert the field to numeric if necessary
  if (is.character(shapefile$type) || is.factor(shapefile$type)) {
    shapefile$type <- as.numeric(as.character(shapefile$type))
  }
  
  # Create an output raster with the same extent, resolution, and CRS as the original raster
  output_raster <- raster(r)
  values(output_raster) <- 1  # Initialize with default value 1
  
  # Update values in the raster based on the original raster values
  #values(output_raster)[values(r) == 0] <- 2
  
  # Create a raster layer for polygons
  poly_raster <- rasterize(shapefile, output_raster, field = "type", update = TRUE)
  
  # Assign values to the raster based on the polygon type
  # Ensure polygon types are numeric (assuming 'type' column contains 5 and 6 as numeric)
  values(output_raster)[!is.na(values(poly_raster))] <- values(poly_raster)[!is.na(values(poly_raster))]
  
  # Write the output raster to a file
  writeRaster(output_raster, filename = output_raster_file, format = "GTiff", overwrite = TRUE)
  
  # Print completion message
  cat("Raster processing complete. Output saved to", output_raster_file, "\n")
  
}