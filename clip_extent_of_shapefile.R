clip_extent_of_shapefile <- function(raster_file,
                                     polygon_shapefile,
                                     output_raster){

library(sf)
library(raster)
  
  # Read the polygon shapefile
  polygon <- st_read(polygon_shapefile)
  
  # Read the raster file
  r <- raster(raster_file)
  
  # Reproject the polygon to match the raster's CRS
  polygon <- st_transform(polygon, crs(r))
  
  # Get the extent of the polygon
  polygon_extent <- st_bbox(polygon)
  
  # Create a new raster with the extent of the polygon
  r_clipped_extent <- raster(extent(polygon_extent), crs = crs(r))
  
  # Crop the raster to the extent of the polygon
  r_cropped <- crop(r, extent(r_clipped_extent))
  
  # Save the clipped raster
  writeRaster(r_cropped, output_raster, format = "GTiff", overwrite = TRUE)
  
  plot(r_cropped)
  
}