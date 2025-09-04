## Uses GEE to acquire NAIP background image
## currently works for Vince and Chicago
## a naming change to Tmrt outputs makes Corlears and Los Angeles not currently work

get_naip_background_image <- function(site){
  
  # Load libraries
  library(tidyverse)
  library(pbapply)
  library(sf)
  library(terra)
  library(raster)
  library(viridis)
  library(ggplot2)
  library(ggnewscale)
  
  # Path to the shapefile (make sure to provide the correct path)
  playarea_path <- paste0(local_path, site, "/from_siteplan/narrow_boundary.shp")
  
  # Read the shapefile
  boundary <- st_read(playarea_path, quiet = TRUE)
  
  year <- 10
  surface <- 1
  position <- 1
  day <- 226
  results_hour <- 19
  scenario <- 0
  
  # Path to the raster file and shapefile
  raster_path <- paste0(local_path, site, "/y", year,"_",surface,"_",position,"_",scenario,
                        "/Tmrt_2023_", day, "_",results_hour,"00D.tif")
  
  # Read the raster file
  raster_data <- rast(raster_path)
  
  # Ensure the shapefile and raster are in the same CRS
  boundary <- st_transform(boundary, crs(raster_data))
  
  # Convert the shapefile to a SpatVector for use with terra
  boundary_terra <- vect(boundary)
  
  # Mask the raster with the shapefile boundary
  r_clipped <- mask(raster_data, boundary_terra)
  
  naip_crs <- "EPSG:4326"
  
  r <- terra::project(r_clipped, naip_crs)
  
  #bounds <- terra::project(boundary_terra, naip_crs)
  
  # Get the extent of the raster
  ext <- ext(r)
  
  ### work in EE
  
  library(rgee)
  
  # Initialize Earth Engine
  ee_Initialize()
  
  # Define bounding box coordinates
  xmin <- ext[1]
  xmax <- ext[2]
  ymin <- ext[3]
  ymax <- ext[4]
  
  # Create a bounding box for your extent
  bounding_box <- ee$Geometry$Rectangle(c(xmin, ymin, xmax, ymax))
  
  # Define the NAIP ImageCollection and filter by date and bounding box
  naip_collection <- ee$ImageCollection('USDA/NAIP/DOQQ') %>%
    ee$ImageCollection$filterBounds(bounding_box) %>%
    ee$ImageCollection$filterDate('2020-01-01', '2023-12-31') # Adjust the date range as needed
  
  # Select a single image from the filtered collection
  naip_image <- naip_collection$first()
  
  # Define visualization parameters
  vis_params <- list(
    bands = c('R', 'G', 'B'), 
    min = 0,
    max = 255,
    gamma = 1.4
  )
  
  # Center the map on the bounding box 
  Map$centerObject(bounding_box, zoom = 10) 
  Map$addLayer(naip_image$clip(bounding_box), vis_params, 'NAIP Image')
  
  # Export the image
  task <- ee$batch$Export$image$toDrive(
    image = naip_image$clip(bounding_box),
    description = 'naip_image_export',
    fileNamePrefix = paste0('naip_background_image_',site),
    folder = "rgee_backup",
    scale = 0.5, 
    region = bounding_box
  )
  
  # Start the export task
  task$start()
  
  # ###
  # 
  # # Load the downloaded NAIP image
  # naip_raster <- rast(paste0(gee_path,paste0("naip_background_image_",site,".tif"))) %>%
  #   project(naip_crs)
  # 
  # # Convert rasters to data frames for plotting
  # r_df <- as.data.frame(r, xy = TRUE)
  # colnames(r_df) <- c("x","y","value")
  # naip_df <- as.data.frame(naip_raster, xy = TRUE)
  # naip_df$rgb <- with(naip_df, rgb(R/255, G/255, B/255))
  # 
  # 
  # # Plot using ggplot2
  # # Create plots
  # ggplot() +
  #   # Plot the data raster with the Inferno palette
  #   geom_raster(data = naip_df, aes(x = x, y = y, fill = rgb), alpha = 1) +
  #   scale_fill_identity() +
  #   new_scale_fill() + 
  #   geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  #   scale_fill_viridis_c(option = "inferno") +
  #   coord_equal() +
  #   theme_minimal() +
  #   theme(axis.title.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.text.y = element_blank(),
  #         legend.position = "bottom")
}