wbgt_threshold <- function(site, year, landcover, position, scenario, threshold, boundary_type){
    
  # Define the folder containing the rasters
  folder_path <- paste0(local_path, site,
                        "/y",year,
                        "_",landcover,"_", 
                        position, 
                        "_", scenario)
  
  boundary <- paste0(local_path, site, "/from_siteplan/",
                     boundary_type, ".shp") %>%
    st_read(quiet = TRUE)
  
  # List all raster files in the folder that start with "WBGT"
  raster_files <- list.files(folder_path, pattern = "^WBGT.*\\.tif$", full.names = TRUE)
  
  # Convert boundary shapefile to an `sf` object
  boundary_sf <- st_as_sf(boundary)

  # Initialize a vector to store percentages
  percentages <- numeric(length(raster_files))
  names(percentages) <- basename(raster_files)
  
  # Function to process each raster
  process_raster <- function(raster_file) {
    # Read the raster
    r <- rast(raster_file)
    
    # Ensure the shapefile and raster are in the same CRS
    boundary_sf <- st_transform(boundary_sf, crs(r))
    
    # Clip the raster to the boundary
    clipped_raster <- mask(r, vect(boundary_sf))
    
    # Calculate the total number of cells
    total_cells <- sum(values(clipped_raster) > -100, na.rm = TRUE)
    
    # Calculate the number of cells with values greater than the threshold
    above_threshold <- sum(values(clipped_raster) > threshold, na.rm = TRUE)
    
    # Calculate the percentage
    percentage <- (above_threshold / total_cells) * 100
    
    return(percentage)
  }
  
  # Apply the function to each raster file with progress bar
  percentages <- pblapply(raster_files, process_raster)
  
  # Set names of percentages vector to match raster file names
  names(percentages) <- basename(raster_files)
  
  # Sort the percentages in alphabetical order of raster filenames
  saveRDS(percentages[order(names(percentages))],
          file = paste0(local_path,site,"/intermediates/y",
                        year,"_",landcover,"_",position,"_",scenario,
                        "_",threshold*100,
                        "_",boundary_type,".rds"))
}