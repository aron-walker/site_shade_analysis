library(tidyverse)
library(raster)
library(pbapply)

source(paste0(script_path,"thermofeel.R")) # for UTCI
source(paste0(script_path,"calculate_dew_point.R")) # 

make_utci_rasters <- function(site, year, landcover, position, scenario){
  
  raster_directory <- paste0(local_path, site,
                             "/y", year,
                             "_", landcover,
                             "_", position,
                             "_", scenario)
  
  raster_crs <- list.files(raster_directory,
                           pattern = paste0("^","Tmrt",".*\\.tif$"),
                           full.names = TRUE) %>%
    first() %>% raster() %>% st_crs()
  
  # Load and reproject site boundary
  boundary <- paste0(local_path,site,"/from_siteplan/narrow_boundary.shp") %>%
    st_read(quiet = TRUE) %>% 
    st_transform(crs = raster_crs)
  
  # Define the function f
  f <- function(MRT, Tair, RH, U) {
    
    mrt_c <- MRT
    t2_k <- celsius_to_kelvin(Tair)
    td_k <- calculate_dew_point(Tair, RH) %>% 
      celsius_to_kelvin()
    va <- U
    
    return(calculate_utci_polynomial(t2_k, mrt_c, va, td_k))
    
  }
  
  # Set the paths
  txt_file <- paste0(local_path,site,"/met_data/2023_8am_to_8pm.txt")
  
  # Read the text file
  txt_data <- read.table(txt_file, header = TRUE, sep = "", fill = TRUE)
  
  # Get the list of raster files
  raster_files <- list.files(raster_directory, pattern = "^Tmrt.*\\.tif$", full.names = TRUE)
  
  # Ensure the raster files are sorted in the same order as the text file rows
  raster_files <- sort(raster_files) %>% head(-1)
  
  # Check if the number of rasters matches the number of rows in the text file
  if (length(raster_files) != nrow(txt_data)) {
    stop("Number of raster files does not match number of rows in the text file.")
  }
  
  # Define a function to process each raster file
  process_raster <- function(i) {
    # Read the raster
    a_raster <- raster(raster_files[i]) %>% 
      crop(extent(boundary)) %>% 
      mask(boundary)
    
    # Get the corresponding values from the text file
    b <- txt_data$Tair[i]
    c <- txt_data$RH[i]
    d <- txt_data$U[i]
    
    # Apply the function f using overlay
    result_raster <- raster::overlay(a_raster,
                                     fun = function(a_cell) f(a_cell, b, c, d))
    
    # Define output file name
    output_file <- file.path(raster_directory,
                             paste0("UTCI_",
                                    basename(raster_files[i])))
    
    # Save the result raster
    writeRaster(result_raster,
                filename = output_file,
                format = "GTiff",
                overwrite = TRUE)
  }
  
  # Use pblapply to process rasters with a progress bar
  pblapply(seq_along(raster_files), process_raster)
  
}