process_solweig <- function(site, year, landcover, position, scenario, boundary){

  # Play area boundary
  boundary <- paste0(local_path, site, "/from_siteplan/",
                     boundary, ".shp") %>%
    st_read(quiet = TRUE)
  
  # Met data
  met_data_path <- paste0(local_path, site,
                          "/met_data/2023_8am_to_8pm.txt")
  met_data <- read.table(file = met_data_path,
                         header = TRUE, sep = " ",
                         stringsAsFactors = FALSE)
  
    # Function to calculate dew point temperature
    calculate_dew_point <- function(Ta, RH) {
      # Calculate saturation vapor pressure
      E_s <- 6.112 * exp((17.67 * Ta) / (Ta + 243.5))
      # Calculate actual vapor pressure
      E_a <- (RH / 100) * E_s
      # Calculate dew point temperature
      Tdew <- (243.5 * log(E_a / 6.112)) / (17.67 - log(E_a / 6.112))
      return(Tdew)
    }
  
  met_data <- met_data %>%
    mutate(Tdew = calculate_dew_point(Tair, RH))
  
  # Path to rasters
  
  ## sites with meaningful surface changes
  if (model_surface_changes == 1) {
    baseline_directory <-  paste0(local_path, site,
                                  "/y0_0_", 
                                  position, 
                                  "_0")
  }
  
  ## sites without meaningful surface changes (not making surface = 0 rasters)
  if (model_surface_changes == 0) {
    baseline_directory <-  paste0(local_path, site,
                                  "/y0_1_", 
                                  position, 
                                  "_0")
  }
  
  future_directory <-  paste0(local_path, site,
                              "/y", year,
                              "_", landcover,
                              "_", position,
                              "_", scenario)
  
  baseline_rasters <- list.files(path = baseline_directory, 
                               pattern = "^Tmrt_[0-9]+", 
                               full.names = TRUE)
  
  future_rasters <- list.files(path = future_directory, 
                               pattern = "^Tmrt_[0-9]+", 
                               full.names = TRUE)
  
  extract_numeric <- function(file_path) {
    # Extract the filename from the full path
    filename <- basename(file_path)
    # Extract the segment between the second and third underscores
    parts <- unlist(strsplit(filename, "_"))
    # Return the numeric part as an integer
    as.integer(parts[3])
  }
  
  # Sort filenames based on the numeric part
  baseline_rasters <- baseline_rasters[order(sapply(baseline_rasters,
                                                    extract_numeric))]
  future_rasters <- future_rasters[order(sapply(future_rasters,
                                                extract_numeric))]
  
  process_raster <- function(index){
  
    # Read the raster file
    baseline_raster_data <- rast(baseline_rasters[index])
    future_raster_data <- rast(future_rasters[index])
    
    # Ensure the shapefile and raster are in the same CRS
    boundary <- st_transform(boundary, crs(baseline_raster_data))
    
    boundary_terra <- vect(boundary)
    
    # Mask the raster with the shapefile boundary
    baseline_raster_masked <- mask(baseline_raster_data, boundary_terra)
    future_raster_masked <- mask(future_raster_data, boundary_terra)
    
    baseline_raster_df <- as.data.frame(baseline_raster_masked,
                                        xy = TRUE, na.rm = TRUE)
    future_raster_df <- as.data.frame(future_raster_masked,
                                      xy = TRUE, na.rm = TRUE)
    
    names(baseline_raster_df)[3] <- "baseline_mrt"  # Rename the raster layer column to 'value'
    names(future_raster_df)[3] <- "future_mrt"  # Rename the raster layer column to 'value'
    
    baseline_mean_mrt <- mean(baseline_raster_df$baseline_mrt, na.rm = TRUE)
    future_mean_mrt <- mean(future_raster_df$future_mrt, na.rm = TRUE)
    
    t2_k <- celsius_to_kelvin(met_data$Tair[index])
    td_k <- celsius_to_kelvin(met_data$Tdew[index])
    va <- met_data$U[index]
  
    baseline_raster_df$baseline_wbgt <- mapply(calculate_wbgt, t2_k,
                                               baseline_raster_df$baseline_mrt,
                                               va, td_k)
    future_raster_df$future_wbgt <- mapply(calculate_wbgt, t2_k,
                                      future_raster_df$future_mrt,
                                      va, td_k)
  
    baseline_mean_wbgt <- mean(baseline_raster_df$baseline_wbgt, na.rm = TRUE)
    future_mean_wbgt <- mean(future_raster_df$future_wbgt, na.rm = TRUE)
    
    baseline_raster_df$baseline_utci <- mapply(calculate_utci_polynomial, t2_k,
                                               baseline_raster_df$baseline_mrt,
                                               va, td_k)
    
    future_raster_df$future_utci <- mapply(calculate_utci_polynomial, t2_k,
                                               future_raster_df$future_mrt,
                                               va, td_k)
    
    baseline_mean_utci <- mean(baseline_raster_df$baseline_utci, na.rm = TRUE)
    future_mean_utci <- mean(future_raster_df$future_utci, na.rm = TRUE)
    
    # Calculate the average value of the raster within the boundary
    return(c(baseline_mean_mrt,future_mean_mrt,
             baseline_mean_wbgt,future_mean_wbgt,
             baseline_mean_utci,future_mean_utci))
  }
  
  # Apply the function to each value in the sequence
  results <- pblapply(1:length(baseline_rasters), process_raster)
  
  # Join results into a single data frame
  results_df <- as.data.frame(do.call(rbind, results))
  results_df <- cbind(1:length(baseline_rasters), results_df)
  
  # Set column names
  colnames(results_df) <- c("timestep","baseline_mean_mrt",
                            "future_mean_mrt","baseline_mean_wbgt","future_mean_wbgt",
                            "baseline_mean_utci","future_mean_utci")
  
  # Save results
  saveRDS(results_df, file = paste0(local_path, site, 
                                    "/intermediates/y", year, 
                                    "_", landcover, "_", position,
                                    "_", scenario,
                                    "_results.rds"))
}