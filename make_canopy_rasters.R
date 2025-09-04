# Install and load necessary packages
library(terra)
library(sf)
library(dplyr)
library(readr)

time_steps <- c(5, 10, 15, 20, 25, 30)

make_canopy_rasters <- function(site, scenario){
  
  # Define file paths
  raster_file <- paste0(local_path, site, "/from_lidar/cdsm.tif")
  shapefile_path <- paste0(local_path, site, "/from_siteplan/trees.shp")
  gazebo_path <- paste0(local_path, site, "/from_siteplan/gazebo.shp")
  if (scenario == 1){ # create a canopy over the entire site area
    canopy_path <- paste0(local_path, site, "/from_siteplan/narrow_boundary.shp")
  }
  height_csv <- paste0(script_path,"itree_height.csv")
  width_csv <- paste0(script_path,"itree_width.csv")
  
  # Import raster and shapefile using terra
  existing_raster <- rast(raster_file)
  shapefile_data <- st_read(shapefile_path) %>%
    st_transform(crs(existing_raster)) %>%
    arrange(id)
  
  # Load CSV files
  height_eq <- read_csv(height_csv)
  width_eq <- read_csv(width_csv)
  
  # Function to compute DBH
  compute_dbh <- function(DBH0, speed, time, frost_free_days) {
    (DBH0*2.54 + speed * time * frost_free_days / 153 ) / 2.54 #1.78 = park light conditions
  }
  
  # Function to calculate the equation result
  calculate_equation <- function(species_name, DBH, data) {
    # Filter the row that matches the species_name
    species_row <- data %>% filter(Taxon == species_name)
    
    # Check if the species exists in the data
    if (nrow(species_row) == 0) {
      stop(paste("Species not found in the data:", species_name))
    }
    
    # Extract the equation and parameters
    equation <- species_row$Model
    B0 <- species_row$B0
    B1 <- species_row$B1
    B2 <- species_row$B2
    
    # Evaluate the equation based on its type
    if (grepl("e\\(B0", equation)) {
      # Exponential equation
      result <- (exp(B0 + (log(DBH) * B1))) / 3.28084
    } else if (grepl("ln\\(DBH\\)", equation)) {
      # Exponential equation
      result <- (B0 + (log(DBH) * B1)) / 3.28084
    } else if (grepl("DBH2", equation)) {
      # Polynomial equation
      result <- (B0 + (DBH * B1) + (DBH^2 * B2)) / 3.28084
    } else if (grepl("DBH", equation) && !grepl("DBH2", equation)) {
      # Linear equation
      result <- (B0 + (DBH * B1)) / 3.28084
    } else {
      stop("Unknown equation format.")
    }
    
    return(result)
  }
  
  # if (site == 'los_angeles'){
  # shapefile_data$DBH <- c(rep(1,11),
  #                         rep(1,4),
  #                         rep(1,8),
  #                         rep(1,3))
  # shapefile_data$speed <- c(rep(0.33,3),
  #                           rep(0.23,4),
  #                           rep(0.43,2), 
  #                           rep(0.23,2),
  #                           rep(0.43,4),
  #                           rep(0.43,6),
  #                           rep(0.23,2),
  #                           rep(0.43,2),
  #                           rep(0.33,1))
  # }
  # 
  # if (site == 'corlears'){
  #   shapefile_data$speed <- rep(0.43,13)
  # }
  
  # Process each time step
  for (time in time_steps) {
    # Calculate DBH for each tree at the current time step
    shapefile_data$DBHt <- compute_dbh(shapefile_data$DBH,
                                       shapefile_data$speed,
                                       time, frost_free_days)
    
    # Calculate height and width for the current time step
    height_data <- height_eq %>% filter(Taxon %in% shapefile_data$Taxon)
    width_data <- width_eq %>% filter(Taxon %in% shapefile_data$Taxon)
    
    # Check if height_data and width_data are non-empty
    if (nrow(height_data) == 0 || nrow(width_data) == 0) {
      stop("Height or width data is empty. Check your CSV files.")
    }
    
    # Calculate height and width for each tree
    shapefile_data <- shapefile_data %>%
      rowwise() %>%
      mutate(
        Height = calculate_equation(Taxon, DBHt, height_data),
        Width = calculate_equation(Taxon, DBHt, width_data)
      ) %>%
      ungroup()
    
    # Convert shapefile to sf and buffer
    shapefile_sf <- st_as_sf(shapefile_data)
    
    # Create buffers for each tree
    buffers <- st_buffer(shapefile_sf, dist = shapefile_data$Width / 2)
    
    # Create an empty raster with the same extent as the spatial data
    r <- rast(ext = ext(existing_raster), res = res(existing_raster), crs = crs(existing_raster))
    
    # Initialize an empty list to store rasters
    tree_rasters <- list()
    
    # Rasterize each tree
    for (i in 1:nrow(shapefile_data)) {
      # For each tree, create a buffer and rasterize it
      tree_type <- shapefile_data$Taxon[i]
      width <- shapefile_data$Width[i]
      height <- shapefile_data$Height[i]
      
      # Check if width and height are valid
      if (is.na(width) || is.na(height) || width <= 0 || height <= 0) {
        next
      }
      
      # Buffer distance should be half of the width (diameter)
      tree_buffer <- st_buffer(shapefile_sf[i, ], dist = width / 2)
      
      # Rasterize the buffer
      tree_raster <- rasterize(tree_buffer, r, field = height, background = 0)
      
      # Store the raster in the list
      tree_rasters[[i]] <- tree_raster
    }
    
    # Add the original canopy to the set
    tree_rasters[[length(tree_rasters)+1]] <- existing_raster
    
    # Gazebo
    # Check if the shapefile exists at the specified path
    if (file.exists(gazebo_path)) {
      # Read the shapefile using st_read
      gazebo_vector <- st_read(gazebo_path) %>%
        st_transform(crs(existing_raster))
      gazebo_height <- gazebo_vector$height
      gazebo_raster <- gazebo_vector %>%
        rasterize(r, field = gazebo_height, background = 0)
      tree_rasters[[length(tree_rasters)+1]] <- gazebo_raster
    } 
    
    # Max_Canopy
    # Check if the shapefile exists at the specified path
    if (scenario == 1) {
      # Read the shapefile using st_read
      canopy_vector <- st_read(canopy_path) %>%
        st_transform(crs(existing_raster))
      canopy_height <- 3.048
      canopy_raster <- canopy_vector %>%
        rasterize(r, field = canopy_height, background = 0)
      tree_rasters[[length(tree_rasters)+1]] <- canopy_raster
    } 
    
    # Combine rasters by taking the maximum value at each cell
    if (length(tree_rasters) > 0) {
      if (scenario == 0){
      combined_raster <- app(rast(tree_rasters), fun = max, na.rm = TRUE)
      }
      if (scenario == 1){ # hypothetical canopy plus existing trees
        alt_raster_package <- list(existing_raster,canopy_raster) # no new trees (too high)
        combined_raster <- app(rast(alt_raster_package), fun = max, na.rm = TRUE)
        }
    } else {
      stop("No rasters were created for this time step.")
    }
    
    # Save the combined raster for the current time step
    output_raster_file <- paste0(local_path, site, "/intermediates/year_", time,
                                 "_", scenario, ".tif")
    writeRaster(combined_raster, output_raster_file, overwrite = TRUE)
    
    print(paste("Saved combined raster for time step", time))
  }
  
  print("All time step rasters have been saved.")
  
}
 
show_canopy_rasters <- function(site, scenario){
 
  # Define file paths for the rasters (assuming you have saved these rasters already)
  raster_files <- paste0(local_path, site,
                         "/intermediates/year_", time_steps,
                         "_", scenario, ".tif")
  
  # Load the rasters
  rasters <- lapply(raster_files, rast)
  
  # Set up plotting layout for a 2x2 grid
  par(mfrow = c(2, 3))  # 2 rows, 2 columns
  
  # Define the min and max values for the color scheme
  min_value <- 0
  max_value <- 20
  
  # Define the color breaks and corresponding colors
  breaks <- seq(min_value, max_value, length.out = 9)
  colors <- terrain.colors(50)
  
  # Plot with fixed min and max color scheme

  
  # Plot each raster
  for (i in seq_along(rasters)) {
    plot(rasters[[i]], 
         main = paste("Year", time_steps[i]),
         col = colors, 
         breaks = breaks)  # Use terrain.colors for a color palette
    
    # Optionally add a legend
    #legend("topright", legend = c("Low", "High"), fill = terrain.colors(2), title = "Value")
  }
  
  # Reset plotting layout to default
  par(mfrow = c(1, 1))

}
