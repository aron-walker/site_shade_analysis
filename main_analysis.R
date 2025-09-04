run_analysis <- function(site){
  
  # Load libraries
  library(tidyverse)
  library(pbapply)
  library(sf)
  library(terra)
  library(raster)
  source(paste0(script_path,"formatting_functions.R"))
  source(paste0(script_path, "site_specific_variables.R")) ## needs to include info re: site
  
  # # # Crop surface models to boundary.shp
  # source(paste0(script_path,"clip_extent_of_shapefile.R"))
  # clip_extent_of_shapefile(paste0(local_path,site,"/from_lidar/",site,"_dsm.tif"),
  #                          paste0(local_path,site,"/from_siteplan/","boundary.shp"),
  #                          paste0(local_path,site,"/from_lidar/","dsm.tif"))
  # clip_extent_of_shapefile(paste0(local_path,site,"/from_lidar/",site,"_dem.tif"),
  #                          paste0(local_path,site,"/from_siteplan/","boundary.shp"),
  #                          paste0(local_path,site,"/from_lidar/","dem.tif"))
  # clip_extent_of_shapefile(paste0(local_path,site,"/from_lidar/",site,"_cdsm.tif"),
  #                          paste0(local_path,site,"/from_siteplan/","boundary.shp"),
  #                          paste0(local_path,site,"/from_lidar/","cdsm.tif"))
  # 
  # # Get meteorological data
  # source(paste0(script_path,"generate_met_data.R")) # Calls Google Earth Engine (GEE)
  # source(paste0(script_path,"load_met_data.R")) # Retrieves the result from GEE
  # source(paste0(script_path,"extract_met_data.R")) # Processes and formats the data for SOLWEIG
  #   generate_met_data(site) # Extracts met data at centroid of project area
  #   print("Pausing 5 minutes for Google Earth Engine to work and Google Drive to sync")
  #   countdown_timer(300) # Enough time for GEE to run and Google Drive to sync
  #   load_met_data(site) %>%
  #     extract_met_data()
  #   print("Meteorological data retrieved and formatted")
  #   
  # Make canopy rasters
  # Input: cdsm.tif + trees.shp + structures.shp (optional)
  # Output: intermediates/yearN_cdsm.tif (canopy rasters updated for tree growth)
  # source(paste0(script_path,"make_canopy_rasters.R"))
  #   make_canopy_rasters(site, 0)
  #   make_canopy_rasters(site, 1)
  #   show_canopy_rasters(site, 0)
  #   show_canopy_rasters(site, 1)
  
  # Make landcover raster
  # Input: dsm.tif + landcover.shp (or original_landcover.shp)
  # Output: from_siteplan/landcover.tif (or from_siteplan/original_landcover.shp)
  source(paste0(script_path,"make_landcover.R"))
  
  if (model_surface_changes == 1) {
    make_landcover(site, 0) # original_landcover
    make_landcover(site, 1) # landcover, i.e. renovated surfaces
    landcovers <- c(0,1)
  }
  
  if (model_surface_changes == 0) {
    make_landcover(site, 1) # only one landcover file
    landcovers <- c(1)
  }
  
  print("Canopy and landcover rasters ready; starting SOLWEIG preprocessing")
  
  # Pre-processing
  source(paste0(script_path,"preprocessing_commands.R"))
  system(wall_command)
  
  combos <- expand.grid(a = c(0,years_of_growth),
                        b = 0)
  combos <- rbind(combos,c(5,1)) # add max scenario
  
  pblapply(seq(1, dim(combos)[1]),
           function(i) {make_svf(combos[i,1],
                                 combos[i,2],
                                 qgis_path)})
  
  print("Preprocessing complete, starting SOLWEIG")
  print("Expect one SOLWEIG progress bar (0...1...) for each
        {year, landcover, position} combination")
  
  # run SOLWEIG
  source(paste0(script_path,"call_solweig.R"))
  
  # Permute combinations of inputs
  combinations <- expand.grid(a = years_of_growth,
                              b = landcovers,
                              c = 1,
                              d = 0)
  
  max_case <- c(5, 1-model_surface_changes,1,1)
  
  if (model_surface_changes == 1) {
    baseline_case <- c(0, 0, 1, 0)
    surface_only <- c(0, 1, 1, 0)
    combinations <- rbind(baseline_case,surface_only,combinations,max_case)
    
  }
  
  if (model_surface_changes == 0) {
    baseline_case <- c(0, 1, 1, 0)
    combinations <- rbind(baseline_case,combinations,max_case)
  }
  
  # can use something like the below to model fewer cases
  # combinations <- combinations[c(1,6:10),]
  
  # Apply the function to each combination
  pblapply(seq(1, dim(combinations)[1]),
           function(i) {call_solweig(site,
                                     combinations[i,1],
                                     combinations[i,2],
                                     combinations[i,3],
                                     combinations[i,4],
                                     qgis_path)})
  
  print("SOLWEIG complete, processing outputs")
  print("Expect one progress bar per {year, landcover, position} combination")
  
  # Process SOLWEIG outputs
  source(paste0(script_path,"thermofeel.R")) # for WBGT, from Brimicombi et al.
  source(paste0(script_path,"process_solweig.R"))
  
  boundary <- "narrow_boundary" # can change to consider multiple ways of defining project area
  
  pblapply(seq(1, dim(combinations)[1]),
           function(i) {process_solweig(site,
                                        combinations[i,1],
                                        combinations[i,2],
                                        combinations[i,3],
                                        combinations[i,4],
                                        boundary)})
  
  print("SOLWEIG outputs processed, making WBGT rasters")
  print("Expct one progress bar per {year, landcover, position} combination")
  
  # Make WBGT rasters
  source(paste0(script_path,"thermofeel.R")) # for WBGT
  source(paste0(script_path,"calculate_dew_point.R"))
  source(paste0(script_path,"make_wbgt_rasters.R"))
  
  # # Permute combinations of inputs
  # combinations <- expand.grid(a = years_of_growth,
  #                             b = landcovers,
  #                             c = positions)
  
  lapply(seq(1, dim(combinations)[1]),
         function(i) {make_wbgt_rasters(site,
                                        combinations[i,1],
                                        combinations[i,2],
                                        combinations[i,3],
                                        combinations[i,4])})
  
  print("WBGT rasters complete, start UTCI rasters")
  print("Expct one progress bar per {year, landcover, position} combination")
  
  # Make UTCI rasters
  source(paste0(script_path,"thermofeel.R")) # for WBGT
  source(paste0(script_path,"calculate_dew_point.R"))
  source(paste0(script_path,"make_utci_rasters.R"))
  
  # # Permute combinations of inputs
  # combinations <- expand.grid(a = years_of_growth,
  #                             b = landcovers,
  #                             c = positions)
  
  lapply(seq(1, dim(combinations)[1]),
         function(i) {make_utci_rasters(site,
                                        combinations[i,1],
                                        combinations[i,2],
                                        combinations[i,3],
                                        combinations[i,4])})
  
  print("UTCI rasters complete, start usability analysis")
  print("Expct one progress bar per {year, landcover, position} combination")
  
  # Analyze WBGT effects
  source(paste0(script_path,"wbgt_threshold.R"))
  
  # https://www.weather.gov/ict/wbgt
  # https://www.sciencedirect.com/science/article/pii/S0143622814002513?via%3Dihub
  cat1_thresholds <- c(22.4, 24.6, 26.8, 29.0)
  cat2_thresholds <- c(24.4, 26.6, 28.8, 31.0)
  cat3_thresholds <- c(25.7, 27.9, 30.1, 32.3)
  
  if (threshold_classification == 1){thresholds <- cat1_thresholds}
  if (threshold_classification == 2){thresholds <- cat2_thresholds}
  if (threshold_classification == 3){thresholds <- cat3_thresholds}
  
  level_1_combos <- cbind(combinations, d = thresholds[1])
  level_2_combos <- cbind(combinations, d = thresholds[2])
  level_3_combos <- cbind(combinations, d = thresholds[3])
  level_4_combos <- cbind(combinations, d = thresholds[4])
  
  combos_w_thresholds <- rbind(level_1_combos,
                               level_2_combos,
                               level_3_combos,
                               level_4_combos)
  
  # # Permute combinations of inputs
  # combinations <- expand.grid(a = years_of_growth,
  #                             b = landcovers,
  #                             c = positions,
  #                             d = thresholds)
  
  # combinations <- combinations[c(1,6:10,11,16:20,21,26:30,31,36:40),]
  
  boundary_type <- "narrow_boundary"
  
  pblapply(seq(1, dim(combos_w_thresholds)[1]),
           function(i) {wbgt_threshold(site, combos_w_thresholds[i,1],
                                       combos_w_thresholds[i,2],
                                       combos_w_thresholds[i,3],
                                       combos_w_thresholds[i,4],
                                       combos_w_thresholds[i,5],
                                       boundary_type)})
}