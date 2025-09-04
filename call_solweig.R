call_solweig <- function(site, year, surface, posture, scenario, qgis_path){
  
  # Specify canopy raster
  if (year == 0) {canopy_raster <- "/from_lidar/cdsm.tif"}
  else {canopy_raster <- paste0("/intermediates/year_",year,"_",scenario,".tif")}
  
  # Specify surface raster
  if (surface == 1) {surface_raster <- "/from_siteplan/landcover.tif"}
  else {surface_raster <- "/from_siteplan/original_landcover.tif"}
  
  # Construct SOLWEIG command
  solweig_command <- paste0(qgis_path,
                            "qgis_process run 'umep:Outdoor Thermal Comfort: SOLWEIG' --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7030 --INPUT_DSM=",
                            local_path,site,
                            "/from_lidar/dsm.tif --INPUT_SVF=",
                            local_path,site,
                            "/intermediates/svf_y",year,"_",scenario,"/svfs.zip --INPUT_HEIGHT=",
                            local_path,site,
                            "/intermediates/wall_height.tif --INPUT_ASPECT=",
                            local_path,site,
                            "/intermediates/wall_aspect.tif --INPUT_CDSM=",
                            local_path,site,
                            canopy_raster,
                            " --TRANS_VEG=3 --LEAF_START=97 --LEAF_END=300 --CONIFER_TREES=false --INPUT_THEIGHT=25 --INPUT_LC=",
                            local_path,site,
                            surface_raster,
                            " --USE_LC_BUILD=false --INPUT_DEM=",
                            local_path,site,
                            "/from_lidar/dem.tif --SAVE_BUILD=false --INPUT_ANISO= --ALBEDO_WALLS=0.2 --ALBEDO_GROUND=0.15 --EMIS_WALLS=0.9 --EMIS_GROUND=0.95 --ABS_S=0.7 --ABS_L=0.95 --POSTURE=",
                            posture,
                            " --CYL=true --INPUTMET=",
                            local_path,site,
                            "/met_data/2023_8am_to_8pm.txt --ONLYGLOBAL=true --UTC=0 --POI_FILE=",
                            #micro
                            #local_path,site,
                            #"/from_siteplan/fine_grid_clipped.shp",
                            " --POI_FIELD= --AGE=8 --ACTIVITY=80 --CLO=0.9 --WEIGHT=27 --HEIGHT=137 --SEX=1 ",
                            "--SENSOR_HEIGHT=10 --OUTPUT_TMRT=true --OUTPUT_KDOWN=false --OUTPUT_KUP=false --OUTPUT_LDOWN=false --OUTPUT_LUP=false --OUTPUT_SH=true --OUTPUT_TREEPLANTER=false --OUTPUT_DIR=",
                            local_path,site,
                            paste0("/y", year,
                                   "_", surface,
                                   "_", posture,
                                   "_", scenario)
  )
  
  # Run SOLWEIG command
  system(solweig_command)
}