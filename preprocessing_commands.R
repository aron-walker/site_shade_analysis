# Construct SOLWEIG wall command
wall_command <- paste0(qgis_path,
                       "qgis_process run 'umep:Urban Geometry: Wall Height and Aspect' --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7030 --INPUT=",
                       local_path,site,
                       "/from_lidar/dsm.tif --INPUT_LIMIT=3 --OUTPUT_HEIGHT=",
                       local_path,site,
                       "/intermediates/wall_height.tif --OUTPUT_ASPECT=",
                       local_path,site,
                       "/intermediates/wall_aspect.tif")

make_svf <- function(year, scenario, qgis_path){
  
  # Specify canopy raster
  if (year == 0) {
    canopy_raster <- "/from_lidar/cdsm.tif"
  } else {
    canopy_raster <- paste0("/intermediates/year_",year,"_",scenario,".tif")
  }
  
  # Construct SOLWEIG SVF command
  svf_command <- paste0(qgis_path,
                        "qgis_process run 'umep:Urban Geometry: Sky View Factor' --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7030 --INPUT_DSM=",
                        local_path,site,
                        "/from_lidar/dsm.tif --INPUT_CDSM=",
                        local_path,site,
                        canopy_raster,
                        " --TRANS_VEG=3 --INPUT_THEIGHT=25 --ANISO=true --OUTPUT_DIR=",
                        local_path,site,
                        "/intermediates/svf_y",
                        year,"_",scenario,
                        " OUTPUT_FILE=TEMPORARY_OUTPUT")
  
  # Run SOLWEIG SVF command
  system(svf_command)
}