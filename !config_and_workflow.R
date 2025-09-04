##################################################################
## ** Outdoor Space Thermal Comfort Modeler **
## This script lays out the steps for running a SOLWEIG model
## of a renovated play area, including tree canopy growth
## and surface material changes.
##################################################################
## Prerequisites:
##  * LIDAR-derived surface models (DSM, DEM, CDSM); can be larger than needed
##  * Site plan with tree species and location, surface material, and new structures
##  * QGIS with the UMEP/SOLWEIG plugin
##  * Google Earth Engine account and properly configured R package `rgee`
##  * i-Tree data files: itree_height.csv, itree_width.csv
##  * (for report writing) LaTeX distribution for RMarkdown
##  * Miscellaneous site-specific parameters (see site_specific_parameters.R)
##################################################################
## Step 1: Configuration
## Step 1.1: Set once per new system (i.e. computer or cloud location)

## Set path to scripts directory (e.g. Git Repo)
script_path <- ""

## Set path to local directory for QGIS and intermediate outputs (e.g. Box)
local_path <- ""

## Set path to Google Earth Engine outputs directory (e.g. via auto-sync Google Drive)
gee_path <- ""

## Set system path to QGIS qgis_process command (OS dependent)
qgis_path <- "/Applications/QGIS-LTR.app/Contents/MacOS/bin/"

## Step 1.2: Set for each site
#site <- "los_angeles" ## name must match name in site_specific_variables.R

## Specify years of canopy growth to model
years_of_growth <- c(5, 10, 15, 20)

## Specify "position" of humans for thermal exposure
## SOLWEIG default is standing c(0); for sitting, use c(1)
## To compare, use both: c(0,1)
positions <- c(1)

## Modeling scenario (0 = as renovated, 1 = theoretical max (10' canopy))
scenarios <- c(0, 1) # for both, use c(0,1)

##################################################################
## Step 2: Assemble data
# 
## Step 2.1: Setup directories in local_path
source(paste0(script_path,"setup_directories.R"))
setup_directories(local_path, "chicago")
setup_directories(local_path, "chicago_canopies")
setup_directories(local_path, "corlears")
setup_directories(local_path, "los_angeles")
setup_directories(local_path, "los_angeles_big_trees")
setup_directories(local_path, "vince")

## Step 2.2: In local_path/site/from_lidar, place surface models:
## <site>_dsm.tif, <site>_dem.tif, <site>_cdsm.tif
## These models will be cropped; it is okay if they are larger than needed.

## Step 2.3: In QGIS, georeference the site plan.
## (only if manually creating the shapefiles below)

## Step 2.4: In local_path/site/from_siteplan, create with QGIS:
## * boundary.shp ## area for SOLWEIG (include anything that could shade play space)
## * narrow_boundary.shp ## specific project area, e.g. just the play space
## * trees.shp with fields:
##    * "speed" [number, precision = 2] (0.23 = slow, 0.33 = moderate, 0.43 = fast)
##        --> Look up growth rates: https://database.itreetools.org/#/species
##        --> Also note family and order in case needed for next field...
##    * "Taxon" [string, 30 char] (find closest match in itrees_height.csv)
##        --> if the species is present, enter <Genus species>;
##        --> else, Genus; else, Family; else, Order
##    * "DBH" [number] (trunk diameter at 1.3 meters)
##        --> if not given in plans (i.e. tree size given by height in feet):
##            use Taxon-matching iTree_height.csv equation and "solve" for DBH
##            (e.g. 8.34 + 3.6x - 0.003x^2 = 10, solve for x, e.g. wolframalpha.com)
## * gazebo.shp with field "height" [number] (unit = meters)
##    --> can use for any new structure
## * landcover.shp with field type:
##        --> types: 1 = pavement, 2 = buildings, 5 = grass, 6 = bare ground
##        --> these are the *only* options
##        --> "cool" pavements probably closest to bare ground (more reflective)
## * original_landcover.shp (if there was a different original landcover)

## Step 2.5: Populate script_path/site_specific_variables.R with data for site

##################################################################
## Step 3: Run the following R script:
## This script will display a series of progress bars (i.e. more than one!)
## Estimated total time 6-24 hours, highly dependent on the size of boundary.shp
source(paste0(script_path,"main_analysis.R"))
site <- "los_angeles"
site <- "los_angeles_big_trees"
site <- "chicago"
site <- "chicago_add_canopies"
site <- "corlears"
site <- "vince"
run_analysis(site)
## n.b. there are some configurable parameters in this script, for atypical cases

##################################################################
## Step 4: Prepare <site>_report.Rmd, using another as a template
## (the report template incorporates modeling results and calls scripts for figures)
## ((Consider using `trackdown` R package to edit narrative text in Google Docs))
## https://cran.r-project.org/web/packages/trackdown/vignettes/trackdown-workflow.html
## trackdown::update_file(file = paste0(script_path,"combined_report.Rmd"))
## trackdown::download_file(file = paste0(script_path,"combined_report.Rmd"))
## Can download NAIP image for figure background (uses rgee)
## source(paste0(script_path,"get_naip_background_image.R))
## get_naip_background_image(site)
##################################################################