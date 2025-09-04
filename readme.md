### Outdoor Space Thermal Comfort Modeler

This script lays out the steps for running a SOLWEIG model of a renovated play area, including tree canopy growth and surface material changes.

Prerequisites:

 * LIDAR-derived surface models (DSM, DEM, CDSM); can be larger than needed
 * Site plan with tree species and location, surface material, and new structures
 * QGIS with the UMEP/SOLWEIG plugin
 * Google Earth Engine account and properly configured R package `rgee`
 * i-Tree data files: itree_height.csv, itree_width.csv
 * (for report writing) LaTeX distribution for RMarkdown
 * Miscellaneous site-specific parameters (see site_specific_parameters.R)
 
The R script "!config_and_workflow.R" lays out the steps of workflow, invoking scripts as needed. Note that some scripts in this repository are for generating figures for the report. A template report is included.
 
Thank you to the developers and maintainers of R and QGIS for excellent open source software. Thank you to the makers of SOLWEIG for useful and free modeling software. Thank you to Google for making Earth Engine freely available to researchers. Thank you to Brimicombe et al. for creating `thermofeel.py`.