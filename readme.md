### Outdoor Space Thermal Comfort Modeling Pipeline

This script lays out the steps for running a SOLWEIG model of a renovated play area, including tree canopy growth and surface material changes, and using the outputs to calculate usable space.

Prerequisites:

 * LIDAR-derived surface models (DSM, DEM, CDSM); can be larger than needed
 * Site plan with tree species and location, surface material, and new structures
 * QGIS with the UMEP/SOLWEIG plugin
 * Google Earth Engine account and properly configured R package `rgee`
 * i-Tree data files (extracted from USFS publication): itree_height.csv, itree_width.csv
 * (for report writing) LaTeX distribution for RMarkdown
 * Miscellaneous site-specific parameters (see site_specific_parameters.R)
 
The R script "!config_and_workflow.R" lays out the steps of workflow, invoking scripts as needed. Note that some scripts in this repository are for generating figures for the report. A template report is included. The overall process is:

1. Use R scripts to set up appropraite directory structure and store site-specific information
2. Use QGIS with the graphical interface to prepare geospatial files based on the site plans (tree locations, species, DHB; structures; land cover classification)
3. Use R scripts invoking `rgee` to pull meteorological data
4. Use R scripts to generate a system call to QGIS to use SOLWEIG in the background
5. Use R scripts to process the SOLWEIG outputs
6. Use RMarkdown / LaTeX and additional R scripts to analyze process outputs and generate a report
 
Thank you to the developers and maintainers of R, RMarkdown, QGIS, and SOLWEIG for excellent open source software. Thank you to Google for making Earth Engine freely available to researchers. Thank you to Brimicombe et al. for creating `thermofeel`. Thank you to USFS / Nowak et al for creating i-Tree and making its data publicly accessible. Thank you to the Trust for Public Land, The Nike Foundation, and UCLA's Luskin Center for Innovation who guided, funded, and supported this project.

Please contact aronwalker@ucla.edu with questions, problems, etc. I will help if I can.
