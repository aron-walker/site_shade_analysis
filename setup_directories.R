## This function creates a directory structure for storing inputs
## and intermediates along the analysis pipeline

setup_directories <- function(path, place){
  
  # Create the main directory path/place
  main_dir <- file.path(path, place)
  dir.create(main_dir, showWarnings = FALSE, recursive = TRUE)
  
  # List of subdirectories to create
  subdirs <- c("from_lidar", "from_siteplan", "intermediates", "met_data")
  
  # Create subdirectories
  for (subdir in subdirs) {
    dir.create(file.path(main_dir, subdir), showWarnings = FALSE)
  }
  
  # Create directories for ya_b_c_d
  for (a in c(0, years_of_growth)) {
    for (b in c(0, 1)) {
      for (c in positions) {
        for (d in c(0, 1)) {
          dir_name <- paste0("y", a, "_", b, "_", c, "_", d)
          dir.create(file.path(main_dir, dir_name), showWarnings = FALSE)
        }
      }
    }
  }
}