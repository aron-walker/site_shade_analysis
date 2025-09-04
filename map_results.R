## This function generates MRT maps from SOLWEIG's MRT rasters
## Maps are super-imposed on an aerial image from NAIP
## This image must be first generated in Google Earth Engine and saved to
## script_path/images/<site>_naip_image_half_meter.tif
## Use of multiple color scales (MRT:inferno, NAIP:rgb) req's library(ggnewscale)

## min_value and max_value can be adjusted for plot color bar
library(scales)
source(paste0(script_path,"formatting_functions.R"))

map_results <- function(site, year, surface, position, scenario, boundary, day, hour, name){
  
  if (site %in% c("los_angeles","los_angeles_big_trees")){time_zone_offset <- 7}
  if (site %in% c("chicago","chicago_add_canopies")){time_zone_offset <- 5}
  if (site %in% c("corlears","vince")){time_zone_offset <- 4}
  
  results_hour <- hour + time_zone_offset
  
  naip_crs <- "EPSG:4326"
  
  # Path to the raster file and shapefile
  raster_data <- paste0(local_path, site,
                        "/y", year,
                        "_",surface,
                        "_",position,
                        "_",scenario,
                        "/Tmrt_2023_",
                        day,"_",
                        results_hour,"00D.tif") %>%
    rast()
  
  # Read the shapefile
  boundary_terra <- paste0(local_path, site, "/from_siteplan/", boundary, ".shp") %>%
    st_read(quiet = TRUE) %>%
    st_transform(crs(raster_data)) %>%
    vect()
  
  # Mask the raster with the shapefile boundary
  raster_masked <- mask(raster_data, boundary_terra) %>%
    terra::project(naip_crs)
  
  # Calculate the average value of the raster within the boundary
  mean_value <- global(raster_masked, fun = "mean", na.rm = TRUE)
  
  # Convert the raster to a data frame for ggplot
  r_df <- as.data.frame(raster_masked, xy = TRUE, na.rm = TRUE)
  names(r_df)[3] <- "value"  # Rename the raster layer column to 'value'
  
  min_val <- 25
  max_val <- 75
  naip_df <- rast(paste0(gee_path, "/naip_background_image_",
                         site,".tif")) %>%
  # naip_df <- rast(paste0(script_path, "images/",
  #                        site, "_naip_image_half_meter.tif")) %>%
    terra::project(naip_crs) %>%
    as.data.frame(xy = TRUE)
  naip_df$rgb <- with(naip_df, rgb(R/255, G/255, B/255))
  
  c2f <- function(c) (c * 9/5) + 32
  
  ggplot() +
    ## NAIP background
    geom_raster(data = naip_df, aes(x = x, y = y, fill = rgb), alpha = 1) +
    scale_fill_identity() +
    ## MRT
    new_scale_fill() + 
    geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "inferno", 
                         na.value = "transparent", 
                         limits = c(min_val, max_val),
                         breaks = seq(min_val, max_val, by = 10),
                         labels = function(x) paste0(round(x, 0), "°C\n", round(c2f(x), 0), "°F")) +  # Celsius on bottom
    theme_minimal() +
    guides(fill = guide_colorbar(title = "Mean Radiant Temperature",
                                 title.position = "top",
                                 label.position = "bottom",
                                 barwidth = 15, 
                                 barheight = 0.5,
                                 ticks = TRUE,
                                 ticks.colour = "black",
                                 frame.colour = "black")) +
    coord_equal() +
    labs(title = paste0(name," on ", day_of_year_to_date(day),
                        " at ", convert_to_time(hour),
                        " (Year ", year,")"),
         subtitle = paste0("Average mean radiant temperature = ",
         round(mean_value,0)," °C / ",
         round(mean_value,0)*9/5+32, " °F")) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom")
}