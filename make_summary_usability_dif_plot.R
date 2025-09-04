make_summary_usability_dif_plot <- function(site,year,
                               surface,
                               position,
                               threshold_level,
                               boundary_type){
  
  # https://www.weather.gov/ict/wbgt
  # https://www.sciencedirect.com/science/article/pii/S0143622814002513?via%3Dihub
  cat1_thresholds <- c(22.4, 24.6, 26.8, 29.0)
  cat2_thresholds <- c(24.4, 26.6, 28.8, 31.0)
  cat3_thresholds <- c(25.7, 27.9, 30.1, 32.3)
  
  # http://www.castlewilliams.com/wbgt-regions.html
  cat1_sites <- c("los_angeles","corlears","vince")
  cat2_sites <- c("chicago")
  cat3_sites <- c()
  
  if (site %in% cat1_sites){thresholds <- cat1_thresholds}
  if (site %in% cat2_sites){thresholds <- cat2_thresholds}
  if (site %in% cat3_sites){thresholds <- cat3_thresholds}
  
  threshold_text <- c(" vs. Elevated Threat WBGT",
                      " vs. Moderate Threat WBGT",
                      " vs. High Threat WBGT",
                      " vs. Extreme Threat WBGT")[threshold_level]
  
  if (year == 0){
    if (surface == 1){
      subtitle <- paste0("Only surface changes", threshold_text)
    }
    if (surface == 0){
      subtitle <- paste0("No changes", threshold_text)
    }
  }
  else {
    if (surface == 0) {
      surface_text <- " (no surface change)"}
    if (surface == 1) {
      surface_text <- " & surface changes"
    }
    subtitle <- paste0(year, " years later", surface_text,
                       threshold_text)
  }
  
  get_wbgt_usability <- function(year, surface, position, threshold){
    
    readRDS(paste0(local_path, site, "/intermediates/y",
                   year, "_", surface, "_", position, "_",
                   threshold*100, "_", boundary_type, ".rds")) %>%
      unlist() %>%
      replace_na(0) %>%
      return()
    
  }
  
  baseline <- get_wbgt_usability(0,1,position,thresholds[threshold_level]) / 100
  future <- get_wbgt_usability(year,surface,position,thresholds[threshold_level]) / 100
  
  dif <- baseline - future
  
  results_df <- data.frame(seq(1,length(baseline)),dif)
  colnames(results_df) <- c("timestep", "wbgt_usability_dif")

# Step 1: Read the data
met_data <- read.table(paste0(local_path,site,"/met_data/2023_8am_to_8pm.txt"),
                   header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

# Step 2: Process the data
# Convert `Tair` to numeric, if necessary
met_data$Tair <- as.numeric(met_data$Tair)

# Add a column to identify each day
# The day starts from hour 15, so we need to adjust this
met_data$Day <- (met_data$it >= 15) * met_data$id + (met_data$it < 15) * (met_data$id - 1)

# Group by `Day` and find the max `Tair` for each day
daily_max <- met_data %>%
  group_by(Day) %>%
  summarize(MaxTair = max(Tair, na.rm = TRUE))

# Find the day with the highest max `Tair`
max_tair_day <- daily_max %>%
  filter(MaxTair == max(MaxTair)) %>%
  dplyr::select(Day, MaxTair)

# Calculate the 90th percentile for max `Tair`
percentile_90 <- quantile(daily_max$MaxTair, 0.90, na.rm = TRUE)

# Find days with max `Tair` in the 90th percentile and above
high_tair_days <- daily_max %>%
  filter(MaxTair >= percentile_90)

d <- results_df
d$day <- met_data$Day
d$it <- met_data$it

hottest_day_results <- d %>% dplyr::filter(day == max_tair_day$Day)
hottest_day_cooling <- mean(hottest_day_results$wbgt_usability_dif)

p90 <- d %>% dplyr::filter(day %in% high_tair_days$Day)
p90_cooling <- mean(p90$wbgt_usability_dif)

# if (type == "hottest"){
#   data <- hottest_day_results
# }
# if (type == "p90"){
#   data <- p90
# }
# if (type == "all"){
#   data <- d
# }
steps_per_day <- 13

compute_hourly_averages <- function(data){
  
  # Initialize vector to store averages for the current month
  averages <- numeric(steps_per_day)
  
  # Compute averages for each time step
  for (i in 1:steps_per_day) {
    # Get time steps for the current step across the days in the month
    current_step_indices <- data$timestep[data$timestep %% steps_per_day == i %% steps_per_day]
    
    # Calculate mean for current time step
    if (length(current_step_indices) > 0) {
      averages[i] <- mean(data$wbgt_usability_dif[data$timestep %in% current_step_indices], na.rm = TRUE)
    } else {
      # Handle cases where there are no entries for a specific time step
      averages[i] <- NA
    }
  }
  
  return(averages)
}

hottest_hourly <- compute_hourly_averages(hottest_day_results)
p90_hourly <- compute_hourly_averages(p90)
all_hourly <- compute_hourly_averages(d)

# Create a vector for the x-axis (time steps 1 to 13)
x_values <- 1:steps_per_day
# 
# Define custom x-axis labels
time_labels <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm")


plot(seq(1:steps_per_day),
     hottest_hourly,
     type = "l", col = "red", lty = 2,
     xlab = "Time of Day",
     ylim = c(0,1),
     ylab = "Average Change in Usable Fraction", 
     xaxt = "n")
lines(seq(1:steps_per_day), p90_hourly, col = "blue", lty = 2)
lines(seq(1:steps_per_day), all_hourly, col = "black", lty = 2)

title <- "Cooling Enhances Play Area Usability"
mtext(side=3, line=2, at=0, adj=0, cex=1.5, title)
mtext(side=3, line=1, at=0, adj=0, cex=1, subtitle)

legend_position <- "topright"

# Add a legend
legend(legend_position,
       legend = c("Hottest Day",
                  "90th+ Percentile Days",
                  "All Days"),
       col = c("red","blue","black"),
       lty = 2)

# plot(hottest_day_results$timestep,
#      hottest_day_results$wbgt_usability_dif,
#      type = "l", col = "red", lty = 2,
#      xlab = "Time of Day",
#      ylim = c(-1,9),
#      ylab = "Average Decrease in MRT (°C)", 
#      main = "Average Cooling of Renovated Area Due to Increased Shade",
#      xaxt = "n")

# Customize x-axis with time labels
time_labels <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm")
axis(1, at = x_values, labels = time_labels, las = 2)

rect(xleft = 2 , ybottom = par("usr")[3], 
     xright = 2 + 0.25, ytop = par("usr")[4], 
     col = rgb(0.7, 1, 1, 0.5), border = NA)

rect(xleft = 2 + 40/60 , ybottom = par("usr")[3], 
     xright = 2 + 55/60, ytop = par("usr")[4], 
     col = rgb(1, 1, .7, 0.5), border = NA)

rect(xleft = 3 + 25/60 , ybottom = par("usr")[3], 
     xright = 3 + 40/60, ytop = par("usr")[4], 
     col = rgb(1, .7, 1, 0.5), border = NA)

rect(xleft = 4 + 10/60, ybottom = par("usr")[3], 
     xright = 4 + 50/60, ytop = par("usr")[4], 
     col = rgb(0.7, 1, 1, 0.5), border = NA)

rect(xleft = 5 , ybottom = par("usr")[3], 
     xright = 5 + 40/60, ytop = par("usr")[4], 
     col = rgb(1, 1, .7, 0.5), border = NA)

rect(xleft = 5 + 40/60 , ybottom = par("usr")[3], 
     xright = 6 + 20/60, ytop = par("usr")[4], 
     col = rgb(1, .7, 1, 0.5), border = NA)

text_vertical_position <- 0.6

# Add the vertical label "recess" inside the shaded rectangle
text(x = 2 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
     labels = "TK-1st recess", cex = 1.2, font = 2, srt = 90)

# Add the vertical label "recess" inside the shaded rectangle
text(x = 2 + 40/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
     labels = "2nd-3rd recess", cex = 1.2, font = 2, srt = 90)

# Add the vertical label "recess" inside the shaded rectangle
text(x = 3 + 25/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
     labels = "4th-5th recess", cex = 1.2, font = 2, srt = 90)

# Add the vertical label "recess" inside the shaded rectangle
text(x = 4 + 10/60 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
     labels = "TK-1st lunch", cex = 1.2, font = 2, srt = 90)

# Add the vertical label "recess" inside the shaded rectangle
text(x = 5 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
     labels = "2nd-3rd lunch", cex = 1.2, font = 2, srt = 90)

# Add the vertical label "recess" inside the shaded rectangle
text(x = 5 + 40/60 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
     labels = "4th-5th lunch", cex = 1.2, font = 2, srt = 90)

}