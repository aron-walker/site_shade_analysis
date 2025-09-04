make_summary_plot <- function(site, year, surface, position){
  
  if (year == 0){
    if (surface == 1){
      subtitle <- ("Only surface changes")
    }
    if (surface == 0){
      subtitle <- ("No changes")
    }
  } else {
    if (surface == 0) {
      surface_text <- " only (no surface change)"}
    if (surface == 1) {
      surface_text <- " and surface changes"
    }
    subtitle <- paste0(year, " years of canopy growth", surface_text)
  }
  
  results_df <- readRDS(paste0(local_path,site,"/intermediates/y",
                               year, "_",
                               surface, "_",
                               position, "_results.rds"))
  
  results_dfx <- results_df %>%
    mutate(delta_mean_mrt = baseline_mean_mrt - future_mean_mrt) %>%
    mutate(delta_mean_wbgt = baseline_mean_wbgt - future_mean_wbgt)
  
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
  
  d <- results_dfx %>% 
    dplyr::select(c(timestep, baseline_mean_mrt,future_mean_mrt,delta_mean_mrt))
  
  d$day <- met_data$Day
  d$it <- met_data$it
  
  hottest_day_results <- d %>% dplyr::filter(day == max_tair_day$Day)
  hottest_day_cooling <- mean(hottest_day_results$delta_mean_mrt)
  
  p90 <- d %>% dplyr::filter(day %in% high_tair_days$Day)
  p90_cooling <- mean(p90$delta_mean_mrt)
  
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
        averages[i] <- mean(data$delta_mean_mrt[data$timestep %in% current_step_indices], na.rm = TRUE)
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
       ylim = c(-1,12),
       ylab = "Average Decrease in MRT (°C)", 
       xaxt = "n")
  lines(seq(1:steps_per_day), p90_hourly, col = "blue", lty = 2)
  lines(seq(1:steps_per_day), all_hourly, col = "black", lty = 2)
  
  title <- "Average Cooling of Renovated Area"
  mtext(side=3, line=2, at=0, adj=0, cex=1.5, title)
  mtext(side=3, line=1, at=0, adj=0, cex=1, subtitle)
  
  if (year < 14) {legend_position <- "topright"}
  if (year > 14) {legend_position <- "bottomright"}
  
  # Add a legend
  legend(legend_position,
         legend = c("Hottest Day",
                    "90th+ Percentile Days",
                    "All Days"),
         col = c("red","blue","black"),
         lty = 2)
  
  # plot(hottest_day_results$timestep,
  #      hottest_day_results$delta_mean_mrt,
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
  
  # Add the vertical label "recess" inside the shaded rectangle
  text(x = 2 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2, 
       labels = "TK-1st recess", cex = 1.2, font = 2, srt = 90)
  
  # Add the vertical label "recess" inside the shaded rectangle
  text(x = 2 + 40/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2, 
       labels = "2nd-3rd recess", cex = 1.2, font = 2, srt = 90)
  
  # Add the vertical label "recess" inside the shaded rectangle
  text(x = 3 + 25/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2, 
       labels = "4th-5th recess", cex = 1.2, font = 2, srt = 90)
  
  # Add the vertical label "recess" inside the shaded rectangle
  text(x = 4 + 10/60 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2, 
       labels = "TK-1st lunch", cex = 1.2, font = 2, srt = 90)
  
  # Add the vertical label "recess" inside the shaded rectangle
  text(x = 5 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2, 
       labels = "2nd-3rd lunch", cex = 1.2, font = 2, srt = 90)
  
  # Add the vertical label "recess" inside the shaded rectangle
  text(x = 5 + 40/60 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * 0.2, 
       labels = "4th-5th lunch", cex = 1.2, font = 2, srt = 90)
  
}

## Code for finding and interpolating missing met data

# # Determine the minimum and maximum values in the "timestep" column
# min_timestep <- min(d$timestep)
# max_timestep <- max(d$timestep)
# 
# # Create the full sequence of expected values
# full_sequence <- seq(min_timestep, max_timestep)
# 
# # Find the missing timestep by comparing the full sequence with the actual values
# missing_timestep <- setdiff(full_sequence, d$timestep)
# 
# # Display the missing timestep
# print(missing_timestep)
# 
# # Define the rows
# row_above <- c(2023, 191, 15, 0, -999, 5.99, 3.6, -999, -999, 0.67, 81.99, 17.63, 100.49, 0, 246.81, -999, 320.86, -999, -999, -999, -999, -999, -999, -999, -999)
# row_below <- c(2023, 191, 17, 0, -999, 153.28, 30.4, -999, -999, 1.12, 73.84, 19.62, 100.54, 0, 651.39, -999, 331.61, -999, -999, -999, -999, -999, -999, -999, -999)
# 
# # Define a function to interpolate values
# interpolate <- function(val_above, val_below) {
#     return((val_above + val_below) / 2)
#   }
# 
# # Apply interpolation
# missing_row <- mapply(interpolate, row_above, row_below)
# 
# formatted_missing_row <- paste(sprintf("%.3f", missing_row), collapse = " ")
# 
# # Display the formatted row
# cat(formatted_missing_row)