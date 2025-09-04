make_monthly_plot <- function(site, year, surface, position, scenario){

  if (year == 0){
    if (surface == 1){
      subtitle <- ("Only surface changes")
    }
    if (surface == 0){
      subtitle <- ("No changes")
    }
  }
  else {
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
                              position, "_",
                              scenario, "_results.rds"))
  # 
  results_dfx <- results_df %>%
    mutate(delta_mean_mrt = baseline_mean_mrt - future_mean_mrt) %>%
    mutate(delta_mean_wbgt = baseline_mean_wbgt - future_mean_wbgt)
  # 
  #results_dfx <- hottest_day_results
  
  # plot(results_df$timestep,results_df$baseline_mean_mrt,pch='.',
  #      xlab = "model timestep (hours)",
  #      ylab = "Mean Radiant Temperature (°C)",
  #      main = "Los Angeles Site")
  # lines(results_df$timestep,results_df$baseline_mean_mrt,col='black')
  # lines(results_df$timestep,results_df$future_mean_mrt,col='blue')
  # 
  # plot(results_df$timestep,results_df$baseline_mean_wbgt,pch='.',
  #      xlab = "model timestep (hours)",
  #      ylab = "Wet Bulb Globe Temperature (°C)",
  #      main = "Los Angeles Site")
  # lines(results_df$timestep,results_df$baseline_mean_wbgt,col='black')
  # lines(results_df$timestep,results_df$future_mean_wbgt,col='blue')
  # abline(h=26.5, col='red')
  # 
  # plot(results_dfx$timestep,results_dfx$delta_mean_mrt,pch='.',
  #      xlab = "model timestep (hours)",
  #      ylab = "Mean Temperature Difference (°C)",
  #      main = "Los Angeles Site")
  # lines(results_dfx$timestep,results_dfx$delta_mean_mrt,col='orange')
  # lines(results_dfx$timestep,results_dfx$delta_mean_wbgt,col='green')
  # legend("topleft",legend = c("MRT","WBGT"),col=c("orange","green"),lty=1)
  # 
  # results_sept <- results_dfx[1886:2275,]#[2682:3012,] #need to check
  # day_labels <- 1:30
  # day_positions <- seq(2682, 3012, length.out = 30)
  # 
  # plot(results_sept$timestep,results_sept$baseline_mean_mrt,pch='.',
  #      xlab = "Day in September",
  #      ylab = "Mean Radiant Temperature (°C)",
  #      main = "Los Angeles Site: September",
  #      xaxt = "n")
  # lines(results_sept$timestep,results_sept$baseline_mean_mrt,col='black')
  # lines(results_sept$timestep,results_sept$future_mean_mrt,col='blue')
  # axis(1, at = day_positions, labels = day_labels)
  # 
  # plot(results_sept$timestep,results_sept$baseline_mean_wbgt,pch='.',
  #      xlab = "Day in September",
  #      ylab = "Wet Bulb Globe Temperature (°C)",
  #      main = "Los Angeles Site: September",
  #      xaxt = "n")
  # lines(results_sept$timestep,results_sept$baseline_mean_wbgt,col='black')
  # lines(results_sept$timestep,results_sept$future_mean_wbgt,col='blue')
  # axis(1, at = day_positions, labels = day_labels)
  # abline(h=26.5, col='red')
  # 
  # 
  # plot(results_sept$timestep,results_sept$delta_mean_mrt,pch='.',
  #      xlab = "Day in September",
  #      ylab = "Mean Temperature Decrease (°C)",
  #      main = "Los Angeles Site: September",
  #      ylim = c(0,6),
  #      xaxt = "n")
  # lines(results_sept$timestep,results_sept$delta_mean_mrt,col='orange')
  # lines(results_sept$timestep,results_sept$delta_mean_wbgt,col='green')
  # legend("topleft",legend = c("MRT","WBGT"),col=c("orange","green"),
  #        lty=1,horiz = TRUE)
  # axis(1, at = day_positions, labels = day_labels)
  # 
  # #### written together with ChatGPT
  # 
 # Define the number of time steps per day
  steps_per_day <- 13
  # 
  # # Initialize vectors to store averages
  # averages <- numeric(steps_per_day)
  # 
  # # Loop through each time step from 1 to 13
  # for (i in 1:steps_per_day) {
  #   # Find the time steps in the pattern n*13 + i
  #   indices <- seq(i, by = steps_per_day, length.out = length(results_dfx[ , 1]) %/% steps_per_day + 1)
  #   
  #   # Compute the average of foo for these time steps
  #   averages[i] <- mean(results_dfx$delta_mean_mrt[results_dfx$timestep %in% indices])
  # }
  # 
  
# Create a vector for the x-axis (time steps 1 to 13)
  x_values <- 1:steps_per_day
  # 
  # Define custom x-axis labels
 time_labels <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm")
  # 
  # # Plot the averages
  # plot(x_values, averages, type = "b", pch = 19, xaxt = "n", xlab = "Time of Day", ylab = "Decrease in Average Mean Radiant Temperature", 
  #      main = "Average Cooling by Time of Day")
  # 
  # # Customize x-axis with time labels
  # axis(1, at = x_values, labels = time_labels, las = 2)
  # 
  
  #### written together with ChatGPT
  
  # Define the number of time steps per day
  steps_per_day <- 13
  
  # Define parameters
  df <- results_dfx %>%
    mutate(timestep = timestep + steps_per_day*98) # the days before April 9
  
  # Define the start and end days for the relevant months
  month_days <- list(
    May = c(121, 151),      # May starts on the 121st day and ends on the 151st day
    June = c(152, 181),     # June starts on the 152nd day and ends on the 181st day
    July = c(182, 212),     # July starts on the 182nd day and ends on the 212th day
    August = c(213, 243),   # August starts on the 213th day and ends on the 243rd day
    September = c(244, 273) # September starts on the 244th day and ends on the 273rd day
  )
  months <- seq(1,5,1)
  
  # Initialize list to store averages for each month
  monthly_averages <- list()
  
  # Calculate averages for each month
  for (month in names(month_days)) {
    start_day <- month_days[[month]][1]
    end_day <- month_days[[month]][2]
    
    # Filter data for the current month
    df_month <- df[df$timestep >= start_day*steps_per_day -(steps_per_day-1) & df$timestep <= end_day*steps_per_day, ]
    
    # Check if df_month has any data
    if (nrow(df_month) == 0) {
      warning(paste("No data for", month, "in the given range."))
      next
    }
    
    # Initialize vector to store averages for the current month
    averages <- numeric(steps_per_day)
    
    # Compute averages for each time step
    for (i in 1:steps_per_day) {
      # Get time steps for the current step across the days in the month
      current_step_indices <- df_month$timestep[df_month$timestep %% steps_per_day == i %% steps_per_day]
      
      # Calculate mean for current time step
      if (length(current_step_indices) > 0) {
        averages[i] <- mean(df_month$delta_mean_mrt[df_month$timestep %in% current_step_indices], na.rm = TRUE)
      } else {
        # Handle cases where there are no entries for a specific time step
        averages[i] <- NA
      }
    }
    
    # Store the averages in the list
    monthly_averages[[month]] <- averages
  }
  
  # Check for any NaN values in averages
  #print(monthly_averages)
  
  # Define custom x-axis labels
  time_labels <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm")
  x_values <- 1:steps_per_day
  
  # Plot the averages
  plot(x_values, monthly_averages[[1]], type = "l", col = "red", lty = 2,
       xlab = "Time of Day",
       ylim = c(0,10),
       ylab = "Average Decrease in MRT (°C)", 
       xaxt = "n")
  
  title <- paste("Average Cooling at ",str_to_title(site))
  mtext(side=3, line=2, at=0, adj=0, cex=1.5, title)
  mtext(side=3, line=1, at=0, adj=0, cex=1, subtitle)
  
  # Add lines for the remaining months
  colors <- c("red", "orange", "green", "blue", "purple")
  for (i in 2:length(months)) {
    if (!is.null(monthly_averages[[months[i]]])) {
      lines(x_values, monthly_averages[[months[i]]], col = colors[i], lty = i+1)
    }
  }
  
  # Customize x-axis with time labels
  axis(1, at = x_values, labels = time_labels, las = 2)
  
  if (year < 15) {legend_position <- "topright"}
  else {legend_position <- "bottomright"}
  
  # Add a legend
  legend(legend_position,
         legend = names(month_days),
         col = colors,
         lty = 1:length(month_days) + 1)
  
  if (site == "los_angeles"){
    # Add a shaded rectangle for "recess" from 9am to 9:15am
    # 9am is the 2nd time step and 9:15am is just after the 2nd time step
    
    rect(xleft = 2 , ybottom = par("usr")[3],
         xright = 6 + 20/60, ytop = par("usr")[4],
         col = rgb(0.7, 1, 1, 0.5), border = NA)
    # 
    # rect(xleft = 2 , ybottom = par("usr")[3],
    #      xright = 2 + 0.25, ytop = par("usr")[4],
    #      col = rgb(0.7, 1, 1, 0.5), border = NA)
    # 
    # rect(xleft = 2 + 40/60 , ybottom = par("usr")[3],
    #      xright = 2 + 55/60, ytop = par("usr")[4],
    #      col = rgb(1, 1, .7, 0.5), border = NA)
    # 
    # rect(xleft = 3 + 25/60 , ybottom = par("usr")[3],
    #      xright = 3 + 40/60, ytop = par("usr")[4],
    #      col = rgb(1, .7, 1, 0.5), border = NA)
    # 
    # rect(xleft = 4 + 10/60, ybottom = par("usr")[3],
    #      xright = 4 + 50/60, ytop = par("usr")[4],
    #      col = rgb(0.7, 1, 1, 0.5), border = NA)
    # 
    # rect(xleft = 5 , ybottom = par("usr")[3],
    #      xright = 5 + 40/60, ytop = par("usr")[4],
    #      col = rgb(1, 1, .7, 0.5), border = NA)
    # 
    # rect(xleft = 5 + 40/60 , ybottom = par("usr")[3],
    #      xright = 6 + 20/60, ytop = par("usr")[4],
    #      col = rgb(1, .7, 1, 0.5), border = NA)
    # 
    # text_vertical_position <- 0.6
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 2 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position,
    #      labels = "TK-1st recess", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 2 + 40/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position,
    #      labels = "2nd-3rd recess", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 3 + 25/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position,
    #      labels = "4th-5th recess", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 4 + 10/60 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position,
    #      labels = "TK-1st lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 5 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position,
    #      labels = "2nd-3rd lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 5 + 40/60 + 20/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position,
    #      labels = "4th-5th lunch", cex = 1.2, font = 2, srt = 90)
  }
  
  if (site %in% c("corlears","vince")){
    # Add a shaded rectangle for "recess" from 9am to 9:15am
    # 9am is the 2nd time step and 9:15am is just after the 2nd time step
    
    
    rect(xleft = 3 + 35/60 , ybottom = par("usr")[3], 
         xright = 6 + 20/60, ytop = par("usr")[4], 
         col = rgb(0.7, 1, 1, 0.5), border = NA)  
    
    # rect(xleft = 3 + 35/60 , ybottom = par("usr")[3], 
    #      xright = 4 + 0/60, ytop = par("usr")[4], 
    #      col = rgb(0.7, 1, 1, 0.5), border = NA)
    # 
    # rect(xleft = 4 + 0/60 , ybottom = par("usr")[3], 
    #      xright = 4 + 25/60, ytop = par("usr")[4], 
    #      col = rgb(1, 1, .7, 0.5), border = NA)
    # 
    # rect(xleft = 4 + 35/60 , ybottom = par("usr")[3], 
    #      xright = 5 + 0/60, ytop = par("usr")[4], 
    #      col = rgb(1, .7, 1, 0.5), border = NA)
    # 
    # rect(xleft = 5 + 0/60, ybottom = par("usr")[3], 
    #      xright = 5 + 25/60, ytop = par("usr")[4], 
    #      col = rgb(0.7, 0.7, 1, 0.5), border = NA)
    # 
    # rect(xleft = 5 + 30/60, ybottom = par("usr")[3], 
    #      xright = 5 + 55/60, ytop = par("usr")[4], 
    #      col = rgb(0.7, 1, .7, 0.5), border = NA)
    # 
    # rect(xleft = 5 + 55/60 , ybottom = par("usr")[3], 
    #      xright = 6 + 20/60, ytop = par("usr")[4], 
    #      col = rgb(1, .7, 0.7, 0.5), border = NA)
    # 
    # text_vertical_position <- 0.6
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 3 + 35/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
    #      labels = "2nd grade lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 4 + 0/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
    #      labels = "5th grade lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 4 + 35/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
    #      labels = "Kindergarten lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 5 + 0/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
    #      labels = "4th grade lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 5 + 30/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
    #      labels = "1st grade lunch", cex = 1.2, font = 2, srt = 90)
    # 
    # # Add the vertical label "recess" inside the shaded rectangle
    # text(x = 5 + 55/60 + 7.5/60, y = par("usr")[3] + (par("usr")[4] - par("usr")[3]) * text_vertical_position, 
    #      labels = "3rd grade lunch", cex = 1.2, font = 2, srt = 90)
  }
}