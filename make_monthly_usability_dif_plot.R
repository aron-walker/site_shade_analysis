make_monthly_usability_dif_plot <- function(site,year,
                                            surface,
                                            position,
                                            scenario,
                                            threshold_level,
                                            boundary_type,
                                            name){

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
} else {
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
                 scenario, "_",
                 threshold*100, "_", boundary_type, ".rds")) %>%
    unlist() %>%
    replace_na(0) %>%
    return()

}

baseline <- get_wbgt_usability(0,1,position,thresholds[threshold_level]) / 100
future <- get_wbgt_usability(year,surface,position,thresholds[threshold_level]) / 100
# surface_0 <- get_wbgt_usability(0,1,1,thresholds[i]) / 100
# trees_5 <- get_wbgt_usability(5,0,1,thresholds[i]) / 100
# both_5 <- get_wbgt_usability(5,1,1,thresholds[i]) / 100
# trees_10 <- get_wbgt_usability(10,0,1,thresholds[i]) / 100
# both_10 <- get_wbgt_usability(10,1,1,thresholds[i]) / 100
# trees_15 <- get_wbgt_usability(15,0,1,thresholds[i]) / 100
# both_15 <- get_wbgt_usability(15,1,1,thresholds[i]) / 100
# trees_20 <- get_wbgt_usability(20,0,1,thresholds[i]) / 100
# both_20 <- get_wbgt_usability(20,1,1,thresholds[i]) / 100

dif <- baseline - future

steps_per_day <- 13
#### written together with ChatGPT

# Define the number of time steps per day

results_df <- data.frame(seq(1,length(baseline)),dif)
colnames(results_df) <- c("timestep", "wbgt_usability_dif")

# Define parameters
df <- results_df %>%
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
      averages[i] <- mean(df_month$wbgt_usability_dif[df_month$timestep %in% current_step_indices], na.rm = TRUE)
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
write.csv(monthly_averages,paste0("/Users/aronwalker/Desktop/v2_",name,".csv"))
# Plot the averages
plot(x_values, monthly_averages[[1]]*100, type = "l", col = "red", lty = 2,
     xlab = "Time of Day",
     ylim = c(0,12),
     ylab = "Average Change in Usable Percent", 
     xaxt = "n")

title <- paste0("Usability Enhancement at ",name)
mtext(side=3, line=2, at=0, adj=0, cex=1.5, title)
mtext(side=3, line=1, at=0, adj=0, cex=1, subtitle)

# Add lines for the remaining months
colors <- c("red", "orange", "green", "blue", "purple")
for (i in 2:length(months)) {
  if (!is.null(monthly_averages[[months[i]]])) {
    lines(x_values, monthly_averages[[months[i]]]*100, col = colors[i], lty = i+1)
  }
}

# Customize x-axis with time labels
axis(1, at = x_values, labels = time_labels, las = 2)

legend_position <- "topright"

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
  
#   rect(xleft = 2 , ybottom = par("usr")[3],
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

# text_vertical_position <- 0.6

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