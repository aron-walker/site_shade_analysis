countdown_timer <- function(seconds) {
  while (seconds > 0) {
    # Calculate minutes and seconds
    mins <- seconds %/% 60
    secs <- seconds %% 60
    
    # Print the countdown
    cat(sprintf("\rTime remaining: %02d:%02d", mins, secs))
    
    # Wait for 1 second
    Sys.sleep(1)
    
    # Decrease the counter
    seconds <- seconds - 1
  }
}

convert_to_time <- function(hours) {
  # Ensure hours is a vector of numeric values
  if (!is.numeric(hours)) {
    stop("Input must be numeric.")
  }
  
  # Convert each hour to a time string with leading zeros
  time_strings <- sprintf("%02d:00", hours)
  
  return(time_strings)
}

day_of_year_to_date <- function(day_of_year) {
  # Define the number of days in each month for a non-leap year
  # Can also change displayed names of months here
  days_in_month <- c(Jan = 31, Feb = 28, Mar = 31, Apr = 30, May = 31, Jun = 30,
                     Jul = 31, Aug = 31, Sep = 30, Oct = 31, Nov = 30, Dec = 31)
  
  # Validate input
  if (!is.numeric(day_of_year) || day_of_year < 1 || day_of_year > 365) {
    stop("day_of_year must be a numeric value between 1 and 365.")
  }
  
  # Find the month and day corresponding to the given day of the year
  cumulative_days <- cumsum(days_in_month)
  month_index <- which(day_of_year <= cumulative_days)[1]
  if (month_index == 1) {
    day_of_month <- day_of_year
  } else {
    day_of_month <- day_of_year - cumulative_days[month_index - 1]
  }
  
  # Convert to "Month, Day" format
  month_name <- names(days_in_month)[month_index]
  formatted_date <- sprintf("%s %02d", month_name, day_of_month)
  
  return(formatted_date)
}