# Function to calculate dew point temperature
calculate_dew_point <- function(Ta, RH) {
  # Calculate saturation vapor pressure
  E_s <- 6.112 * exp((17.67 * Ta) / (Ta + 243.5))
  # Calculate actual vapor pressure
  E_a <- (RH / 100) * E_s
  # Calculate dew point temperature
  Tdew <- (243.5 * log(E_a / 6.112)) / (17.67 - log(E_a / 6.112))
  return(Tdew)
}