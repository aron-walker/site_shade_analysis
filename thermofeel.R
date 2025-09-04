
# This code was originally written by Chloe Brimicombe et al. (2022) in Python
# Link: https://github.com/ecmwf/thermofeel/blob/master/thermofeel/thermofeel.py
# Translated to R by ChatGPT

kelvin_to_celsius <- function(temp_k) {
  return(temp_k - 273.15)
}

celsius_to_kelvin <- function(temp_c) {
  return(temp_c + 273.15)
}

fahrenheit_to_kelvin <- function(temp_f) {
  return((temp_f - 32) * 5/9 + 273.15)
}

kelvin_to_fahrenheit <- function(temp_k) {
  return((temp_k - 273.15) * 9/5 + 32)
}

scale_windspeed <- function(va, h) {
  c <- 1 / log10(10 / 0.01)
  c <- 0.333333333333
  vh <- va * log10(h / 0.01) * c
  
  return(vh)
}

calculate_wbgt <- function(t2_k, mrt, va, td_k) {
  
  mrt_k <- celsius_to_kelvin(mrt)
  bgt_k <- calculate_bgt(t2_k, mrt_k, va)
  bgt_c <- kelvin_to_celsius(bgt_k)
  
  rh <- calculate_relative_humidity_percent(t2_k, td_k)
  t2_c <- kelvin_to_celsius(t2_k)
  tw_k <- calculate_wbt(t2_k, rh)
  tw_c <- kelvin_to_celsius(tw_k)
  
  wbgt <- 0.7 * tw_c + 0.2 * bgt_c + 0.1 * t2_c
  #wbgt_k <- celsius_to_kelvin(wbgt)
  
  return(wbgt)
}

calculate_bgt <- function(t2_k, mrt, va) {
  
  v <- scale_windspeed(va, 1.1)  # Wind speed at 1.1m (level of the globe)
  
  d <- (1.1e8 * v^0.6) / (0.95 * 0.15^0.4)
  e <- -(mrt^4) - d * t2_k
  
  q <- 12 * e
  s <- 27 * (d^2)
  delta <- ((s + sqrt(s^2 - 4 * (q^3))) / 2)^(1 / 3)
  Q <- 0.5 * sqrt((1 / 3) * (delta + q / delta))
  
  bgt <- -Q + 0.5 * sqrt(-4 * (Q^2) + d / Q)
  
  return(bgt)
}

calculate_relative_humidity_percent <- function(t2_k, td_k) {
  
  t2_c <- kelvin_to_celsius(t2_k)
  td_c <- kelvin_to_celsius(td_k)
  
  # Saturated vapor pressure
  es <- 6.11 * 10.0^(7.5 * t2_c / (237.3 + t2_c))
  # Vapor pressure
  e <- 6.11 * 10.0^(7.5 * td_c / (237.3 + td_c))
  # Relative humidity
  rh <- (e / es) * 100
  
  return(rh)
}

calculate_wbt <- function(t2_k, rh) {
  kelvin_to_celsius <- function(temp_k) {
    return(temp_k - 273.15)
  }
  
  celsius_to_kelvin <- function(temp_c) {
    return(temp_c + 273.15)
  }
  
  t2_c <- kelvin_to_celsius(t2_k)
  tw <- (
    t2_c * atan(0.151977 * sqrt(rh + 8.313659)) +
      atan(t2_c + rh) -
      atan(rh - 1.676331) +
      0.00391838 * (rh)^(3 / 2) * atan(0.023101 * rh) -
      4.686035
  )
  tw_k <- celsius_to_kelvin(tw)
  
  return(tw_k)
}

calculate_saturation_vapour_pressure <- function(t2_k){

g = c(
  -2.8365744e3,
  -6.028076559e3,
  1.954263612e1,
  -2.737830188e-2,
  1.6261698e-5,
  7.0229056e-10,
  -1.8680009e-13,
  2.7150305)

ess = g[7] * log(t2_k)
for (i in 1:7){
  ess <- ess + g[i] * t2_k^(i - 2)
}

ess <- exp(ess) * 0.01  # hPa

return(ess)
}

calculate_utci_polynomial <- function(t2_k, mrt_c, va, td_k) {

  t2m <- kelvin_to_celsius(t2_k)
  rh_pc = calculate_relative_humidity_percent(t2_k, td_k)
  ehPa <- calculate_saturation_vapour_pressure(t2_k) * rh_pc / 100.0
  rh <- ehPa / 10.0  # water vapour pressure in kPa
  
  e_mrt <- mrt_c - t2m
  
  t2m2 <- t2m^2
  t2m3 <- t2m^3
  t2m4 <- t2m^4
  t2m5 <- t2m^5
  t2m6 <- t2m^6
  
  va2 <- va^2
  va3 <- va^3
  va4 <- va^4
  va5 <- va^5
  va6 <- va^6
  
  e_mrt2 <- e_mrt^2
  e_mrt3 <- e_mrt^3
  e_mrt4 <- e_mrt^4
  e_mrt5 <- e_mrt^5
  e_mrt6 <- e_mrt^6
  
  rh2 <- rh^2
  rh3 <- rh^3
  rh4 <- rh^4
  rh5 <- rh^5
  rh6 <- rh^6
  
  varh2 = va * rh2
  va2_rh = va2 * rh
  va2_e_mrt = va2 * e_mrt
  e_mrt_rh = e_mrt * rh
  e_mrt_rh2 = e_mrt * rh2
  e_mrt2_rh = e_mrt2 * rh
  e_mrt2_rh2 = e_mrt2 * rh2
  e_mrt_rh3 = e_mrt * rh3
  va_e_mrt = va * e_mrt
  va_e_mrt2 = va * e_mrt2
  va_rh = va * rh
  t2m_va = t2m * va
  e_mrt3_rh = e_mrt3 * rh
  e_mrt4_rh = e_mrt4 * rh
  
  utci <- (
    t2m
    + 6.07562052e-01
    + -2.27712343e-02 * t2m
    + 8.06470249e-04 * t2m2
    + -1.54271372e-04 * t2m3
    + -3.24651735e-06 * t2m4
    + 7.32602852e-08 * t2m5
    + 1.35959073e-09 * t2m6
    + -2.25836520e00 * va
    + 8.80326035e-02 * t2m * va
    + 2.16844454e-03 * t2m2 * va
    + -1.53347087e-05 * t2m3 * va
    + -5.72983704e-07 * t2m4 * va
    + -2.55090145e-09 * t2m5 * va
    + -7.51269505e-01 * va2
    + -4.08350271e-03 * t2m * va2
    + -5.21670675e-05 * t2m2 * va2
    + 1.94544667e-06 * t2m3 * va2
    + 1.14099531e-08 * t2m4 * va2
    + 1.58137256e-01 * va3
    + -6.57263143e-05 * t2m * va3
    + 2.22697524e-07 * t2m2 * va3
    + -4.16117031e-08 * t2m3 * va3
    + -1.27762753e-02 * va4
    + 9.66891875e-06 * t2m * va4
    + 2.52785852e-09 * t2m2 * va4
    + 4.56306672e-04 * va5
    + -1.74202546e-07 * t2m * va5
    + -5.91491269e-06 * va6
    + 3.98374029e-01 * e_mrt
    + 1.83945314e-04 * t2m * e_mrt
    + -1.73754510e-04 * t2m2 * e_mrt
    + -7.60781159e-07 * t2m3 * e_mrt
    + 3.77830287e-08 * t2m4 * e_mrt
    + 5.43079673e-10 * t2m5 * e_mrt
    + -2.00518269e-02 * va_e_mrt
    + 8.92859837e-04 * t2m * va_e_mrt
    + 3.45433048e-06 * t2m2 * va_e_mrt
    + -3.77925774e-07 * t2m3 * va_e_mrt
    + -1.69699377e-09 * t2m4 * va_e_mrt
    + 1.69992415e-04 * va2_e_mrt
    + -4.99204314e-05 * t2m * va2_e_mrt
    + 2.47417178e-07 * t2m2 * va2_e_mrt
    + 1.07596466e-08 * t2m3 * va2_e_mrt
    + 8.49242932e-05 * va3 * e_mrt
    + 1.35191328e-06 * t2m * va3 * e_mrt
    + -6.21531254e-09 * t2m2 * va3 * e_mrt
    + -4.99410301e-06 * va4 * e_mrt
    + -1.89489258e-08 * t2m * va4 * e_mrt
    + 8.15300114e-08 * va5 * e_mrt
    + 7.55043090e-04 * e_mrt2
    + -5.65095215e-05 * t2m * e_mrt2
    + -4.52166564e-07 * t2m * e_mrt2
    + 2.46688878e-08 * t2m3 * e_mrt2
    + 2.42674348e-10 * t2m4 * e_mrt2
    + 1.54547250e-04 * va_e_mrt2
    + 5.24110970e-06 * t2m * va_e_mrt2
    + -8.75874982e-08 * t2m2 * va_e_mrt2
    + -1.50743064e-09 * t2m3 * va_e_mrt2
    + -1.56236307e-05 * va2 * e_mrt2
    + -1.33895614e-07 * t2m * va2 * e_mrt2
    + 2.49709824e-09 * t2m2 * va2 * e_mrt2
    + 6.51711721e-07 * va3 * e_mrt2
    + 1.94960053e-09 * t2m * va3 * e_mrt2
    + -1.00361113e-08 * va4 * e_mrt2
    + -1.21206673e-05 * e_mrt3
    + -2.18203660e-07 * t2m * e_mrt3
    + 7.51269482e-09 * t2m2 * e_mrt3
    + 9.79063848e-11 * t2m3 * e_mrt3
    + 1.25006734e-06 * va * e_mrt3
    + -1.81584736e-09 * t2m_va * e_mrt3
    + -3.52197671e-10 * t2m2 * va * e_mrt3
    + -3.36514630e-08 * va2 * e_mrt3
    + 1.35908359e-10 * t2m * va2 * e_mrt3
    + 4.17032620e-10 * va3 * e_mrt3
    + -1.30369025e-09 * e_mrt4
    + 4.13908461e-10 * t2m * e_mrt4
    + 9.22652254e-12 * t2m2 * e_mrt4
    + -5.08220384e-09 * va * e_mrt4
    + -2.24730961e-11 * t2m_va * e_mrt4
    + 1.17139133e-10 * va2 * e_mrt4
    + 6.62154879e-10 * e_mrt5
    + 4.03863260e-13 * t2m * e_mrt5
    + 1.95087203e-12 * va * e_mrt5
    + -4.73602469e-12 * e_mrt6
    + 5.12733497e00 * rh
    + -3.12788561e-01 * t2m * rh
    + -1.96701861e-02 * t2m2 * rh
    + 9.99690870e-04 * t2m3 * rh
    + 9.51738512e-06 * t2m4 * rh
    + -4.66426341e-07 * t2m5 * rh
    + 5.48050612e-01 * va_rh
    + -3.30552823e-03 * t2m * va_rh
    + -1.64119440e-03 * t2m2 * va_rh
    + -5.16670694e-06 * t2m3 * va_rh
    + 9.52692432e-07 * t2m4 * va_rh
    + -4.29223622e-02 * va2_rh
    + 5.00845667e-03 * t2m * va2_rh
    + 1.00601257e-06 * t2m2 * va2_rh
    + -1.81748644e-06 * t2m3 * va2_rh
    + -1.25813502e-03 * va3 * rh
    + -1.79330391e-04 * t2m * va3 * rh
    + 2.34994441e-06 * t2m2 * va3 * rh
    + 1.29735808e-04 * va4 * rh
    + 1.29064870e-06 * t2m * va4 * rh
    + -2.28558686e-06 * va5 * rh
    + -3.69476348e-02 * e_mrt_rh
    + 1.62325322e-03 * t2m * e_mrt_rh
    + -3.14279680e-05 * t2m2 * e_mrt_rh
    + 2.59835559e-06 * t2m3 * e_mrt_rh
    + -4.77136523e-08 * t2m4 * e_mrt_rh
    + 8.64203390e-03 * va * e_mrt_rh
    + -6.87405181e-04 * t2m_va * e_mrt_rh
    + -9.13863872e-06 * t2m2 * va * e_mrt_rh
    + 5.15916806e-07 * t2m3 * va * e_mrt_rh
    + -3.59217476e-05 * va2 * e_mrt_rh
    + 3.28696511e-05 * t2m * va2 * e_mrt_rh
    + -7.10542454e-07 * t2m2 * va2 * e_mrt_rh
    + -1.24382300e-05 * va3 * e_mrt_rh
    + -7.38584400e-09 * t2m * va3 * e_mrt_rh
    + 2.20609296e-07 * va4 * e_mrt_rh
    + -7.32469180e-04 * e_mrt2_rh
    + -1.87381964e-05 * t2m * e_mrt2_rh
    + 4.80925239e-06 * t2m2 * e_mrt2_rh
    + -8.75492040e-08 * t2m3 * e_mrt2_rh
    + 2.77862930e-05 * va * e_mrt2_rh
    + -5.06004592e-06 * t2m_va * e_mrt2_rh
    + 1.14325367e-07 * t2m2 * va * e_mrt2_rh
    + 2.53016723e-06 * va2 * e_mrt2_rh
    + -1.72857035e-08 * t2m * va2 * e_mrt2_rh
    + -3.95079398e-08 * va3 * e_mrt2_rh
    + -3.59413173e-07 * e_mrt3_rh
    + 7.04388046e-07 * t2m * e_mrt3_rh
    + -1.89309167e-08 * t2m2 * e_mrt3_rh
    + -4.79768731e-07 * va * e_mrt3_rh
    + 7.96079978e-09 * t2m_va * e_mrt3_rh
    + 1.62897058e-09 * va2 * e_mrt3_rh
    + 3.94367674e-08 * e_mrt4_rh
    + -1.18566247e-09 * t2m * e_mrt4_rh
    + 3.34678041e-10 * va * e_mrt4_rh
    + -1.15606447e-10 * e_mrt5 * rh
    + -2.80626406e00 * rh2
    + 5.48712484e-01 * t2m * rh2
    + -3.99428410e-03 * t2m2 * rh2
    + -9.54009191e-04 * t2m3 * rh2
    + 1.93090978e-05 * t2m4 * rh2
    + -3.08806365e-01 * varh2
    + 1.16952364e-02 * t2m * varh2
    + 4.95271903e-04 * t2m2 * varh2
    + -1.90710882e-05 * t2m3 * varh2
    + 2.10787756e-03 * va2 * rh2
    + -6.98445738e-04 * t2m * va2 * rh2
    + 2.30109073e-05 * t2m2 * va2 * rh2
    + 4.17856590e-04 * va3 * rh2
    + -1.27043871e-05 * t2m * va3 * rh2
    + -3.04620472e-06 * va4 * rh2
    + 5.14507424e-02 * e_mrt_rh2
    + -4.32510997e-03 * t2m * e_mrt_rh2
    + 8.99281156e-05 * t2m2 * e_mrt_rh2
    + -7.14663943e-07 * t2m3 * e_mrt_rh2
    + -2.66016305e-04 * va * e_mrt_rh2
    + 2.63789586e-04 * t2m_va * e_mrt_rh2
    + -7.01199003e-06 * t2m2 * va * e_mrt_rh2
    + -1.06823306e-04 * va2 * e_mrt_rh2
    + 3.61341136e-06 * t2m * va2 * e_mrt_rh2
    + 2.29748967e-07 * va3 * e_mrt_rh2
    + 3.04788893e-04 * e_mrt2_rh2
    + -6.42070836e-05 * t2m * e_mrt2_rh2
    + 1.16257971e-06 * t2m2 * e_mrt2_rh2
    + 7.68023384e-06 * va * e_mrt2_rh2
    + -5.47446896e-07 * t2m_va * e_mrt2_rh2
    + -3.59937910e-08 * va2 * e_mrt2_rh2
    + -4.36497725e-06 * e_mrt3 * rh2
    + 1.68737969e-07 * t2m * e_mrt3 * rh2
    + 2.67489271e-08 * va * e_mrt3 * rh2
    + 3.23926897e-09 * e_mrt4 * rh2
    + -3.53874123e-02 * rh3
    + -2.21201190e-01 * t2m * rh3
    + 1.55126038e-02 * t2m2 * rh3
    + -2.63917279e-04 * t2m3 * rh3
    + 4.53433455e-02 * va * rh3
    + -4.32943862e-03 * t2m_va * rh3
    + 1.45389826e-04 * t2m2 * va * rh3
    + 2.17508610e-04 * va2 * rh3
    + -6.66724702e-05 * t2m * va2 * rh3
    + 3.33217140e-05 * va3 * rh3
    + -2.26921615e-03 * e_mrt_rh3
    + 3.80261982e-04 * t2m * e_mrt_rh3
    + -5.45314314e-09 * t2m2 * e_mrt_rh3
    + -7.96355448e-04 * va * e_mrt_rh3
    + 2.53458034e-05 * t2m_va * e_mrt_rh3
    + -6.31223658e-06 * va2 * e_mrt_rh3
    + 3.02122035e-04 * e_mrt2 * rh3
    + -4.77403547e-06 * t2m * e_mrt2 * rh3
    + 1.73825715e-06 * va * e_mrt2 * rh3
    + -4.09087898e-07 * e_mrt3 * rh3
    + 6.14155345e-01 * rh4
    + -6.16755931e-02 * t2m * rh4
    + 1.33374846e-03 * t2m2 * rh4
    + 3.55375387e-03 * va * rh4
    + -5.13027851e-04 * t2m_va * rh4
    + 1.02449757e-04 * va2 * rh4
    + -1.48526421e-03 * e_mrt * rh4
    + -4.11469183e-05 * t2m * e_mrt * rh4
    + -6.80434415e-06 * va * e_mrt * rh4
    + -9.77675906e-06 * e_mrt2 * rh4
    + 8.82773108e-02 * rh5
    + -3.01859306e-03 * t2m * rh5
    + 1.04452989e-03 * va * rh5
    + 2.47090539e-04 * e_mrt * rh5
    + 1.48348065e-03 * rh6
  )
  
  return(utci)
}