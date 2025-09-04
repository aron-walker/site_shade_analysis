if (site == "chicago"){
  model_surface_changes <- 1
  first_day_of_school_2023_2024 <- 233 # Aug 21; source: https://www.cps.edu/globalassets/cps-pages/calendar/cps-2023-2024-calendar_english-1.pdf
  last_day_of_school_2022_2023 <- 158 # June 7; source: https://www.cps.edu/globalassets/cps-pages/calendar/cps-2022-2023-calendar-english.pdf
  time_zone_offset <- 5 # vs. UTC
  frost_free_days <- 197 # source: https://www.almanac.com/gardening/frostdates/IL/Chicago
  threshold_classification <- 2 # source: # http://www.castlewilliams.com/wbgt-regions.html
}

if (site == "chicago_add_canopies"){
  model_surface_changes <- 1
  first_day_of_school_2023_2024 <- 233 # Aug 21; source: https://www.cps.edu/globalassets/cps-pages/calendar/cps-2023-2024-calendar_english-1.pdf
  last_day_of_school_2022_2023 <- 158 # June 7; source: https://www.cps.edu/globalassets/cps-pages/calendar/cps-2022-2023-calendar-english.pdf
  time_zone_offset <- 5 # vs. UTC
  frost_free_days <- 197 # source: https://www.almanac.com/gardening/frostdates/IL/Chicago
  threshold_classification <- 2 # source: # http://www.castlewilliams.com/wbgt-regions.html
}

# Los Angeles

if (site == "los_angeles"){
  model_surface_changes <- 1
  first_day_of_school_2023_2024 <- 226 # Aug 14; source: 
  last_day_of_school_2022_2023 <- 166 # June 7; source: https://www.eaglerockhsptsa.org/2022/04/29/lausd-board-approved-2022-2023-calendar/
  time_zone_offset <- 7 # vs. UTC
  frost_free_days <- 365
  threshold_classification <- 1
}

# Los Angeles

if (site == "los_angeles_big_trees"){
  model_surface_changes <- 1
  first_day_of_school_2023_2024 <- 226 # Aug 14; source: 
  last_day_of_school_2022_2023 <- 166 # June 7; source: https://www.eaglerockhsptsa.org/2022/04/29/lausd-board-approved-2022-2023-calendar/
  time_zone_offset <- 7 # vs. UTC
  frost_free_days <- 365
  threshold_classification <- 1
}

# New York

if (site == "corlears"){
  model_surface_changes <- 0
  first_day_of_school_2023_2024 <- 250 # 
  last_day_of_school_2022_2023 <- 178 # 
  time_zone_offset <- 4 # vs. UTC
  frost_free_days <- 224 # New York (https://www.almanac.com/gardening/frostdates/NY/New%20York)
  threshold_classification <- 1 # source: # http://www.castlewilliams.com/wbgt-regions.html
}

if (site == "vince"){
  model_surface_changes <- 1
  first_day_of_school_2023_2024 <- 250 # 
  last_day_of_school_2022_2023 <- 178 # 
  time_zone_offset <- 4 # vs. UTC
  frost_free_days <- 224 # New York (https://www.almanac.com/gardening/frostdates/NY/New%20York)
  threshold_classification <- 1 # source: # http://www.castlewilliams.com/wbgt-regions.html
}