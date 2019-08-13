get_climatology_quantiles <- function(stn_data, last_year, break_vals){
  
  print(paste("Using climatology from earliest year to", last_year))
  print("Quantiles are defined relative to the difference between TMIN and TMAX")
  warning("Break values controls smoothness of colour scale, but too breaks many will the plot object complex.")
  
  mid_quantiles = stn_data %>% 
    dplyr::filter(year < last_year) %>%
    dplyr::mutate(tmid = (tmax - tmin)/2 + tmin) %>%
    dplyr::select("tmid") %>%
    unlist() %>%
    quantile(probs = break_vals, na.rm = TRUE) %>%
    as.numeric()
  
  return(mid_quantiles)
}

get_year_data <- function(stn_data, year_val){
  
  year_data <- stn_data %>% 
    dplyr::mutate(year = year(date)) %>%
    dplyr::filter(year == year_val) 
  
  return(year_data)
  
}

add_angle <- function(df){
  
  # get angle for circular plotting
  df <- df %>% 
    mutate(year_day = yday(date)) %>%
    mutate(year = year(date)) %>%
    mutate(angle = if_else(year%%4 == 0, 
                           year_day/366*360, 
                           year_day/365*360))
  return(df)
  
}

add_scaled_rainfall <- function(stn_data){
  max_rf = stn_data$prcp %>% max(na.rm = TRUE) 
  print("Note: currently ignoring multiday totals, these are stored in different element")
  
  stn_data <- stn_data %>% 
    dplyr::mutate(prcp = if_else(prcp == 0, NA_real_, prcp)) %>%
    dplyr::mutate(rf_scale = prcp^(2)/max_rf^(2)) 
  
  return(stn_data)
}