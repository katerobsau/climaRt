get_all_station_meta_data <- function(){
  
  # country_names <- ghcnd_countries()
  print("WARNING: Takes 1 minute to get the stations")
  station_meta_data <- rnoaa::ghcnd_stations()
  return(station_meta_data)
  
}

get_station_data <- function(station_meta_data, lat_lon_df, search_country_code = "AS", 
                             record_len = 100, radius = 10){
  
  if(nrow(lat_lon_df) != 1){
    stop("Error: Only queries one station at a time!")
  }
  # filter stations by country and record length
  filtered_meta_data <- station_meta_data %>%
    dplyr::mutate(country_code = substr(id, 1, 2)) %>%
    dplyr::filter(country_code == search_country_code) %>%
    dplyr::filter(last_year - first_year > record_len)

  nearby_stations <- rnoaa::meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                         station_data = filtered_meta_data, 
                                         radius = radius)

  nearby_stn_df <- nearby_stations %>% as.data.frame()
  names(nearby_stn_df) = names(nearby_stations[[1]])
  
  print("WARNING: Just taking the closest station")
  print("WARNING: Not doing anything clever with matching TMAX, TMIN and PRCP")
  print(nearby_stn_df[1,])
  stn_id = nearby_stn_df$id[1] %>% as.character()
  
  stn_var <- filter(filtered_meta_data, id == stn_id)
  elements <- stn_var$element %>% sort()
  if(!all(elements == c("PRCP", "TMAX", "TMIN"))){
    stop("Missing necessary variables at station")
  }

  raw_stn_data <- rnoaa::meteo_pull_monitors(monitors = stn_id)

  # variable stored within 10ths of a degree or tenths of a mm
  stn_data <- raw_stn_data %>%
    dplyr::mutate(tmax = tmax/10) %>%
    dplyr::mutate(tmin = tmin/10) %>%
    dplyr::mutate(prcp = prcp/10)

  return(stn_data)
  
}
