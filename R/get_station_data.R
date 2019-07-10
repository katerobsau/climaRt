#' Gets the station data for the station closest to the given longitude and latitude
#'
#' @param ghcnd_meta_data Data frame that contains the station meta data
#' for the Global Historical Climatology Network Daily (GHCND). The most up
#' to date version of this meta data can be returned from function
#' \code{rnoaa::ghncd_stations}. However, we have also stored a copy
#' as a data object to save time.
#' @param lat_lon_df Similar to the input for lat_lon_df from
#' \code{rnoaa::rnoaa::meteo_nearby_stations}. A single row dataframe that
#' contains the
#' latitude, longitude, and a unique identifier for each location (id). For
#' an example of the proper format for this dataframe, see the example
#' below. Latitude and longitude must both be in units of decimal degrees.
#'Southern latitudes and Western longitudes should be given as negative values.
#' @param search_country_code Two letter string representing the country/state
#' code used in GHCN Daily.
#' For Australia the code is "AS". For a full list of country codes refer to
#' the function \code{rnoaa::ghcnd_countries} or \code{rnoaa::ghncd_states}
#' @param record_len Minimum record length considered. To compare plots over
#' multiple years we scale the observations relative to mean climate.
#' We recommend using a station with a longer record length (Default is 100).
#' @param radius A numeric value giving the radius in kilometres to search
#' for the closest station to the given longitude and latitude. (Default is 10).
#' (Similar to the input for radius from
#' \code{rnoaa::meteo_nearby_stations})
#'
#' @return Returns a dataframe with columns id, date, and the relevant
#' climate variables for precipitation and temperature.
#'
#' @examples
#' lat_lon_df <- data.frame(id = "melbourne",
#'                         latitude = -37.8136,
#'                         longitude = 144.9631)
#'
#' ghcnd_meta_data <- data("ghcnd_meta_data", package = "climaRt")
#'
#' stn_data <- get_station_data(ghcnd_meta_data, lat_lon_df,
#'                   search_country_code = "AS)
#'
get_station_data <- function(ghncd_meta_data, lat_lon_df,
                             search_country_code,
                             record_len = 100, radius = 10){

  if(nrow(lat_lon_df) != 1){
    stop("Error: Only queries one station at a time!")
  }

  # filter stations by country and record length
  filtered_meta_data <- ghcnd_meta_data %>%
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

  stn_var <- dplyr::filter(filtered_meta_data, id == stn_id)
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
