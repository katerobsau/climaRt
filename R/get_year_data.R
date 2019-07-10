#' Filters the station data by the given year
#'
#' @param stn_data A data frame of station data
#' @param year_val A numeric value for the year that will be used to filter
#' the data
#'
#' @return only the station data for the given year
#'
get_year_data <- function(stn_data, year_val){

  year_data <- stn_data %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::filter(year == year_val)

  return(year_data)

}
