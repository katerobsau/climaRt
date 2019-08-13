#' Add a new column called angle
#'
#' Adds a new column called angle to the input data frame. The angle is given by
#' (day in year)/(# days in year)*360.
#' This angle is used later in plotting functions.
#'
#' @param stn_data A data frame of station data, must include the variable
#' date
#'
#' @return returns the \code{stn_data} input with an addition column called angle
#'
#' @examples
#' date_seq = seq(lubridate::as_date("2019-01-01"),
#'                lubridate::as_date("2019-01-31"),
#'                by = "days")
#' stn_data = data.frame(date = date_seq)
#' add_angle(stn_data)
#'
add_angle <- function(stn_data){

  # get angle for circular plotting
  stn_data <- stn_data %>%
    dplyr::mutate(year_day = lubridate::yday(date)) %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::mutate(angle = if_else(year%%4 == 0,
                           year_day/366*360,
                           year_day/365*360)) %>%
    dplyr::select(-year, -year_day)

  return(stn_data)

}
