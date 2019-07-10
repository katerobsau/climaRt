#' Adds a new column called angle. The angle is given by
#' $$ angle  = year di(# days in year)*360$$
#' This anlge is for plotting later.
#'
#' @param stn_data A data frame of station data, must include the variable
#' date
#'
#' @return station data with an addition column called angle
#'
add_angle <- function(stn_data){

  # get angle for circular plotting
  stn_data <- stn_data %>%
    mutate(year_day = yday(date)) %>%
    mutate(year = year(date)) %>%
    mutate(angle = if_else(year%%4 == 0,
                           year_day/366*360,
                           year_day/365*360))
  return(stn_data)

}
