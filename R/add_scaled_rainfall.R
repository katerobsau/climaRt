#' Adds a new column to the station data of the rainfall.
#' This column rescales the rainfall data relative to the
#' maximum rainfall on record. This rescale is for plotting later.
#'
#' @param stn_data A data frame of station data, must include the variable
#' prcp (precipitation)
#' @param power The power we use to rescale the rainfall (Default = 2)
#'
#' @return station data with an addition column of scaled rainfall
#'
add_scaled_rainfall <- function(stn_data, power = 2){

  max_rf = stn_data$prcp %>% max(na.rm = TRUE)

  warning("Currently ignoring multiday totals, these are stored in different element")

  stn_data <- stn_data %>%
    dplyr::mutate(prcp = if_else(prcp == 0, NA_real_, prcp)) %>%
    dplyr::mutate(rf_scale = prcp^(power)/max_rf^(power))

  return(stn_data)

}
