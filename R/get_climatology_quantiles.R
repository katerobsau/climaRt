#' Returns the quantiels from the period of reference climatology for temperature.
#' The quantiles are defined relative to the difference between TMIN and TMAX.
#'
#' @param stn_data A data frame of station data, must include variables;
#' date, tmax and tmin. Output of function \code{get_station_data()}.
#' @param last_year The numeric value for the year that ends of the
#' cliamte reference period. We will estimate the quantiles for the
#' reference climatology from this period.
#' If not provided the default value is the last year on the record.
#' @param break_vals A vector given the quantiles to estimate using the
#' reference climatology.
#' These values will correspond directly to a colour scale later.
#' (Default is seq(0,1,0.025))
#' These values control the smoothness of colour scale, but be warned too many
#' breaks will make the plot object needlessly complex.
#'
#' @return Returns empirical quantiles for \code{(tmax - tmin)/2 + tmin}
#' for the reference climatology period
#'
get_climatology_quantiles <- function(stn_data, last_year,
                                      break_vals = seq(0,1,0.025)){

  if(missing(last_year)){
    last_year = max(stn_data$date %>% lubridate::year(), na.rm = TRUE)
  }

  mid_quantiles = stn_data %>%
    dplyr::filte(year = lubridate::year(date))
    dplyr::filter(year < last_year) %>%
    dplyr::mutate(tmid = (tmax - tmin)/2 + tmin) %>%
    dplyr::select("tmid") %>%
    unlist() %>%
    quantile(probs = break_vals, na.rm = TRUE) %>%
    as.numeric()

  return(mid_quantiles)

}
