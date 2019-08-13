#' Adds the rainfall circles to the temperature bar plot
#'
#' @param temp_plot ggplot object returend by \code{create_temperature_bar_plot}
#' @param rf_circle a numeric value that controls the height at which the rainfall
#' circles are plotted against the temperature bars (default is 0.4)
#' @param white_shift a numeric value that can shift the height of the circles.
#' (default is 0) This parameter is slightly different from the rf_circle as it
#' relates to the shifting of the entire plot, not just the circles.
#' @param rf_col colour for plotting the rainfall circles (default is
#' "lightgoldenrod2")
#' @param rf_alpha numeric value between 0 and 1 giving the transparency of the circles
#' (default is 0.5)
#'
#' @return returns the input of \code{temp_plot} with circles added to show rainfall
#'
#' @examples
#' set.seed(1)
#' tmin = sin(seq(0, 2*pi, length.out = 360)) + rnorm(360, 20, 3)
#' rf_scale = rep(NA,360)
#' samp = sample(1:360,20)
#' rf_scale[samp] = runif(20, 0, 5)
#' plot_data = data.frame(angle = 1:360,
#'     tmin = tmin,
#'     tmax = tmin + 10,
#'     rf_scale = rf_scale)
#' mid_quantiles = seq(18, 28, length.out = 10)
#' tmin_min = min(plot_data$tmin) - 1
#' tmax_max = max(plot_data$tmax) + 1
#' temp_plot = create_temperature_bar_plot(plot_data, mid_quantiles,
#'    tmin_min, tmax_max)
#' add_rf_circles(temp_plot)
add_rf_circles <- function(temp_plot, rf_circle = 0.4, white_shift = 0,
                           rf_col = "lightgoldenrod2",  rf_alpha = 0.5){

  rain_plot <- temp_plot +
    geom_point(aes(x = as.factor(angle), y = rf_circle + white_shift,
                   size = rf_scale),
               col = rf_col, alpha = rf_alpha, shape = 20) +
    scale_size_continuous(range = c(0,10))

  return(rain_plot)

}

