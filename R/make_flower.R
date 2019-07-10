#' Takes a bar plot of the climate and
#' wraps the plot around a circle to make a circular bar plot.
#'
#' @param climate_plot ggplot object, that can be an output of either
#' \code{create_temperature_bar_plot} or \code{add_rf_circles}
#' @param white_shift double for vertical translation relative to the
#' circle radius (default 0)
#' @param start double that rotates which date occurs at 12'oclock, (default of 0)
#'
#' @return the climate_plot object as a circular bar plot. Axes are
#' removed
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
#' climate_plot = add_rf_circles(temp_plot)
#' make_flower(climate_plot)
#'
make_flower <- function(climate_plot, white_shift = 0, start = 0){
  flower_plot <- climate_plot +
    # ggtitle(year) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-2,4), "cm"),
      legend.position = "none"
    ) +
    ylim(c(-0.1, 1.1 + white_shift)) +
    coord_polar(start = start)
  return(flower_plot)
}
