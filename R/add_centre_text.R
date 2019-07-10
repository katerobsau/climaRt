#' Adds text to the centre of the circular bar plot. (Haven't automated
#' the text positioning here well. Would like to update this function
#' in future.)
#'
#' @param flower_plot ggplot object that is a circular bar plot,
#' output of \code{make_flower()}
#' @param stn_name string giving the corresponding station name
#' @param year_val double giving the corresponding year
#'
#' @return the flower_plot object with additional text in the centre
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
#' flower_plot <- make_flower(climate_plot)
#' add_centre_text(flower_plot,
#'  stn_name = "Lorem Ipsum",
#'    year_val = 9999)
#'
add_centre_text <- function(flower_plot, stn_name, year_val){

  text_item = c(stn_name, year_val)
  # text_item = c("      ART + \n CLIMATE = \n  CHANGE", year_val)
  text_info <- data.frame(x = c(0, 0), y = c(0,0), text_item,
                          hjust = c(0.75, 0.75), vjust = c(0, 2))
  text_col <- c("darkturquoise", "darkgray")

  finished_plot <- flower_plot +
    geom_text(data = text_info, aes(x = x, y = y, label = text_item,
                                    color = text_item,
                                    hjust = hjust, vjust = vjust),
              size = 5) +
    scale_color_manual(values = text_col)

  return(finished_plot)

}
