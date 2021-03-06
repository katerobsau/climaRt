% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_centre_text.R
\name{add_centre_text}
\alias{add_centre_text}
\title{Adds text to the centre of the circular bar plot. (Haven't automated
the text positioning here well. Would like to update this function
in future.)}
\usage{
add_centre_text(flower_plot, stn_name, year_val)
}
\arguments{
\item{flower_plot}{ggplot object that is a circular bar plot,
output of \code{make_flower()}}

\item{stn_name}{string giving the corresponding station name}

\item{year_val}{double giving the corresponding year}
}
\value{
the flower_plot object with additional text in the centre
}
\description{
Adds text to the centre of the circular bar plot. (Haven't automated
the text positioning here well. Would like to update this function
in future.)
}
\examples{
set.seed(1)
tmin = sin(seq(0, 2*pi, length.out = 360)) + rnorm(360, 20, 3)
rf_scale = rep(NA,360)
samp = sample(1:360,20)
rf_scale[samp] = runif(20, 0, 5)
plot_data = data.frame(angle = 1:360,
    tmin = tmin,
    tmax = tmin + 10,
    rf_scale = rf_scale)
mid_quantiles = seq(18, 28, length.out = 10)
tmin_min = min(plot_data$tmin) - 1
tmax_max = max(plot_data$tmax) + 1
temp_plot = create_temperature_bar_plot(plot_data, mid_quantiles,
   tmin_min, tmax_max)
climate_plot = add_rf_circles(temp_plot)
flower_plot <- make_flower(climate_plot)
add_centre_text(flower_plot,
 stn_name = "Lorem Ipsum",
   year_val = 9999)

}
