library(rnoaa)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

setwd("/Users/katesaunders/Documents/Git/ClimateChangeVsArt")
source("utils_get_data.R")
source("utils_data_preprocessing.R")
source("utils_flower_plot.R")

# get data automatically from server
country_code = "NL"
lat_lon_df <- data.frame(id = "debilt",#"melbourne",
                         latitude = 52.10, #-37.8136,
                         longitude = 5.18)# 144.9631)
station_meta_data <- ghcnd_meta_data
stn_data <- get_station_data(station_meta_data, lat_lon_df,
                             search_country_code = country_code)

# get quantiles from climatology
break_vals = seq(0,1,0.025)
mid_quantiles <- get_climatology_quantiles(stn_data = stn_data %>%
                                             dplyr::mutate(year = year(date)),
                                           last_year = 1960,
                                           break_vals = break_vals)

# add rainfall scaling for later plotting
stn_data_with_scaled_rf <- add_scaled_rainfall(stn_data)

# get a years worth of data
# year_val = 1910

#Creating countdown .png files from 10 to "GO!"
# png(file="example%02d.png", width = 325, height = 325)

for(year_val in 1910:2014){

year_data <- get_year_data(stn_data_with_scaled_rf, year_val)

# add angle for plotting
plot_data <- add_angle(year_data)
if(nrow(plot_data) > 366){
  warning("There is more than one years worth of data")
}

# create a base plot
base_plot <- ggplot(data = plot_data)

# define necessary variables for plotting
white_shift = 0.25
color_palette = create_colour_palette()
tmin_min = stn_data$tmin %>% min(na.rm = T)
tmax_max = stn_data$tmax %>% max(na.rm = T)

# add temperature bars
temp_plot <- base_plot
palette_len = length(color_palette)
break_len = length(break_vals)
plot_quantiles = c(-10^6, mid_quantiles, 10^6) # extend for later climatology
num_intervals = length(plot_quantiles) - 1
for(i in 1:num_intervals){
  temp_plot <- add_temperature_bars(temp_plot, df = plot_data,
                                y_min = plot_quantiles[i], y_max = plot_quantiles[i+1],
                                col_val = color_palette[i/num_intervals * palette_len],
                                tmin_min = tmin_min, tmax_max = tmax_max,
                                white_shift = white_shift)
}
# temp_plot

# rf_circle = (mid_quantiles[length(mid_quantiles)/2] - tmin_min)/(tmax_max - tmin_min)
rf_circle = white_shift
climate_plot <- add_rf_circles(temp_plot, rf_circle = rf_circle, white_shift = white_shift)
# climate_plot

flower_plot <- make_flower(climate_plot, white_shift = white_shift)
#flower_plot

finished_plot <- add_centre_text(flower_plot,
                                 stn_name = lat_lon_df$id %>% as.character(),
                                 year_val = year_val)
print(finished_plot)

}

# dev.off()

#
# # Converting .png files in one .gif image using ImageMagick
# system("convert -delay 80 *.png example_1.gif")
# # Remove .png files from working directory
# file.remove(list.files(pattern=".png"))
