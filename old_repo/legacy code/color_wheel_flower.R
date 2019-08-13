# set us a colour palette
temp_colors <- c("darkturquoise", "deepskyblue1", "deepskyblue2", "deepskyblue3",  "maroon1", "firebrick2")
pal_len = 100
temp_ramp <- colorRampPalette(temp_colors)
temp_palette <- temp_ramp(pal_len)

# get quantiles for temperatures pre 1960
# tq = c(0, 0.01, 0.1, 0.25, 0.4, 0.6, 0.75, 0.9, 0.99, 1)
# tlabels = c("extremely cold", "very cold", "cold", "cool", "normal", "warm", "hot",  "very hot", "extremely hot")
break_vals = seq(0,1,0.025)

max_ecdf = stn_data %>% 
  dplyr::filter(year < 1960) %>%
  dplyr::select("tmax") %>%
  unlist() %>%
  ecdf()

max_quantiles = stn_data %>% 
  dplyr::filter(year < 1960) %>%
  dplyr::select("tmax") %>%
  unlist() %>%
  quantile(probs = break_vals, na.rm = TRUE) %>%
  as.numeric()

min_ecdf = stn_data %>% 
  dplyr::filter(year < 1960) %>%
  dplyr::select("tmin") %>%
  unlist() %>%
  ecdf()

min_quantiles = stn_data %>% 
  dplyr::filter(year < 1960) %>%
  dplyr::select("tmin") %>%
  unlist() %>%
  quantile(probs = break_vals, na.rm = TRUE) %>%
  as.numeric()

mid_quantiles = stn_data %>% 
  dplyr::filter(year < 1960) %>%
  dplyr::mutate(tmid = (tmax - tmin)/2 + tmin) %>%
  dplyr::select("tmid") %>%
  unlist() %>%
  quantile(probs = break_vals, na.rm = TRUE) %>%
  as.numeric()

mid_ecdf = stn_data %>% 
  dplyr::filter(year < 1960) %>%
  dplyr::mutate(tmid = (tmax - tmin)/2 + tmin) %>%
  dplyr::select("tmid") %>%
  unlist() %>%
  ecdf()

# assign factors to stn data
# test = cut(x = seq(0, 1, by = 0.01), breaks = tq, labels = tlabels)
# factor_data <- stn_data %>%
#   mutate(max_factor = cut(x = tmax, breaks = max_quantiles)) %>%
#   mutate(min_factor = cut(x = tmin, breaks = min_quantiles))

year_val = 1910
plot_data <- stn_data %>%
  dplyr::filter(year == year_val) 

base_plot <- ggplot(data = plot_data) 
col_plot <- base_plot
palette_len = length(temp_palette)
break_len = length(break_vals)
mid_quantiles = c(-10^6, mid_quantiles, 10^6) # extend for later climatology
num_intervals = length(mid_quantiles) - 1
for(i in 1:num_intervals){
  col_plot <- add_plot_segments(col_plot, df = plot_data, 
                              y_min = mid_quantiles[i], y_max = mid_quantiles[i+1], 
                                col_val = temp_palette[i/num_intervals * palette_len],
                              tmin_min = tmin_min, tmax_max = tmax_max,
                              white_shift = 0.25)
}

temp_plot <- col_plot

rf_circle = 0.4
print("Should scale it relative to the quantile where most of the rain occurs")
rain_plot <- temp_plot +
    geom_point(aes(x = as.factor(angle), y = rf_circle + white_shift, size = rf_scale),
             col = "lightgoldenrod2", alpha = 0.5, shape = 20) +
    scale_size_continuous(range = c(0, 10)) 
# rain_plot

flower_plot <- rain_plot +
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
    coord_polar(start = 0) 

flower_plot
