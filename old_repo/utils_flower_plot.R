create_colour_palette <- function(colors, pal_len = 100){
  if(missing(colors))
    colors <- c("darkturquoise", "deepskyblue", "deepskyblue1","deepskyblue1", "deepskyblue2", "deepskyblue2", "deepskyblue3", "maroon1", "firebrick1")
  temp_ramp <- colorRampPalette(colors)
  temp_palette <- temp_ramp(pal_len)
  return(temp_palette)
}

add_temperature_bars <- function(base_plot, df, y_min, y_max, col_val, 
                              tmin_min, tmax_max, white_shift = 0){
  
  t_range = tmax_max - tmin_min
  
  plot_seg <- df %>%
    dplyr::select(tmax, tmin) %>%
    dplyr::mutate(y_beg = case_when(is.na(tmin) ~ NA_real_,
                                    is.na(tmax) ~ NA_real_,
                                    tmin >= y_min & tmin < y_max ~ ((tmin - tmin_min)/t_range), # inside the interval
                                    tmin <= y_min & tmax > y_min ~ ((y_min - tmin_min)/t_range) # before the interval
    )) %>%
    dplyr::mutate(y_end = case_when(is.na(tmin) ~ NA_real_,
                                    is.na(tmax) ~ NA_real_,
                                    tmax >= y_min & tmax < y_max ~ ((tmax - tmin_min)/t_range), # inside the interval
                                    tmin <= y_max & tmax > y_max ~ ((y_max - tmin_min)/t_range) # after the interval
    ))
  
  y = plot_seg %>% dplyr::select(y_beg) %>% unlist() %>% as.numeric()
  y_end = plot_seg %>% dplyr::select(y_end) %>% unlist() %>% as.numeric()
  
  y = y + white_shift
  y_end = y_end + white_shift
  
  base_plot <- base_plot +
    geom_segment(stat ="identity", aes(x= as.factor(angle), y = y,
                                       xend = as.factor(angle), yend = y_end
    ), col = col_val)
  
  return(base_plot)

}

add_rf_circles <- function(temp_plot, rf_circle = 0.4, white_shift = 0, 
                           rf_col = "lightgoldenrod2",  rf_alpha = 0.5, rf_size_range = c(0, 10)){

  # print("Should scale it relative to the quantile where most of the rain occurs")
  rain_plot <- temp_plot +
    geom_point(aes(x = as.factor(angle), y = rf_circle + white_shift, size = rf_scale),
             col = rf_col, alpha = rf_alpha, shape = 20) +
    scale_size_continuous(range = rf_size_range) 
  
  return(rain_plot)
  
}

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

add_centre_text <- function(flower_plot, stn_name, year_val){
  text_item = c(stn_name, year_val)
  # text_item = c("      ART + \n CLIMATE = \n  CHANGE", year_val)
  text_info <- data.frame(x = c(0, 0), y = c(0,0), text_item,
                          hjust = c(0.75, 0.75), vjust = c(0, 2))
  text_col <- c("darkturquoise", "darkgray")
  
  finished_plot <- flower_plot + 
    geom_text(data = text_info, aes(x = x, y = y, label = text_item, 
                                    color = text_item, 
                                    hjust = hjust, vjust = vjust), size = 5) +
    scale_color_manual(values = text_col)
}
