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
