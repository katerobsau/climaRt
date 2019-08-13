# bar colour examples

# set us a colour palette
temp_colors <- c("darkturquoise", "deepskyblue", "deepskyblue1", "deepskyblue2", "deepskyblue3", 
                "maroon1", "maroon2", "maroon3", "firebrick2", "firebrick3")

temp_colors <- c("darkturquoise", "deepskyblue1",  "maroon1", "firebrick2")
pal_len = 10
temp_palette <- colorRampPalette(temp_colors)(pal_len)
# check it looks good
test_df <- data.frame(x = rep(1:pal_len, each = pal_len), 
                              y = as.factor(rep(1:pal_len, times = pal_len)))

ggplot(test_df, aes(x)) + 
  geom_bar(aes(fill = y)) +
  scale_fill_manual(values = temp_palette) + 
  coord_polar()

# see if we can implement on an interval of 0 to 1
bar_test_df <- data.frame(date = 1:4, 
                          bar_min = c(0.1, 0.2, 0.3,0.4), 
                          bar_max = c(0.9, 0.95, 0.99, 0.8))
test_col <- apply(bar_test_df, 1, function(r, bar_len){
  # bar_vals = colorRampPalette(temp_colors)(bar_len)
  bar_vals = rep(1, bar_len)
  tmin_bar = floor(r[2]*bar_len) 
  if(tmin_bar >= 1){
    tmin_ind = 1:tmin_bar
  }else{
    tmin_ind = NULL
  }
  # tmax_bar = ceiling(r[3]*bar_len) 
  # if(tmax_bar <= bar_len){
  #   tmax_ind = tmax_bar:bar_len
  # }else{
  #   tmin_ind = NULL
  # }
  # bar_vals[c(tmin_ind, tmax_ind)] = "#ffffff"
  bar_vals[tmin_ind] = 0
  bar_vals = bar_vals
  return(bar_vals)
}, bar_len = bar_len) %>% 
  as.data.frame()

names(test_col) = bar_test_df$date

plot_col <- test_col %>%
  tidyr::gather(key = "date", value = "col_scale", 1:ncol(test_col))

# plot_vals = c("#ffffff", colorRampPalette(temp_colors)(bar_len), "#ffffff")[bar_len:1]
ggplot(plot_col, aes(date)) +
  geom_bar(aes(fill = as.factor(col_scale))) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(values = c("white", "red")) +
  coord_polar()

# plot_vals = c("#ffffff", colorRampPalette(temp_colors)(bar_len), "#ffffff")[bar_len:1]
# ggplot(plot_col, aes(date)) + 
#   geom_bar(aes(fill = as.factor(col_scale))) +
#   scale_fill_manual(values = plot_vals) + 
#   coord_polar() 
  
## ---------------------------------------------------------------------------------------

bar_test_df <- data.frame(date = 1:4, 
                          empty_min = c(0.1, 0.2, 0.3, 0.4),
                          temp_bar = c(0.8, 0.7, 0.6, 0.4), 
                          empty_max = c(0.1, 0.1, 0.1, 0.2))
test_col <- mapply(function(empty_min, temp_bar, empty_max){
  cols = c(rep("a", empty_min*10), rep("b", temp_bar*10), rep("c", empty_max*10))
}, empty_min = c(0.1, 0.2, 0.3, 0.4), 
temp_bar = c(0.8, 0.7, 0.6, 0.4),
empty_max = c(0.1, 0.1, 0.1, 0.2)) %>% 
  as.data.frame()
names(test_col) <- bar_test_df$date

plot_col <- test_col %>%
  tidyr::gather(key = "date", value = "col_scale", 1:ncol(test_col))

ggplot(plot_col, aes(date)) +
  geom_bar(aes(fill = as.factor(col_scale))) +
  coord_polar()

## -------------------------------------

library(tidyverse)
library(forcats)
library(vcd)

ggplot(data = Arthritis, aes(x = Treatment)) +
  geom_bar(aes(fill = Improved), position = "fill")
  