library(rnoaa)
country_names <- ghcnd_countries()
print("WARNING: Takes 1 minute")
station_data <- ghcnd_stations()

# filter stations by country and record length
record_len = 100
search_country_code <- "AS"
country_station_data <- station_data %>%
  mutate(country_code = substr(id, 1, 2)) %>%
  filter(country_code == search_country_code) %>%
  filter(last_year - first_year > record_len)

lat_lon_df <- data.frame(id = c("sydney", "brisbane", "melbourne"),
                         latitude = c(-33.8675, -27.4710, -37.8136),
                         longitude = c(151.2070, 153.0234, 144.9631))

nearby_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                         station_data = country_station_data, 
                                         radius = 10)

stn_id <- nearby_stations$melbourne$id[1]

stn_var <- filter(country_station_data, id == stn_id)
elements <- stn_var$element %>% sort()
if(!all(elements == c("PRCP", "TMAX", "TMIN"))){
  stop("Missing necessary variables at station")
}

raw_stn_data <- meteo_pull_monitors(monitors = stn_id)

# variable stored within 10ths of a degree or tenths of a mm
stn_data <- raw_stn_data %>%
  mutate(tmax = tmax/10) %>%
  mutate(tmin = tmin/10) %>%
  mutate(prcp = prcp/10)

# get angle for circular plotting
stn_data <- stn_data %>% 
  mutate(year_day = yday(date)) %>%
  mutate(year = year(date)) %>%
  mutate(angle = if_else(year%%4 == 0, 
                         year_day/366*360, 
                         year_day/365*360))

# get mean (mean) temp for pre 1960
circle_quantile = 0.5
circle_temp = stn_data %>% 
  dplyr::mutate(mid = tmax - tmin) %>%
  dplyr::filter(year < 1960) %>%
  dplyr::select("mid") %>%
  unlist() %>%
  quantile(probs = circle_quantile, na.rm = TRUE) %>%
  as.numeric()

# get temp range 
tmin_min = stn_data$tmin %>% min(na.rm = T)
tmax_max = stn_data$tmax %>% max(na.rm = T)
t_range = (tmax_max - tmin_min)
stn_data <- stn_data %>%
  mutate(tmax_scale = (tmax - tmin_min)/t_range) %>%
  mutate(tmin_scale = (tmin - tmin_min)/t_range) 

# get scaling for rainfall points
rf_circle = 0.25
max_rf = stn_data$prcp %>% max(na.rm = TRUE) 
  #multiday totals are stored in differnet elements
stn_data <- stn_data %>% 
  mutate(prcp = if_else(prcp == 0, NA_real_, prcp)) %>%
  mutate(rf_scale = prcp^(2)/max_rf^(2)) 
# plot(stn_data$rf_scale, type = "h")

# filter by plot year
# year_val = 1920
for(year_val in seq(1910, 1950, 2010)){

plot_data <- stn_data %>%
  filter(year == year_val)

p <- ggplot(data = plot_data) +      
  # geom_bar(stat="identity",  aes(x=as.factor(angle), y= tmax_scale, fill= tmax_scale)) +
  # geom_bar(stat="identity",  aes(x=as.factor(angle), y = tmin_scale), fill = "white") +
  geom_segment(stat="identity",  aes(x= as.factor(angle), y = tmin_scale, 
                                     xend = as.factor(angle), yend = tmax_scale, 
                                     col = tmax_scale)) +
  scale_color_gradient(low = "cyan", high = "magenta", limits = c(0, 1)) +
  geom_point(aes(x = as.factor(angle), y = rf_circle, size = rf_scale),
             col = "blue", alpha = 0.25, shape = 20) +
  scale_size_continuous(range = c(0, 10)) +
  # ggtitle(year) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm"),
    legend.position = "none"
  ) +
  ylim(c(-0.1, 1.1)) +
  coord_polar(start = 0) 
  

print(p)

}

p + geom_text(label = paste(stn_var$name %>% unique(), year_val), aes(x = 0, y = 1))
