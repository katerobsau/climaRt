library(climaRt)
library(patchwork)
library(zoo)

country_code = "NL" # "AS
station_meta_data <- ghcnd_meta_data

denhelder_lat_lon <- data.frame(id = "denhelder", #id = "debilt",
                         latitude = 52.9563, #52.10,
                         longitude = 4.7608) #5.18)
denhelder_data <- get_station_data(station_meta_data,
                                   denhelder_lat_lon,
                             search_country_code = country_code)
denhelder_rolled <- denhelder_data %>%
  mutate(ROLLMEAN = rollmean(tmax, 3, fill = "left"))

debilt_lat_lon <- data.frame(id = "debilt",
                         latitude = 52.10,
                         longitude = 5.18)
debilt_data <- get_station_data(station_meta_data, debilt_lat_lon,
                             search_country_code = country_code)
debilt_rolled <- debilt_data %>%
  mutate(ROLLMEAN = rollmean(tmax, 3, fill = "left"))

stn_data <- rbind(debilt_rolled %>% mutate(STN = "De Bilt"),
                  denhelder_rolled %>% mutate(STN = "Den Helder"))

am_data <- stn_data %>%
  mutate(YEAR = lubridate::year(date)) %>%
  group_by(YEAR, STN) %>%
  summarise(AM5 = max(ROLLMEAN, na.rm = TRUE),
            AM  = max(tmax, na.rm = TRUE),
            count = sum(!is.na(tmax))) %>%
  ungroup()
print("Acutally am3 not am5")

# colour scale
blue_range = c("navyblue", "lightcyan")
red_range = c("red3", "mistyrose")
blues_pal <- create_colour_palette(colors = blue_range, pal_len = 85)
reds_pal <- create_colour_palette(colors = red_range, pal_len = 100)
full_pal <- c(blues_pal, rev(reds_pal))

# plot stripes
debilt_stripes <- ggplot(data = am_data %>% filter(STN == "De Bilt")) +
  geom_col(aes(x = YEAR, y = 0.5, fill = AM5, col = NULL)) +
  scale_fill_gradientn("AM3", colors = full_pal) +
  xlim(c(1900, 2019)) +
  ggtitle("De Bilt") +
   # scale_fill_distiller(palette = "RdYlBu") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

# plot stripes
denhelder_stripes <- ggplot(data = am_data %>% filter(STN == "Den Helder")) +
  geom_col(aes(x = YEAR, y = 0.5, fill = AM5, col = NULL)) +
  scale_fill_gradientn("AM3", colors = full_pal) +
  xlim(c(1900, 2019)) +
  ggtitle("Den Helder") +
  # scale_fill_distiller(palette = "RdYlBu") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank())

debilt_stripes / denhelder_stripes

### -------------------------------------------------------------------------------

am_data_wider = am_data %>%
  select(YEAR,STN,AM5) %>%
  filter(!is.na(AM5)) %>%
  filter(AM5 != -Inf) %>%
  pivot_wider(values_from = c("AM5"),  names_from = c("STN"))

num_rows = 15
tau_vals <- lapply(1:nrow(am_data_wider), function(i, df, num_rows, min_years){
  slice_df <- df  %>%
    slice(i:(i + num_rows - 1)) %>%
    filter(!is.na(rowSums(.)))
  if(nrow(slice_df) < min_years) return(NA)
  kendall_test = cor.test(x= slice_df %>% pull(1),
                          y =slice_df %>% pull(2),
                          method = "kendall", na.rm = TRUE)
  tau = kendall_test$estimate
  return(tau)
}, df = am_data_wider %>% select(-YEAR), num_rows = num_rows, min_years = 15) %>%
  unlist()
tau_df = am_data_wider %>% mutate(tau =tau_vals)

# plot stripes
tau_stripes <- ggplot(data = tau_df) +
  geom_col(aes(x = YEAR + num_rows/2, y = 0.5, fill = tau, col = NULL)) +
  scale_fill_gradientn("tau", colors = full_pal) +
  xlim(c(1900, 2019)) +
  ggtitle("Tau3") +
  # scale_fill_distiller(palette = "RdYlBu") +
  theme_minimal() +
  theme(axis.title = element_blank())
tau_stripes

# tau <-  t  au_stripes
tau /tau_stripes
# tau3 <- tau_stripes

### -------------------------------------------------------------------------------

window = 20
theta_df <- NULL
for(start_year in unique(am_data$YEAR)){

# filter by window
window_data <- am_data %>%
  mutate(AM = if_else(AM == -Inf, NA_real_, AM)) %>%
  pivot_wider(values_from = AM, names_from = STN) %>%
  filter(YEAR >= start_year & YEAR < start_year + window - 1) %>%
  dplyr::select(-YEAR)

# na values
num_common = sum(rowSums(!is.na(window_data)) == 2)
if(num_common <= 1){ next }

# convert to ecdf
window_ecdf <- apply(window_data, 2, function(x) ecdf(x)(x))

# get distance
# theta_val = (apply(window_data,1,diff) %>% abs() %>% sum())/(2*num_common)
theta_val = cor(window_data, method="kendall", use="pairwise")[1,2]
df = data.frame(YEAR = start_year + window/2,
                THETA = theta_val,
                OBS = num_common)
theta_df <- rbind(theta_df, df)

}

# estimator adjustement
min_years = 15
plot_theta <- theta_df %>%
  mutate(THETA_EST = if_else(OBS > min_years, THETA, NA_real_))
  # mutate(THETA_EST = if_else(THETA > 2, 2, THETA_EST)) %>%
  # mutate(THETA_EST = if_else(THETA < 1, 1, THETA_EST)) %>%


# plot stripes
theta_stripes <- ggplot(data = plot_theta) +
  geom_col(aes(x = YEAR, y = 0.5, fill = THETA_EST, col = NULL)) +
  scale_fill_gradientn(colors = full_pal) +
  # xlim(c(1900, 2019)) +
  ggtitle("Kendall's Tau") +
  xlim(1900, 2019) +
  # scale_fill_distiller(palette = "RdYlBu") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank())

theta_stripes

debilt_stripes / denhelder_stripes / theta_stripes
### Extremal coefficient
get_fmado_dist <- function(x){
  Nnb = ncol(x)
  Tnb = nrow(x)
  V = array(NaN, dim = c(Tnb,Nnb))
  for(p in 1:Nnb) {
    x.vec = as.vector(x[,p])
    if(all(is.na(x.vec))){
      V[,p] = x.vec
      next
    }
    Femp = ecdf(x.vec)(x.vec)
    V[,p] = Femp
  }
  DD_fmado = dist(t(V), , diag = TRUE, upper = TRUE)/(2*Tnb)
  return(DD_fmado)
}
#
#
# get_fmado_dist(x = data.frame(am_data$AM ))
