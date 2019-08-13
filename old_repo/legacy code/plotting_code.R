library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)

# ---------------------------------------------------------------------------------

working_dir = "Documents/Git/ClimateChangeVsArt/"

# ---------------------------------------------------------------------------------

year = 2009#1910

Tmax_data <- read_csv(paste(working_dir, "Tmax.086338.daily.", year, ".csv", sep = ""), skip = 1)
Tmax_data <- Tmax_data[,1:2]
names(Tmax_data) <- c("date", "tmax")
print(Tmax_data$tmax %>% max())

Tmin_data <- read_csv(paste(working_dir, "Tmin.086338.daily.", year, ".csv", sep = ""), skip = 1)
Tmin_data <- Tmin_data[,1:2]
names(Tmin_data) <- c("date", "tmin")
print(Tmin_data$tmin %>% min())

temp_data <- full_join(Tmax_data, Tmin_data)
dates <- dmy(temp_data$date)
temp_data <- temp_data %>% 
  mutate(date = dates)
print("FIX DATE - THINKS IT'S 2010 not 1910!!!")
print("WARNING HACKED IN RF DATA TO PLOT!!!")

raw_rf_data <- read_csv(paste(working_dir, "IDCJAC0009_086071_", year, "_Data.csv", sep = ""))
rf_data <- raw_rf_data
if(year < 1911){
  rf_data <- raw_rf_data %>%
  mutate(Year = Year + 100) 
}

rf_data <- rf_data %>% 
  mutate(date = paste(Year, Month, Day, sep = "-")) %>% 
  mutate(date = as_date(date)) %>%
  filter(`Period over which rainfall was measured (days)` == 1) %>% 
  filter(`Rainfall amount (millimetres)` > 0) 
rf_data = rf_data[,c(9,6)]
names(rf_data) <- c("date", "rain")

max_rf = max(rf_data$rain)*1.01
scale_rf_data <- rf_data %>% 
  mutate(q = ecdf(rain)(rain)) %>%
  mutate(trans = rain^(2)/max_rf^(2))
plot(scale_rf_data$trans, type = "h")

climate_data <- left_join(temp_data, scale_rf_data, by = "date")

climate_data <- climate_data %>% 
  mutate(angle = 1:nrow(temp_data)/365*360) %>% 
  as.data.frame()

### ---------------------------------------------------------------------------

# cc_palette = colorRampPalette(c("cyan", "magenta","red"))(10)

p <- ggplot(data = climate_data) +      
  geom_bar(stat="identity",  aes(x=as.factor(angle), y= tmax, fill= tmax)) +
  scale_fill_gradient(low = "cyan", high = "magenta") +
  geom_bar(stat="identity",  aes(x=as.factor(angle), y = tmin), fill = alpha("white", 1)) +
  geom_point(aes(x = as.factor(angle), y = 0, size = trans),
             col = "blue", alpha = 0.25, shape = 20) +
  scale_size_continuous(range = c(0, 10)) +
  # scale_fill_distiller(palette = "RdYlBu", direction = -1) +
  # scale_fill_gradientn(colours = cc_palette) +
  # ggtitle(year) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm"),
    legend.position = "none"
  ) +
  ylim(c(-20, 60)) +
  coord_polar(start = 0)

p

### Title gets cropped by margin shave 

### To shade bars will need to trick it
# https://community.rstudio.com/t/ggplot-dynamic-shading-pattern-overlay-on-geom-bar/2281

### Code needed to add central text
# https://medium.com/optima-blog/create-basic-sunburst-graphs-with-ggplot2-7d7484d92c61
