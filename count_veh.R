## ===========================================================================
## ===========================================================================

library(tidyverse)
library(sf)
library(sp)

## Import the vehicular count data for September 2023

count_veh <- read.table("Bases/conteo_veh_set23/autoscope_09_2023_volumen.csv", 
                        header = TRUE, sep = ",")

# Select relevant variables

count_veh <- count_veh %>% 
  select(cod_detector, fecha, hora, latitud, longitud, volume, dsc_avenida) %>% 
  group_by(cod_detector) %>% 
  reframe(fecha, hora, latitud, longitud, volume, dsc_avenida) 

# Add the total vehicle volume

vehicle_summary <- count_veh %>%
  group_by(cod_detector, hora, fecha, latitud, longitud, dsc_avenida) %>%
  reframe(total_volume = sum(volume))

rm(count_veh)

# Adjust the format and aggregate by hour

vehicle_summary <- vehicle_summary %>%
  mutate(hora = as.POSIXct(hora, format = "%H:%M:%S")) %>% 
  mutate(hora = format(hora, format = "%H"))

# Create the 'week' variable

vehicle_summary <- vehicle_summary %>%   
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(week = format(fecha, "%W"))

# Create the 'weekday' variable

vehicle_summary <- vehicle_summary %>% 
  mutate(weekday = weekdays(fecha))


vehicle_summary <- filter(vehicle_summary, week == 36)

## ===========================================================================
## ===========================================================================

# Create a heat map

day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

vehicle_summary <- as.data.frame(vehicle_summary)
vehicle_summary$weekday <- as_factor(vehicle_summary$weekday)

vehicle_summary <- vehicle_summary %>% 
  arrange(factor(weekday, levels = day_order)) %>% 
  filter(!is.na(weekday)) %>% 
  filter(!is.na(total_volume)) %>% 
  filter(!is.na(latitud)) %>% 
  filter(!is.na(longitud)) %>% 
  filter(!is.na(fecha)) %>% 
  filter(!is.na(hora)) %>% 
  filter(!is.na(cod_detector))

ggplot(vehicle_summary, aes(x = hora, y = weekday, fill = total_volume)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Vehicle Count by Hour and Day of the Week",
       x = "Hour of the Day", y = "Day of the Week")

# Specify the file path where you want to save the CSV file
file_path <- "Bases/conteo_veh_set23/vehicle_summary.csv"

# Use write.csv to write the data frame to a CSV file
write.csv(vehicle_summary, file = file_path, row.names = FALSE)

## ===========================================================================
## ===========================================================================

# Adjust the coordinate system to UTM

coordinates(vehicle_summary) <- c("longitud", "latitud")
proj4string(vehicle_summary) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Define the destination CRS (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Perform the transformation

vehicle_summary_utm <- spTransform(vehicle_summary, crs_utm)

vehicle_summary_sf <- st_as_sf(vehicle_summary_utm)

vehicle_summary_sf <- st_set_crs(vehicle_summary_sf, st_crs(mapita_segments))

rm(crs_utm, vehicle_summary_utm, vehicle_summary)

# Create a bubble map with the vehicle count

vehicle_summary_sf <- vehicle_summary_sf %>% 
  filter(!total_volume == 0)

ggplot() + 
  geom_sf(data = mapita_segments, color = "#9ebcda", fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#95aabf", size = 0.005) +
  geom_sf(data = vehicle_summary_sf, aes(size = total_volume),
          color = "#8856a7", shape = 21) +
  geom_jitter(width = 20, height = 70) +
  scale_size_continuous(range = c(1.7, 7)) +
  labs(title = "Bubble Map of Vehicle Count") +
  theme_void() +
  theme(legend.position = "right") + 
  labs(color = "Volume")

# Create a shapefile with geocoded information

st_write(vehicle_summary_sf, "Bases/conteo_veh_set23/count_veh.shp")


## ===========================================================================
## ===========================================================================

# Group by hour and day and calculate the mean of vehicles for each hour and day
hourly_vehicles <- vehicle_summary %>%
  group_by(hora, fecha) %>%
  summarise(mean_total_volume = mean(sum(total_volume)))

# Create the stacked bar chart

ggplot(hourly_vehicles, aes(x = hora, y = mean_total_volume, fill = fecha)) +
  geom_bar(stat = "identity") +
  labs(title = "Vehicle Count by Hour of the Day", x = "Hour of the Day", y = "Total Vehicles") +
  theme_minimal() +
  theme(legend.title = element_blank())
