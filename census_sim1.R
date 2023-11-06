## ===========================================================================
## ===========================================================================

# Load the libraries

library(sf)
library(tidyverse)

## ===========================================================================
## ===========================================================================

# Create a data frame with bus routes in Montevideo

bus_routes <- read_sf("Bases/v_uptu_lsv")

# Filter the dataset

bus_routes <- bus_routes %>% 
  select(DESC_LINEA, DESC_SUBLI, DESC_VARIA, geometry)

# Create a data frame with 2011 census data

mapita <- read_sf("Bases/Marco2011_ZONA_Montevideo_VivHogPer")

# Filter the data

mapita <- mapita %>% 
  select(DENS_P, CODSEG, geometry)

# Aggregate areas by census segment

crs = st_crs(mapita)
st_crs(mapita) = NA

mapita_segments = mapita %>% 
  group_by(CODSEG) %>% 
  summarise(median_DENS_P = median(DENS_P))

st_crs(mapita_segments) = crs

rm(mapita, crs)

## ===========================================================================
## ===========================================================================

# Plot the median population density by census segment

ggplot(data = mapita_segments) +  
  geom_sf(aes(fill = median_DENS_P)) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  ggtitle(label = "Median Population Density by Census Segment")

# Overlay bus routes on the map of median population density

ggplot(data = mapita_segments) +
  geom_sf(aes(fill = median_DENS_P)) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  ggtitle(label = "Median Population Density by Census Segment\n and Bus Routes in Montevideo") +
  geom_sf(data = bus_routes, color = "lightyellow", size = 0.5)

# Map of median population density and bus stops

bus_stops <- read_sf("Bases/v_uptu_paradas")

bus_stops <- st_transform(bus_stops, crs = st_crs(mapita_segments))

ggplot(data = mapita_segments) +
  geom_sf(aes(fill = median_DENS_P)) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  ggtitle(label = "Population Density and Bus Stops in Montevideo")+
  geom_sf(data = bus_stops, color = "black", size = 0.000001) 
