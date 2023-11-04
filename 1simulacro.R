# Cargamos las librerías

library(sf)
library(tidyverse)

# Creamos un data frame con las rutas de ómnibus en montevideo

rutas_bus <- read_sf("Bases/v_uptu_lsv")

# Filtramos base de datos

rutas_bus <- rutas_bus %>% 
  select(DESC_LINEA, DESC_SUBLI, DESC_VARIA, geometry)

# Creamos un data frame con datos del censo de 2011

mapita <- read_sf("Bases/Marco2011_ZONA_Montevideo_VivHogPer")

# Filtramos mapita

mapita <- mapita %>% 
  select(DENS_P, CODSEG, geometry)

# Agregamos áreas por segmento censal

crs = st_crs(mapita)
st_crs(mapita) = NA

mapita_segmentos = mapita %>% 
  group_by(CODSEG) %>% 
  summarise(median_DENS_P = median(DENS_P))

st_crs(mapita_segmentos) = crs

rm(mapita, crs)

# Graficamos la densidad de población mediana por segmento censal

ggplot(data = mapita_segmentos) +  
  geom_sf(aes(fill = median_DENS_P)) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  ggtitle(label = "Densidad de población mediana por segmento censal")

# Superponemos las rutas de ómnibus sobre el mapa de densidad de población mediana

ggplot(data = mapita_segmentos) +
  geom_sf(aes(fill = median_DENS_P)) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  ggtitle(label = "Densidad de población mediana por segmento censal\n y rutas de ómnibus en Montevideo") +
  geom_sf(data = rutas_bus, color = "lightyellow", size = 0.5)

# Mapa de densidad de población mediana y paradas de ómnibus

paradas <- read_sf("Bases/v_uptu_paradas")

paradas <- st_transform(paradas, crs = st_crs(mapita_segmentos))

ggplot(data = mapita_segmentos) +
  geom_sf(aes(fill = median_DENS_P)) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  ggtitle(label = "Densidad de población y paradas de ómnibus en Montevideo")+
  geom_sf(data = paradas, color = "black", size = 0.000001) 
