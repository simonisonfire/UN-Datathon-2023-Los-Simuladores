## ===========================================================================
## ===========================================================================

library(tidyverse)
library(sf)
library(sp)

## Importamos la base de conteo vehicular para el mes de setiembre de 2023

count_veh <- read.table("Bases/conteo_veh_set23/autoscope_09_2023_volumen.csv", 
                        header = TRUE, sep = ",")

# Seleccionamos las variables relevantes

count_veh <- count_veh %>% 
  select(cod_detector, fecha, hora, latitud, longitud, volume, dsc_avenida) %>% 
  group_by(cod_detector) %>% 
  reframe(fecha, hora, latitud, longitud, volume, dsc_avenida) 

# Agregamos el total del volumen de vehículos

resumen_vehiculos <- count_veh %>%
  group_by(cod_detector, hora, fecha, latitud, longitud, dsc_avenida) %>%
  reframe(total_volumen = sum(volume))

# Arreglamos el formato y agregamos por hora
 
resumen_vehiculos <- resumen_vehiculos %>%
  mutate(hora = as.POSIXct(hora, format = "%H:%M:%S")) %>% 
  mutate(hora = format(hora, format = "%H"))
  
# Creamos la varible semana

resumen_vehiculos <- resumen_vehiculos %>%   
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(semana = format(fecha, "%W"))

# Creamos la variable weekday

resumen_vehiculos <- resumen_vehiculos %>% 
  mutate(weekday = weekdays(fecha))

rm(count_veh)

resumen_vehiculos <- filter(resumen_vehiculos, semana == 36)

## ===========================================================================
## ===========================================================================

# Creamos un mapa de calor

orden_dias <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

resumen_vehiculos <- as.data.frame(resumen_vehiculos)
resumen_vehiculos$weekday <- as_factor(resumen_vehiculos$weekday)

resumen_vehiculos <- resumen_vehiculos %>% 
  arrange(factor(weekday, levels = orden_dias)) %>% 
  filter(!is.na(weekday)) %>% 
  filter(!is.na(total_volumen)) %>% 
  filter(!is.na(latitud)) %>% 
  filter(!is.na(longitud)) %>% 
  filter(!is.na(fecha)) %>% 
  filter(!is.na(hora)) %>% 
  filter(!is.na(cod_detector))

ggplot(resumen_vehiculos, aes(x = hora, y = weekday, fill = total_volumen)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Cantidad de Vehículos por Hora y Día de la Semana",
       x = "Hora del Día", y = "Día de la Semana")

## ===========================================================================
## ===========================================================================

# Ajustamos el sistema de coordenadas a UTM

coordinates(resumen_vehiculos) <- c("longitud", "latitud")
proj4string(resumen_vehiculos) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

resumen_vehiculos_utm <- spTransform(resumen_vehiculos, crs_utm)

resumen_vehiculos_sf <- st_as_sf(resumen_vehiculos_utm)

resumen_vehiculos_sf <- st_set_crs(resumen_vehiculos_sf, st_crs(mapita_segmentos))

rm(crs_utm, resumen_vehiculos_utm, resumen_vehiculos)

# Creamos un mapa de burbujas con la cantidad de vehículos

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = resumen_vehiculos_sf, aes(size = total_volumen), color = "#8856a7", shape = 21) +
  geom_jitter(width = 2.5, height = 2.5) +
  scale_size_continuous(range = c(10, 10)) +
  labs(title = "Mapa de Burbujas de Cantidad de Vehículos") +
  theme_void() +
  theme(legend.position = "right") + 
  labs(color = "Volumen")

# Creamos un shapefile con la información geocodificada

st_write(resumen_vehiculos_sf, "Bases/conteo_veh_set23/count_veh.shp")

count_geo <- read_sf("Bases/conteo_veh_set23/count_veh.shp")

## ===========================================================================
## ===========================================================================

# Agrupamos por hora y día y calculamos la media de vehículos para cada hora y día
vehiculos_hora <- resumen_vehiculos %>%
  group_by(hora, fecha) %>%
  summarise(mean_total_volumen = mean(sum(total_volumen)))

# Crea el gráfico de barras apiladas

ggplot(vehiculos_hora, aes(x = hora, y = mean_total_volumen, fill = fecha)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Vehículos por Hora del Día", x = "Hora del Día", y = "Total de Vehículos") +
  theme_minimal() +
  theme(legend.title = element_blank())







