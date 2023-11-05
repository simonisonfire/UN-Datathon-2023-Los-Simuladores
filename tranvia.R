##======================================================================================

library(sf)
library(tidyverse)
library(sp)

#======================================================================================
# 1er dataframe con paradas de Tranvía
# Ruta 1 - Manga

long1 <- c(-34.80719, -34.81466, -34.82207, -34.83071, -34.83729, -34.84473, -34.85169,
           -34.85915, -34.86632, -34.87380, -34.88183, -34.88837, -34.88907, -34.89420)

lat1 <- c(-56.13722, -56.13880, -56.14020, -56.14277, -56.14821, -56.15427, -56.15995,
          -56.16609, -56.17142, -56.17756, -56.18227, -56.18596, -56.19205, -56.19474)

pt1 <- data.frame(lat1, long1)
rm(long1, lat1)

# Ajustamos el sistema de coordenadas a UTM

coordinates(pt1) <- c("lat1", "long1")
proj4string(pt1) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

pt1_utm <- spTransform(pt1, crs_utm)

pt1_sf <- st_as_sf(pt1_utm)

pt1_sf <- st_set_crs(pt1_sf, st_crs(mapita_segmentos))

rm(crs_utm, pt1_utm, pt1)

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = pt1_sf, color = "#8856a7")

#======================================================================================
# 2do dataframe con paradas de Tranvía
# Ruta 2 - Punta de Rieles

long2 <- c(-34.86055, -34.86817, -34.87418, -34.87766, -34.88309, -34.88586, -34.88606,
           -34.88793, -34.88876, -34.88784, -34.88438, -34.89420)

lat2 <- c(-56.13376, -56.13442, -56.13908, -56.14516, -56.15441, -56.16027, -56.16369,
          -56.16516, -56.17523, -56.18568, -56.19202, -56.19433)

pt2 <- data.frame(long2, lat2)
rm(long2, lat2)

# Ajustamos el sistema de coordenadas a UTM

coordinates(pt2) <- c("lat2", "long2")
proj4string(pt2) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

pt2_utm <- spTransform(pt2, crs_utm)

pt2_sf <- st_as_sf(pt2_utm)

pt2_sf <- st_set_crs(pt2_sf, st_crs(mapita_segmentos))

rm(crs_utm, pt2_utm, pt2)

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = pt2_sf, color = "#8856a7")

#======================================================================================
# 3er dataframe con paradas de Tranvía
# Ruta 3 - La Teja

long3 <- c(-34.86890, -34.86336, -34.85887, -34.85512, -34.85996, -34.86424, -34.86892,
           -34.87527, -34.88067, -34.88920, -34.89420)

lat3 <- c(-56.23954, -56.23274, -56.22730, -56.22272, -56.21530, -56.20995, -56.20549,
          -56.19989, -56.19561, -56.19204, -56.19433)

pt3 <- data.frame(long3, lat3)
rm(long3, lat3)

# Ajustamos el sistema de coordenadas a UTM

coordinates(pt3) <- c("lat3", "long3")
proj4string(pt3) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

pt3_utm <- spTransform(pt3, crs_utm)

pt3_sf <- st_as_sf(pt3_utm)

pt3_sf <- st_set_crs(pt3_sf, st_crs(mapita_segmentos))

rm(crs_utm, pt3_utm, pt3)

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = pt3_sf, color = "#8856a7")

#======================================================================================

# 4to dataframe con paradas de Tranvía
# Ruta 4 - Centro

long4 <- c(-34.89420, -34.90273, -34.90515, -34.90645)
lat4 <- c(-56.19433, -56.19290, -56.19434, -56.19876)

pt4 <- data.frame(long4, lat4)
rm(long4, lat4)

# Ajustamos el sistema de coordenadas a UTM

coordinates(pt4) <- c("lat4", "long4")
proj4string(pt4) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

pt4_utm <- spTransform(pt4, crs_utm)

pt4_sf <- st_as_sf(pt4_utm)

pt4_sf <- st_set_crs(pt4_sf, st_crs(mapita_segmentos))

rm(crs_utm, pt4_utm, pt4)

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = pt4_sf, color = "#8856a7")

#======================================================================================
#Trazado de rutas de tranvía

#La Teja
tlong3 <- c(-34.86885, -34.85491, -34.85571, -34.86299, -34.87144, -34.87529, -34.87674,
            -34.87850, -34.88246, -34.88603, -34.89404, -34.89430)
tlat3 <- c(-56.23951, -56.222233, -56.22096, -56.21120, -56.20289, -56.19977, -56.19733,
           -56.19635, -56.19492, -56.19230, -56.19152, -56.19476)

tlin3 <- data.frame(tlong3, tlat3)
rm(tlong3, tlat3)

# Ajustamos el sistema de coordenadas a UTM

coordinates(tlin3) <- c("tlat3", "tlong3")
proj4string(tlin3) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

tlin3_utm <- spTransform(tlin3, crs_utm)

tlin3_sf <- st_as_sf(tlin3_utm)

rm(crs_utm, tlin3_utm, tlin3)

coords <- st_coordinates(tlin3_sf)

tlin3_sf <- st_sfc(st_linestring(coords))

tlin3_sf <- st_set_crs(tlin3_sf, st_crs(mapita_segmentos))

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = tlin3_sf, color = "#8856a7", linewidth = 1.25)

#======================================================================================
#Manga
tlong1 <- c(-34.80717, -34.82472, -34.82882, -34.86224, -34.86243, -34.86387, -34.87640,
            -34.88881, -34.88801, -34.88815, -34.88853, -34.89409, -34.89420)
tlat1 <- c(-56.13729, -56.14095, -56.14123, -56.16851, -56.16987, -56.16967, -56.17963, 
           -56.18634, -56.18864, -56.18916, -56.19206, -56.19150, -56.19433)

tlin1 <- data.frame(tlong1, tlat1)
rm(tlong1, tlat1)

# Ajustamos el sistema de coordenadas a UTM

coordinates(tlin1) <- c("tlat1", "tlong1")
proj4string(tlin1) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

tlin1_utm <- spTransform(tlin1, crs_utm)

tlin1_sf <- st_as_sf(tlin1_utm)

rm(crs_utm, tlin1_utm, tlin1)

coords <- st_coordinates(tlin1_sf)

tlin1_sf <- st_sfc(st_linestring(coords))

tlin1_sf <- st_set_crs(tlin1_sf, st_crs(mapita_segmentos))

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = tlin1_sf, color = "#8856a7", linewidth = 1.25)

#======================================================================================
#Punta de Rieles
tlong2 <- c(-34.85568, -34.85748, -34.86600, -34.87148, -34.87285, -34.88380, -34.88719,
            -34.88446, -34.88550, -34.88792, -34.88936, -34.88687, -34.88824, -34.88854,
            -34.89406, -34.89420)
tlat2 <- c(-56.13207, -56.13350, -56.13409, -56.13531, -56.13673, -56.15542, -56.15835,
           -56.16210, -56.16329, -56.16474, -56.18146, -56.18808, -56.18889, -56.19208,
           -56.19151, -56.19433)

tlin2 <- data.frame(tlong2, tlat2)
rm(tlong2, tlat2)

# Ajustamos el sistema de coordenadas a UTM

coordinates(tlin2) <- c("tlat2", "tlong2")
proj4string(tlin2) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

tlin2_utm <- spTransform(tlin2, crs_utm)

tlin2_sf <- st_as_sf(tlin2_utm)

rm(crs_utm, tlin2_utm, tlin2)

coords <- st_coordinates(tlin2_sf)

tlin2_sf <- st_sfc(st_linestring(coords))

tlin2_sf <- st_set_crs(tlin2_sf, st_crs(mapita_segmentos))

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = tlin2_sf, color = "#8856a7", linewidth = 1.25)

#======================================================================================
#Centro
tlong4 <- c(-34.89927, -34.90255, -34.90513, -34.90546, -34.90644, -34.90648)
tlat4 <- c(-56.19317, -56.19288, -56.19411, -56.19823, -56.19817, -56.19912)

tlin4 <- data.frame(tlong4, tlat4)
rm(tlong4, tlat4)

# Ajustamos el sistema de coordenadas a UTM

coordinates(tlin4) <- c("tlat4", "tlong4")
proj4string(tlin4) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Definimos el CRS de destino (UTM)
crs_utm <- CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs")

# Realizamos la transformación

tlin4_utm <- spTransform(tlin4, crs_utm)

tlin4_sf <- st_as_sf(tlin4_utm)

rm(crs_utm, tlin4_utm, tlin4)

coords <- st_coordinates(tlin4_sf)

tlin4_sf <- st_sfc(st_linestring(coords))

tlin4_sf <- st_set_crs(tlin4_sf, st_crs(mapita_segmentos))

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = tlin4_sf, color = "#8856a7", linewidth = 1.25)

#======================================================================================

## MAPA DEL SISTEMA DE TRANVíAS DE MONTEVIDEO

ggplot() + 
  geom_sf(data = mapita_segmentos, fill = "#e0ecf4") +
  geom_sf(data = avenidas, color = "#9ebcda", size = 0.005) +
  geom_sf(data = tlin1_sf, color = "#fc8d62", linewidth = 1.25) +
  geom_sf(data = tlin2_sf, color = "#e78ac3", linewidth = 1.25) +
  geom_sf(data = tlin3_sf, color = "#a6d854", linewidth = 1.25) +
  geom_sf(data = tlin4_sf, color = "#ffd92f", linewidth = 1.25) +
  labs(title = "Mapa del Sistema Tranviario de Montevideo") +
  theme_minimal()
  
