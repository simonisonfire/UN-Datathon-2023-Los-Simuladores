## ===========================================================================
## ===========================================================================

## Cargamos la librería srvyr para trabajar con la encuesta

library(tidyverse)
library(srvyr)
library(survey)

## Cargamos la base de datos de la encuesta de movilidad

base_viajes <- read.table("Bases/EOD2016/Base Viajes.csv", header = TRUE, sep = ";") %>% 
  select("VF5","VF6", "tiempoviaje", "modoprincipal", "VF9", "codsegorigen", "codsegproposito", "wcal0", "INSE" )

#Convertimos las comas en puntos y pasamos a Double
base_viajes <- as.data.frame(base_viajes)

base_viajes$VF5 <- gsub(",", ".", base_viajes$VF5)
base_viajes$VF6 <- gsub(",", ".", base_viajes$VF6)
base_viajes$wcal0 <- gsub(",", ".", base_viajes$wcal0)

base_viajes$wcal0 <- as.double(base_viajes$wcal0)
base_viajes$VF6 <- as.double(base_viajes$VF6)
base_viajes$VF5 <- as.double(base_viajes$VF5)


## Filtramos modoprincipal para quedarnos con los medios de transporte relevantes

base_viajes <- base_viajes %>% 
  filter(modoprincipal %in% c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 13))


## Agregamos una variable con una etiqueta del tipo de vehiculo
base_viajes <- base_viajes %>% 
  mutate(tipo_vehiculo = case_when(
    modoprincipal %in% c(0, 1) ~ "A pie",
    modoprincipal == 2 ~ "Bicicleta",
    modoprincipal %in% c(4, 5, 6) ~ "Taxi, Uber, Remise, etc.",
    modoprincipal %in% c(7, 8, 9, 10) ~ "Vehiculo particular",
    modoprincipal == 13 ~ "Ómnibus"
  )) 

## ===========================================================================
## ===========================================================================

## Creamos el dataframe con el ponderador

viajes_svy <- base_viajes %>%
  group_by(codsegorigen, codsegproposito, wcal0, tipo_vehiculo) %>%
  summarize(cantidad_viajes = n()) %>% 
  as_survey_design(ids = NULL, strata = NULL, weights = wcal0)

## Tiramos una tabla cruzada con las variables: 
## origen del viaje y tipo de vehiculo

tabla_origen_veh <- svytable(~codsegorigen + tipo_vehiculo, design = viajes_svy)
view(tabla_origen_veh)

## ===========================================================================
## ===========================================================================

## Creamos nuevos DataFrames que resume la cantidad de viajes entre pares de segmentos censales


viajes_destino <-  viajes_svy %>%
  group_by(codsegproposito) %>%
  summarize(
    cantidad_viajes = n(),
    median_viajes = survey_median(cantidad_viajes, w = wcal0, NA.rm = TRUE)
  ) %>%
  ungroup()

viajes_origen <- viajes_svy %>%
  group_by(codsegorigen) %>%
  summarize(
    cantidad_viajes = n(),
    median_viajes = survey_median(cantidad_viajes, w = wcal0, NA.rm = TRUE)
  ) %>%
  ungroup()


# Fusionamos los datos de origen y destino con los datos censales

destino_data <- merge(mapita_segmentos, viajes_destino, by.x = "CODSEG", by.y = "codsegproposito")

origen_data <- merge(mapita_segmentos, viajes_origen, by.x = "CODSEG", by.y = "codsegorigen")

## Creamos un objeto que contenga las avenidas principales de Montevideo

avenidas <- read_sf("Bases/avenidas")

## ===========================================================================
## ===========================================================================
## Graficamos Origen y Destino junto con las Avenidas y las Paradas de Bus

ggplot() +
  geom_sf(data = mapita_segmentos, color = "lightblue") +
  geom_sf(data = destino_data, aes(fill = median_viajes)) +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Cantidad de Viajes") +
  labs(title = "Mapa de Destinos de Viajes y Avenidas") +
  geom_sf(data = avenidas, color = "black", alpha = 0.5, size = 0.005)


ggplot() +
  geom_sf(data = mapita_segmentos, color = "lightblue") +
  geom_sf(data = origen_data, aes(fill = median_viajes)) +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Cantidad de Viajes") +
  labs(title = "Mapa de Orígenes de Viajes") +
  geom_sf(data = avenidas, color = "black", alpha = 0.5, size = 0.005)
