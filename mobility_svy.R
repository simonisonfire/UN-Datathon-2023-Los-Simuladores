## ===========================================================================
## ===========================================================================

## Load the srvyr library to work with the survey

library(tidyverse)
library(srvyr)
library(survey)

## Load the mobility survey dataset

travel_data <- read.table("Bases/EOD2016/Base Viajes.csv", header = TRUE, sep = ";") %>% 
  select("VF5", "VF6", "tiempoviaje", "modoprincipal", "VF9", "codsegorigen", "codsegproposito", "wcal0", "INSE")

# Convert commas to periods and convert to double
travel_data <- as.data.frame(travel_data)

travel_data$VF5 <- gsub(",", ".", travel_data$VF5)
travel_data$VF6 <- gsub(",", ".", travel_data$VF6)
travel_data$wcal0 <- gsub(",", ".", travel_data$wcal0)

travel_data$wcal0 <- as.double(travel_data$wcal0)
travel_data$VF6 <- as.double(travel_data$VF6)
travel_data$VF5 <- as.double(travel_data$VF5)
travel_data$INSE <- as.double(travel_data$INSE)

## Filter modoprincipal to keep relevant modes of transportation

travel_data <- travel_data %>% 
  filter(modoprincipal %in% c(0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 13))

## Add a variable with a vehicle type label

travel_data <- travel_data %>% 
  mutate(vehicle_type = case_when(
    modoprincipal %in% c(0, 1) ~ "On foot",
    modoprincipal == 2 ~ "Bicycle",
    modoprincipal %in% c(4, 5, 6) ~ "Taxi, Uber, Ride-sharing, etc.",
    modoprincipal %in% c(7, 8, 9, 10) ~ "Private vehicle",
    modoprincipal == 13 ~ "Bus"
  ))

## ===========================================================================
## ===========================================================================

## Create the dataframe with the survey weights

travel_survey <- travel_data %>%
  group_by(codsegorigen, codsegproposito, wcal0, vehicle_type, INSE) %>%
  summarize(travel_count = n()) %>% 
  as_survey_design(ids = NULL, strata = NULL, weights = wcal0)

## Create a cross-tabulation table with the variables: 
## origin of travel and vehicle type

origin_vehicle_table <- svytable(~codsegorigen + vehicle_type, design = travel_survey)
view(origin_vehicle_table)

origin_vehicle <- as.data.frame(origin_vehicle_table)

ggplot(data = origin_vehicle, aes(x = vehicle_type, y = log(Freq))) +
  geom_col(stat="identity", width = 0.5, fill = "#fc8d62")+
  theme_minimal() +
  labs(title = "Distribution of Transportation Modes in Montevideo") +
  xlab("Vehicle Type") +
  ylab("Frequency (log)")

## ===========================================================================
## ===========================================================================

## Create new DataFrames summarizing the number of trips between pairs of census segments

destination_trips <- travel_survey %>%
  group_by(codsegproposito) %>%
  summarize(
    travel_count = n(),
    median_trips = survey_median(travel_count, w = wcal0, NA.rm = TRUE)
  ) %>%
  ungroup()

origin_trips <- travel_survey %>%
  group_by(codsegorigen) %>%
  summarize(
    travel_count = n(),
    median_trips = survey_median(travel_count, w = wcal0, NA.rm = TRUE)
  ) %>%
  ungroup()

# Merge the origin and destination data with census data

destination_data <- merge(mapita_segments, destination_trips, by.x = "CODSEG", by.y = "codsegproposito")

origin_data <- merge(mapita_segments, origin_trips, by.x = "CODSEG", by.y = "codsegorigen")

## ===========================================================================
## ===========================================================================

# Calculate quintiles
quintiles <- quantile(travel_survey$INSE, probs = seq(0, 1, 0.2))

travel_survey$INSE <- as.numeric(travel_survey$INSE)

travel_survey <- travel_survey %>% na.omit()

# Create quintiles and assign them to the 'quintil_INSE' column
travel_survey <- travel_survey %>%
  mutate(quintil_INSE = cut(INSE, breaks = quantile(INSE, probs = 0:5/5), include.lowest = TRUE, labels = FALSE))

# Group by 'quintil_INSE' and calculate the sum of 'travel_count'
summary <- travel_survey %>%
  group_by(quintil_INSE) %>%
  summarize(total_trips = sum(travel_count))

# Bar plot
ggplot(summary, aes(x = factor(quintil_INSE), y = total_trips)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#ffd92f") +
  labs(title = "Trips by Socioeconomic Level",
       x = "Socioeconomic Quintile", y = "Number of Trips") +
  theme_minimal()

## ===========================================================================
## ===========================================================================

## Plot Origin and Destination 

ggplot() +
  geom_sf(data = mapita_segments, color = "lightblue") +
  geom_sf(data = destination_data, aes(fill = median_trips), color = "#95aabf") +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Number of Trips") +
  labs(title = "Destination Map of Trips") +
  geom_sf(data = avenues, color = "#525152", alpha = 0.5, size = 0.005) +
  theme_void()

ggplot() +
  geom_sf(data = mapita_segments, color = "lightblue") +
  geom_sf(data = origin_data, aes(fill = median_trips), color = "#95aabf") +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Number of Trips") +
  labs(title = "Origin Map of Trips") +
  geom_sf(data = avenues, color = "#525152", alpha = 0.5, size = 0.005) +
  theme_void()
