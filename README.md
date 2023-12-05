## UN-Datathon-2023-Los-Simuladores

# Tramways, a viable and sustainable option for Montevideo?

### Important

This project consists of four main .R files:
  - census_sim1.R ~ CRUCIAL to load this first, as it contains the base map divided by census segments (mapita_segments) that is used all throughout the project, plus some other necessary objects.
  - mobility_svy.R ~ Analysis of the origin - destiny mobility survey for the year 2016 (IM, UNDP) in Montevideo. This survey is weighted (w = wcal0).
  - count_veh.R ~ Analysis of vehicle count data (IM) from the most important avenues in Montevideo. File "autoscope_09_2023_volumen.csv" was too large to upload on github, you can manually install it from:
    https://catalogodatos.gub.uy/dataset/intendencia-montevideo-conteo-de-vehiculos-del-centro-de-gestion-de-la-movilidad/resource/114ce728-40b2-4ecd-9d43-eaa5e183456b, or load one of the derivate .shp and .csv files (filtered or geocoded).
  - tramway_sys.R ~ Creation of the tramway lines and stops, making up 4 total lines derived from the data analysis.

Additionally, you can find: 
  - simuladores_presentation.html ~ The final project presentation.
  - simuladores_presentation.rmd ~ The R Markdown file the presentation was created in.
