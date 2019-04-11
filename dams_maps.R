## Damn maps

library(tidyverse)


## Three files are loaded
dams <- read_csv2(
  file = "~/Documents/Projects/TAI/AdditionalDATA/dams_eng.csv") # All dams in the world (AQUASTATS FAO)
dam_no <- readxl::read_excel(
  path = '/Users/juanrocha/Documents/Projects/TAI/TransformedData/DamsDatabaseCecchi.XLS') ## Dataset Katja found but we cannot use
dam_katja <- readxl::read_xlsx(
  path = "/Users/juanrocha/Documents/Projects/TAI/TransformedData/Data_Burkina&Ganha/Volta_Dam_density_KLC.xlsx") ## Dataset Katja curated

# some renaming and cleaning
dams <- dams %>% 
  rename(lon = 25, lat = 24) %>% 
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))

## Two maps are created [Katja's dams don't have geo information]
m <- leaflet(dam_no) %>% addTiles() %>% addCircleMarkers(~Longitude, ~Latitude, radius = 0.5)
w <- leaflet(dams) %>% addTiles() %>% addCircles(radius = 0.5)