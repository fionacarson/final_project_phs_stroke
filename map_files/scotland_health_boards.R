library(sf)
library(tidyverse)
library(leaflet)

# shapefile sourced from 
#https://github.com/tomwhite/covid-19-uk-data/issues/18

hb <- st_read(here::here("map_files/UK_covid_reporting_regions/UK_covid_reporting_regions.shp"))

health_boards <- c("Ayrshire and Arran", "Borders", "Dumfries and Galloway",
"Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland", 
"Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles")

scottish_hb <- hb %>% 
  filter(name %in% health_boards)



tictoc::tic()


leaflet(scottish_hb) %>% 
  # addTiles adds scotland map from OpenStreetMap  
  addTiles() %>% 
  # addPolygons adds health board shape from shapefile
  addPolygons(color = "black", weight = 1) %>% 
  # fit scotland onto map using fitBounds once we know the dimensions of the map
  fitBounds(lng1 = -0.867, lng2 = -4.867, lat1 = 54.8, lat2 = 60.4167)



tictoc::toc()