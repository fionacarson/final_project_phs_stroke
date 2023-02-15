library(sf)
library(tidyverse)
library(leaflet)


scot_hb_shapefile <- st_read(here::here("map_files/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp"))

scot_hb_shapefile <- st_transform(scot_hb_shapfile, "+proj=longlat +ellps=WGS84 +datum=WGS84")

simple_scot_hb_shapefile <- rmapshaper::ms_simplify(input = scot_hb_shapfile,
                                           keep = 0.001,
                                           keep_shapes = TRUE)

#st_write(simple_scot_hb_shapefile, here::here("map_files/scotland_hb_shapefile_simplified/
 #        scot_hb_shapefile_simplified.shp"))
                                           
health_board_lat_lon <- read_csv(here::here("map_files/health_board_lat_lon.csv"))  

fake_data <- read_csv(here::here("map_files/fake_health_board_data.csv"))                                
                                           
                                           
leaflet(simple_scot_hb_shapefile) %>% 
  # addTiles adds scotland map from OpenStreetMap  
  addTiles() %>% 
  # addPolygons adds health board shape from shapefile
  addPolygons(color = "black", weight = 1) %>% 
  # fit scotland onto map using fitBounds once we know the dimensions of the map
  fitBounds(lng1 = -1.867, lng2 = -4.867, lat1 = 54.8, lat2 = 60.4167) %>% 
  addCircleMarkers(lng = health_board_lat_lon$Longitude, 
                   lat = health_board_lat_lon$Latitude,
                   radius = fake_data$Situation,
                   color = "purple",
                   weight = 3,
                   opacity = 0.8,
                   label = health_board_lat_lon$HBName)
 #                  labelOptions = labelOptions(noHide = TRUE)) 
  
  
  






