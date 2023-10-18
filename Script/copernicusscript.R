library(raster)
library(sf)
library(terra)
library(dplyr)

copernicus<-st_read("data/copernicus")

#load shp
grid_sf<-st_read("grid_sf")

copernicus_join <- st_join(copernicus, grid_sf)


write.csv(copernicus_join@data, "copernicus.csv", row.names = FALSE)