library(raster)
library(sf)
library(terra)
library(dplyr)

#load shp
districts<-st_read("shpkab")

# Filter the ROI shapefile and necesarry features
ids_to_keep <- c("ID7271", "ID7205", "ID7208", "ID7210")

districts_subset <- subset(districts, ADM2_PCODE %in% ids_to_keep)

shp_subset <- districts_subset %>% select(ADM2_EN, ADM2_PCODE, geometry)


# Create a grid that follows the shape of the administrative boundaries
grid <- st_make_grid(shp_subset, cellsize = c(0.05, 0.05))

# Convert the grid to an sf object
grid_sf <- st_sf(geometry = grid)

# Clip the grid to the extent of the administrative boundaries
grid_sf <- st_intersection(grid_sf, shp_subset)

# Remove empty grid cells (if any)
grid_sf <- grid_sf[!is.na(grid_sf$geometry), ]

# Create a new column in the grid shapefile to store the IDs
grid_sf$grid_id <- NA

# Loop through each grid element and assign an ID
for (i in 1:nrow(grid_sf)) {
  # Assign the ID based on the row number
  grid_sf[i, "grid_id"] <- i
}

#Export
st_write(grid_sf, "grid_sf.shp")