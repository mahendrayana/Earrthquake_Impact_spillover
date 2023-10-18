library(raster)
library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(writexl)

#####datframe#####
# Load the raster data and shapefile for time period 1
pop18_ras <- raster("data/landscan_before.tif")
pop19_ras <- raster("data/landscan_after.tif")
srtm <- raster("data/srtm_slope.tif")
q218 <- raster("data/viirsq218.tif")
q318 <- raster("data/viirsq318.tif")
q418 <- raster("data/viirsq418.tif")
q119 <- raster("data/viirsq119.tif")
q219 <- raster("data/viirsq219.tif")
q319 <- raster("data/viirsq319.tif")
q419 <- raster("data/viirsq419.tif")
shp <- st_read("data/grid")

# Cell-to-cell matching

#Population
pop18 <- extract(pop18_ras, shp, fun = mean, na.rm = TRUE)
pop19 <- extract(pop19_ras, shp, fun = mean, na.rm = TRUE)

# merge the TRI into shp
slope <- extract(srtm, shp)
calculate_TRI <- function(elevation) {
  return(max(elevation) - min(elevation))
}
tri <- sapply(slope, calculate_TRI)
shp$tri <- subsettri

#Earthquake Impact
treat <- read.csv("EI.csv", sep = ";")
treat <- treat %>% rename(grid_id = ?..grid_id)

#merge with the shp
shp <- shp %>% 
  left_join(treat, by = "grid_id")

#Nighttime Lights
q218 <- raster::extract(q218, shp, fun = mean, na.rm = TRUE)
q318 <- raster::extract(q318, shp, fun = mean, na.rm = TRUE)
q418 <- raster::extract(q418, shp, fun = mean, na.rm = TRUE)
q119 <- raster::extract(q119, shp, fun = mean, na.rm = TRUE)
q219 <- raster::extract(q219, shp, fun = mean, na.rm = TRUE)
q319 <- raster::extract(q319, shp, fun = mean, na.rm = TRUE)
q419 <- raster::extract(q419, shp, fun = mean, na.rm = TRUE)

dq218 <- data.frame(grid_id = shp$grid_id, viirs = q218, pop = pop18)
dq318 <- data.frame(grid_id = shp$grid_id, viirs = q318, pop = pop18)
dq418 <- data.frame(grid_id = shp$grid_id, viirs = q418, pop = pop18)
dq119 <- data.frame(grid_id = shp$grid_id, viirs = q119, pop = pop19)
dq219 <- data.frame(grid_id = shp$grid_id, viirs = q219, pop = pop19)
dq319 <- data.frame(grid_id = shp$grid_id, viirs = q319, pop = pop19)
dq419 <- data.frame(grid_id = shp$grid_id, viirs = q419, pop = pop19)


# Combine the individual data frames into a panel data frame
panelq <- bind_rows(dq218, dq318, dq418, dq119, dq219, dq319, dq419)

# Create dummy variables for each quarter
panelq <- panelq %>%
  mutate(quarter = rep(c("q218", "q318", "q418", "q119", "q219", "q319", "q419"), each = nrow(dq218)))


quarter_names <-c("q218", "q318", "q418", "q119", "q219", "q319", "q419")

for (quarter_name in quarter_names) {
  
  # Create the dummy variable
  dummy_var <- ifelse(panelq$quarter == quarter_name, 1, 0)
  
  # Assign the dummy variable to the corresponding column in the data frame
  panelq[paste0("d", quarter_name)] <- dummy_var
}

#Merge the panel data with the shp file that already contains the tri and treat to create a spatio temporal dataset
panelq_sf <- shp %>% left_join(panelq, by = "grid_id")

st_write(panelq_sf, "data/output/panelq.geojson")
