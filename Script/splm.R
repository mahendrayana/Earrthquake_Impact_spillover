library(sf)
library(spdep)
library(splm)
library(tidyverse)
library(plm)



panelq<-st_read("data/output/panelq.geojson")
panelq <- drop_na(panelq)
shp <- st_read("data/grid")

#Proper Panel data
# Create a mapping dictionary
quarter_mapping <- c("q118" = 1, "q218" = 2, "q318" = 3, "q418" = 4, "q119" = 5, "q219" = 6, "q319" = 7, "q419" = 8)

# Use mutate to create a new column with the numeric values
panelq <- panelq %>%
  mutate(quarter = case_when(
    quarter == "q218" ~ "1",
    quarter == "q318" ~ "2",
    quarter == "q418" ~ "3",
    quarter == "q119" ~ "4",
    quarter == "q219" ~ "5",
    quarter == "q319" ~ "6",
    quarter == "q419" ~ "7",
    TRUE ~ quarter
  ))

panelq$time <- panelq$quarter
panelq$id <- panelq$grid_id

panelq <- panelq %>%
  dplyr::select(id, time, everything())

panel_data <- pdata.frame(panelq, index = c("id", "time"))

#Spatial Weight
subset_weight <- panelq[panelq$time %in% c( "1"), ]
points <- st_centroid(subset_weight)
knnnb <- knn2nb(knearneigh(points, k = 7))
weight <- nb2listw(knnnb)


#Regression
# 1 Quarter before Disaster
subset_data <- panel_data[panel_data$time %in% c( "2","1"), ]
fm <- viirs~dq318+treat+treat:dq318+tri+pop
semremod <- spml(formula = fm, data = subset_data, index = NULL, listw = weight,  model = "random", lag = FALSE, spatial.error = "kkp")
summary(semremod)

# 1 Quarter After Disaster
subset_data <- panel_data[panel_data$time %in% c( "2","3"), ]
fm <- viirs~dq418+treat+treat:dq418+tri+pop
semremod <- spml(formula = fm, data = subset_data, index = NULL, listw = weight,  model = "random", lag = FALSE, spatial.error = "kkp")
summary(semremod)


# 2 Quarter After Disaster
subset_data <- panel_data[panel_data$time %in% c( "4","3"), ]
fm <- viirs~dq119+treat+treat:dq119+tri+pop
semremod <- spml(formula = fm, data = subset_data, index = NULL, listw = weight,  model = "random", lag = FALSE, spatial.error = "kkp")
summary(semremod)

# 3 Quarter After Disaster
subset_data <- panel_data[panel_data$time %in% c( "4","5"), ]
fm <- viirs~dq219+treat+treat:dq219+tri+pop
semremod <- spml(formula = fm, data = subset_data, index = NULL, listw = weight,  model = "random", lag = FALSE, spatial.error = "kkp")
summary(semremod)
