library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(spatialreg)
library(spdep)
library(writexl)

#Load the data
panelq_sf <- st_read("data/output/panelq.geojson")

#Yearly liinear regression
subset_data <- panelq_sf[panelq_sf$quarter %in% c("q318", "q419"), ]
reg <- viirs ~ treat + dq419 + dq419 : treat + pop + tri

#Creating spatial weight matrix
subsetnb <- poly2nb(subset_data, queen = TRUE )
weight <- nb2listw(subsetnb, style="B")

#Univariate Moran
subset_matrix <- as.matrix(subset_data[, c("viirs", "tri", "pop")])
univariate_moran <- moran.test(subset_matrix, weight)
#Multivariate Moran
multivariate_moran <- multivariateMoran(residuals(reg), listw = subsetnb, wt = weight)

#Yearly AIC BIC
reg1 <- lm(reg, data = subset_data)
reg2 <- lmSLX(reg, data = subset_data, weight)
reg3 <- lagsarlm(reg, data = subset_data, weight)
reg4 <- errorsarlm(reg, data = subset_data, weight)
reg5 <- lagsarlm(reg, data = subset_data, weight, type = "mixed")

AIC(reg1,reg2,reg3,reg4)
BIC(reg1,reg2,reg3,reg4)
