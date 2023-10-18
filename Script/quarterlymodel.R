library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(spatialreg)
library(writexl)
library(spdep)

#Load the data
panelq_sf <- st_read("data/output/panelq.geojson")

#Create quarter pair
quarter_pairs <-c("q218_q318", "q318_q418", "q418_q119", "q119_q219", "q219_q319", "q319_q419")

#Create weight
subset_data <- panelq_sf[panelq_sf$quarter %in% c( "q318","q418"), ]
subsetnb <- poly2nb(subset_data, queen = TRUE )
weight <- nb2listw(subsetnb)

#reg <- lagsarlm(viirs~dq418+treat+dq418:treat+tri+pop, data=subset_data, weight, type="mixed")
#summary(reg)

#Model Selection

aic = list()
bic = list()

for (quarter_pair in quarter_pairs) {
  # Subset the data for the current month pair
  subset_data <- subset(panelq_sf, (quarter %in% strsplit(quarter_pair, "_")[[1]]))
  
  # Extract the start month and end month from the month pair
  quarters <- strsplit(quarter_pair, "_")[[1]]
  start_quarter <- quarters[1]
  end_quarter <- quarters[2]
  
  # Construct the dummy variable name
  dummy_var <- paste("d", end_quarter, sep = "")
  
  # Define the formula for the regression model
  formula <- as.formula(paste("viirs ~", dummy_var, "+ treat +", dummy_var, ":treat + pop + tri"))
  
  # Fit the regression models
  lm_model <- lm(formula, data = subset_data)
  lmSLX_model <- lmSLX(formula, data = subset_data, weight)
  lagsarlm_model <- lagsarlm(formula, data = subset_data, weight)
  errorsarlm_model <- errorsarlm(formula, data = subset_data, weight)
  durbin_model <- lagsarlm(formula, data = subset_data, weight, type="mixed")
  
  # Extract the AIC and BIC values for each model
  aic_values <- AIC(lm_model, lmSLX_model, lagsarlm_model, errorsarlm_model, durbin_model)
  bic_values <- BIC(lm_model, lmSLX_model, lagsarlm_model, errorsarlm_model, durbin_model)
  
  # Store the AIC and BIC values in the list
  aic[[quarter_pair]] <- list(AIC = aic_values)
  bic[[quarter_pair]] <- list(BIC = bic_values)
}
aic_df <- bind_rows(aic, .id = "quarter_pair")
bic_df <- bind_rows(bic, .id = "quarter_pair")

#Spatial Durbin Model
resultsdm <- list()

# Loop through each month pair
for (quarter_pair in quarter_pairs) {
  # Subset the data for the current month pair
  subset_data <- subset(panelq_sf, (quarter %in% strsplit(quarter_pair, "_")[[1]]))
  
  # Extract the start month and end month from the month pair
  quarters <- strsplit(quarter_pair, "_")[[1]]
  start_quarter <- quarters[1]
  end_quarter <- quarters[2]
  
  # Construct the dummy variable name
  dummy_var <- paste("d", end_quarter, sep = "")
  
  # Define the formula for the regression model
  formula <- as.formula(paste("viirs ~", dummy_var, "+ treat +", dummy_var, ":treat + pop + tri"))
  
  # Fit the regression model
  reg_model <- lagsarlm(formula, data = subset_data, weight, type="mixed")
  
  # Extract the coefficients of interest
  coef_month_pair <- coef(reg_model)[paste(dummy_var, ":treat", sep = "")]
  resultsdm[[quarter_pair]] <- reg_model
}



sdmdf <- data.frame(quarter_pairs = character(0),
                    coef = numeric(0),
                    se = numeric(0))

# Loop through each month pair
for (quarter_pair in quarter_pairs) {
  # Extract the coefficients and standard errors from the results list
  reg_model <- resultsdm[[quarter_pair]]
  coef_quarter_pair <- coef(reg_model)
  se_quarter_pair <- sqrt(diag(vcov(reg_model)))

  # Exclude the intercept term from the coefficients
  coef_quarter_pair <- coef_quarter_pair[-1]
  se_quarter_pair <- se_quarter_pair[-1]
  
  # Create a data frame with the coefficient values, standard errors, and month pair information
  quarter_pair_df <- data.frame(quarter_pairs = quarter_pair,
                              coef = coef_quarter_pair,
                              se = se_quarter_pair)
  
  # Append the data frame to the main data frame
  sdmdf <- rbind(sdmdf, quarter_pair_df)
}

#Generate the p_value
sdmdf$z_value <- sdmdf$coef / sdmdf$se
sdmdf$p_value <- 2 * (1 - pnorm(abs(sdmdf$z_value)))

write
write_xlsx(sdmdf, "report/regression/sdmdf.xlsx")
