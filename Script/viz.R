library(ggplot2)
library(RColorBrewer)
library(spatialreg)
library(spdep)
library(sf)
library(writexl)

sdmdf <- read.csv("report/regression/sdmdf.csv", )
panelq_sf <- st_read("data/output/panelq.geojson")

#yealry abline

subset_data <- panelq_sf[panelq_sf$quarter %in% c("q318", "q419"), ]
reg <- viirs ~ treat + dq419 + dq419 : treat + pop + tri

subsetnb <- poly2nb(subset_data, queen = TRUE )
weight <- nb2listw(subsetnb, style="B")

abline <- lagsarlm(reg, data = subset_data, weight, type = "mixed")

# Order the month_pairs in the desired order
ordered_month_pairs <- factor(sdmdf$month_pairs, levels = month_pairs)

# Create the line graph
ggplot(sdmdf, aes(x = ordered_month_pairs, y = coef, group=1)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = which(sdmdf$month_pairs == "q218_q318"), linetype = "dashed", color = "red") +
  annotate("text", x = which(sdmdf$month_pairs == "q218_q318"), y = max(sdmdf$coef), 
           label = "After Earthquake", vjust = -0.1, hjust = -0.3, color = "red") +
  geom_abline(intercept = 0.845568271, slope = 0, linetype = "dashed", color = "blue") +
  annotate("text", x = which(sdmdf$month_pairs == "q219_q319"), y = 0.6,
           label = "Yearly Impact", vjust = -0.5, hjust = -0.1, color = "blue") +
  labs(x = "Q Pairs", y = "Coefficient") +
  ggtitle("Coefficient by quarter pairs") +
  theme_bw()


ggplot(sdmdf, aes(x = ordered_month_pairs, y = coef, group = 1)) +
  geom_line(size = 1) +  # Increase line size to make it bolder
  geom_point() +
  geom_vline(xintercept = which(sdmdf$month_pairs == "q218_q318"), linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = which(sdmdf$month_pairs == "q218_q318"), y = max(sdmdf$coef), 
           label = "After Earthquake", vjust = -0.1, hjust = -0.3, color = "red") +
  geom_abline(intercept = 0.845568271, slope = 0, linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = which(sdmdf$month_pairs == "q219_q319"), y = 0.6,
           label = "Yearly Impact", vjust = -0.5, hjust = -0.1, color = "blue") +
  labs(x = "Q Pairs", y = "Coefficient") +
  ggtitle("Coefficient by quarter pairs") +
  theme_bw()