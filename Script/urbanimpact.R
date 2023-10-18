bu1ras<- raster("data/ndbi_imagebefore.tif")
bu2ras<- raster("data/ndbi_imageaftr.tif")
pop1ras <-raster("data/landscan_after.tif")
pop2ras <- raster("data/landscan_before.tif")
shp <- st_read(dsn = "data/grid")
srtm <- raster("data/srtm_slope.tif")
di_sf <- st_read("")

# merge the TRI into shp
slope <- extract(srtm, shp)
calculate_TRI <- function(elevation) {
  return(max(elevation) - min(elevation))
}
tri <- sapply(slope, calculate_TRI)
shp$tri <- subsettri

#Earthquake Impact
treat <- read.csv("data/EI.csv", sep = ";")
treat <- treat %>% rename(grid_id = ?..grid_id)
shp <- shp %>% 
  left_join(treat, by = "grid_id")

bu1 <- raster::extract(bu1ras, shp, fun = mean, na.rm = TRUE)
bu2 <- raster::extract(bu2ras, shp, fun = mean, na.rm = TRUE)

pop1 <- raster::extract(pop1ras, shp, fun = mean, na.rm = TRUE)
pop2 <- raster::extract(pop2ras, shp, fun = mean, na.rm = TRUE)

bu1df <- data.frame(grid_id = shp$grid_id, pop=pop1, di=di_sf$di, bu = bu1, post=0)
bu2df <- data.frame(grid_id = shp$grid_id, pop=pop2, di=di_sf$di, bu = bu2, post=1)

panelbu <- bind_rows(bu1df,bu2df)
panelbu_sf <- shp %>% left_join(panelbu, by = "grid_id")


write.csv(panelbu_sf, "panelbu.csv")




min_val <- 0
max_val <- 956.95868

# Normalize bund using min-max normalization
panelbu_sf$nbun_pop <- (panelbu_sf$bun_pop - min_val) / (max_val - min_val)
panelbu_sf$lbp <- log(panelbu_sf$nbun_pop + 0.0001)

panelbu_sf$bun <- (panelbu_sf$bu + 0.316)
panelbu_sf$lbund <- log(panelbu_sf$bund)

panelbu_sfreserve<-panelbu_sf
panelbu_sf<-panelbu2_sfreserve

panelbu_sf$bu_pop <- panelbu_sf$pop / panelbu_sf$bu
panelbu_sf$bund_pop <- panelbu_sf$pop / panelbu_sf$bundn
panelbu_sf$bun_pop <- panelbu_sf$bun * panelbu_sf$pop

panelbu_sf$pop[panelbu_sf$pop == 0] <- 1

reg1<-bu~post+treat+post:treat+tri+di
reg2<-bun~post+treat+post:treat+tri+di
reg3<-bun_pop~post+treat+post:treat+tri+di
reg4<-nbun_pop~post+treat+post:treat+tri+di

reglm1 <- lm(reg1, data=panelbu_sf)
reglm2 <- lm(reg2, data=panelbu_sf)
reglm3 <- lm(reg3, data=panelbu_sf)
reglm4 <- lm(reg4, data= panelbu_sf)


reg41 <- lmSLX(reg4, data=panelbu_sf, weight)
reg42 <- lagsarlm(reg4, data=panelbu_sf, weight)
reg43 <- errorsarlm(reg4, data=panelbu_sf, weight)
reg44 <- lagsarlm(reg4, data=panelbu_sf, weight, type ="mixed")

summary(reg43)