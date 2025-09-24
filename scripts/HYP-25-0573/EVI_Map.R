library(terra)
library(mapview)
library(ggplot2)
library(sf)


# do some pixel averaging in GEE
# to get a raster with the average pixel value for all images within a period
# based on an image collection.

evi_may <- rast("data/Landsat_EVI/USP_2023-05-25.tif")
pix <- as_Spatial(evi_may)

rip <- st_read("data/Alluvial_well_locations.shp")

rip_pix <- st_intersects(rip, evi_may)

evi_sep <- rast("data/Landsat_EVI/USP_2023-09-30.tif")

evi_diff <- evi_sep - evi_may

evi_d <- terra::as.data.frame(evi_diff, xy = TRUE)

#spatial/seasonal change map:
ggplot()+
  geom_raster(data = evi_d, aes(x = x, y = y, fill = EVI))+
  scale_fill_gradient2(low = "red", high = "blue", 
                       mid = "white", midpoint = 0)

plot(evi_may)
plot(evi_sep) # i should get a new scene lol

usp <- st_read("data/USP_WBDHU8.shp")
plot(usp)

st_inter
