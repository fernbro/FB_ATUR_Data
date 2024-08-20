library(terra)
library(sf)
options(scipen = 99999)

# read in landfire tables to get cover for wells
# get a lc_2016 column and lc_2023 column

lc2016 <- rast("data/Landcover/LF2016_USP_Clip.tif")
lc2023 <- rast("data/Landcover/LF2023_USP_Clip.tif")
crs(lc2016) <- "EPSG:5070"
crs(lc2023) <- "EPSG:5070"

# well points:
alluvial_points_raw <- st_read("data/Alluvial_well_locations.shp")
alluvial_points <- vect(st_transform(alluvial_points_raw, crs = "EPSG:5070"))

regional_points_raw <- st_read("data/General_USPWHIP_well_locations.shp")
regional_points <- vect(st_transform(regional_points_raw, crs = "EPSG:5070"))

alluvial_veg_16 <- terra::extract(lc2016, alluvial_points) %>% 
  transmute(lc2016 = EVT_NAME) %>% 
  cbind(as.data.frame(alluvial_points))

alluvial_veg_23 <- terra::extract(lc2023, alluvial_points) %>% 
  transmute(lc2023 = EVT_NAME) %>% 
  cbind(as.data.frame(alluvial_points))

alluvial_lcc <- full_join(alluvial_veg_16, alluvial_veg_23) %>% 
  filter(lc2016 != lc2023)

# bring in landfire metadata

metadata <- read_csv("data/Landcover/Landfire_EVT_meta.csv")
# VALUE (matching alluvial_lcc lc2016 and lc2023 values)
# is what we want to get the EVT_NAME for
