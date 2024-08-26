library(terra)
library(sf)
options(scipen = 99999)

# read in landfire tables to get cover for wells
# get a lc_2016 column and lc_2023 column
# write a function for extracting veg info from points

lc2016 <- rast("data/Landcover/LF2016_USP_Clip.tif")
lc2023 <- rast("data/Landcover/LF2023_USP_Clip.tif")
crs(lc2016) <- "EPSG:5070"
crs(lc2023) <- "EPSG:5070"

# bring in landfire metadata

metadata <- read_csv("data/Landcover/Landfire_EVT_meta.csv") %>% 
  select(VALUE, EVT_NAME)
# VALUE (matching alluvial_lcc lc2016 and lc2023 values)
# is what we want to get the EVT_NAME for


# function for extracting point values for a given map

extract_landfire <- function(veg_map_file, points_file){
  
  raw_points <- st_read(points_file)
  points <- vect(st_transform(raw_points, crs = "EPSG:5070")) 
  # change to soft code of veg map??
  
  veg_map <- rast(veg_map_file)
  veg <- terra::extract(veg_map, points) %>% 
    transmute(landcover = EVT_NAME) %>% 
    cbind(as.data.frame(points))
  
  return(veg)
}

# can join after extracting from various maps to detect change at certain wells!



# well points:
alluvial_points_raw <- st_read("data/Alluvial_well_locations.shp")
alluvial_points <- vect(st_transform(alluvial_points_raw, crs = "EPSG:5070"))

alluvial_veg_16 <- terra::extract(lc2016, alluvial_points) %>% 
  transmute(lc2016 = EVT_NAME) %>% 
  cbind(as.data.frame(alluvial_points))

alluvial_veg_23 <- terra::extract(lc2023, alluvial_points) %>% 
  transmute(lc2023 = EVT_NAME) %>% 
  cbind(as.data.frame(alluvial_points))

alluvial_veg <- full_join(alluvial_veg_16, alluvial_veg_23)

alluvial_landfire <- alluvial_veg %>% 
  mutate(lc2016 = metadata$EVT_NAME[match(lc2016, metadata$VALUE)],
         lc2023 = metadata$EVT_NAME[match(lc2023, metadata$VALUE)])

alluvial_lcc <- filter(alluvial_landfire, lc2016 != lc2023)

# ~~~~

regional_points_raw <- st_read("data/General_USPWHIP_well_locations.shp")
regional_points <- vect(st_transform(regional_points_raw, crs = "EPSG:5070"))

regional_veg_16 <- extract(lc2016, regional_points) %>% 
  transmute(lc2016 = EVT_NAME) %>% 
  cbind(as.data.frame(regional_points))

regional_veg_23 <- extract(lc2023, regional_points) %>% 
  transmute(lc2023 = EVT_NAME) %>% 
  cbind(as.data.frame(regional_points))

regional_veg <- full_join(regional_veg_16, regional_veg_23)

regional_landfire <- regional_veg %>% 
  mutate(lc2016 = metadata$EVT_NAME[match(lc2016, metadata$VALUE)],
         lc2023 = metadata$EVT_NAME[match(lc2023, metadata$VALUE)])

regional_lcc <- filter(regional_landfire, lc2016 != lc2023)

