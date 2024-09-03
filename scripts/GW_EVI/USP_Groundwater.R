library(tidyverse)
library(sf)
options(scipen = 99999)
# Data acquired from San Pedro WHIP (https://uppersanpedrowhip.org/map/) on July 5th, 2024

# read in data:

# 1. groundwater levels below land surface; non-pumping wells
gw_levels <- read_csv("data/Groundwater_levels_below_land_surface.csv",
                      skip = 2, col_names = T)

groundwater <- gw_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%Y")),
         name = station_nm, lat = y, lon = x, level = lev_va)

# extract locations:

gw_locations <- groundwater %>% 
  select(name, lat, lon) %>% 
  as_tibble() %>% 
  distinct(name, lat, lon) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_sf(crs = "epsg:4267") #NAD27 (coordinates)



# 2. alluvial aquifer levels BLS; non-pumping wells
alluv_levels <- read_csv("data/Near-stream_alluvial-aquifer_water_levels.csv",
                         skip = 2, col_names = T)


alluvial <- alluv_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%Y")),
         name = station_nm, lat = y, lon = x, level = lev_va)

# extract locations: (used for Landsat pixels?)

alluv_locations <- alluvial %>% 
  as_tibble() %>% 
  distinct(name, lat, lon) %>%
  as.data.frame() %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_sf(crs = "epsg:4267") #NAD27
  
ggplot(gw_locations)+
  geom_sf()+
  geom_sf(data = alluv_locations)
ggplot(filter(alluvial, year(date) > 2000), aes(x = date, y = level, 
                                                group = name))+
  geom_line()
  
# write out shapefile to get into GEE script for extracting values to points!
# st_write(alluv_locations, "../USP_WHIP_07052024/Alluvial_well_locations.shp")
st_write(gw_locations, "data/General_USPWHIP_well_locations.shp")
