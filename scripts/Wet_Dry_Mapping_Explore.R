library(tidyverse)
source("scripts/Coordinates_from_csv.R")
library(sf)
library(terra)

###################
### Wet/Dry Mapping:

wet_reaches <- st_read("data/SPRNCA/WetDry_1999to2023_Shareable/WetDry_1999to2023_wet_Shareable.shp") %>% 
  st_zm()

survey_reaches <- st_read("data/SPRNCA/WetDry_1999to2023_Shareable/WetDry_1999to2023_Survey_Shareable.shp") %>% 
  st_zm()

# Proposed processing steps:

# 1. identify wells within 100m? (or another distance) from a surveyed reach in a given year
# 2. remove wells from a year's analysis if their reach wasn't surveyed
# 3. then identify wells for each year that are within 100m of a wet reach
#  wells that are near a surveyed reach but not a wet one can be identified as "dry wells"
# 4. i want a dataframe with rows of each well and year with its wet or dry status

# read in well shapefiles:

alluvial_wells <- st_read("data/Alluvial_well_locations.shp")
regional_wells <- st_read("data/General_USPWHIP_well_locations.shp")

# reproject points to the wet-dry map crs:
# "epsg:3742"

alluvial <- st_transform(alluvial_wells, crs = "epsg:3742")
regional <- st_transform(regional_wells, crs = "epsg:3742")

# try st_distance()

alluvial_reaches <- data.frame(st_distance(alluvial,
                                survey_reaches,
                                which = "Euclidean"))




