library(tidyverse)
#install.packages("dataRetrieval")
library(dataRetrieval)
# install.packages("nhdplusTools")
# install.packages("arrow")
# library(nhdplusTools)
library(sf)
library(terra)
#install.packages("data.table")
library(data.table)

# load in watershed:

#usp <- st_read("../../Data/USP_geography/USP_Boundary/USP_Boundary.shp")

usp <- get_huc(
  id = "15050202",
  type = "huc08")

# get NHD objects based on area of interest?

usp_fl <- get_nhdplus(
  AOI = usp,
  realization = "flowline")

plot(usp_fl)

usp_huc12 <- get_huc(
  AOI = usp,
  type = "huc12") %>% 
  filter(huc12 %like% "^15050202")
plot(usp_huc12[1])

# Groundwater Data

codes <- readNWISpCode("all")
gw_code <- codes[grep("groundwater level above",
                      codes$parameter_nm,
                      ignore.case=TRUE),]

AZ_sites <- whatNWISsites(stateCd = "AZ", parameterCd = "62611")

AZ_sites_map <- st_as_sf(x = AZ_sites, 
         coords = c("dec_long_va", "dec_lat_va"),
         crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# define lat-long bounding box

# Upper San Pedro: (rough)

usp_bb <- terra::ext(-110.607203, -109.695586, 30.912081, 32.346726)
st_crs(usp_bb) <- st_crs(AZ_sites_map)

plot(AZ_sites_map)

usp_site_sf <- st_intersection(AZ_sites_map, usp_bb)

# usp sites <- 

AZ_gw_data <- readNWISuv(siteNumbers = "330115109145601",
                         parameterCd = "62611",
                         startDate = "",
                         endDate = "", 
                         tz = "America/Phoenix")


