library(tidyverse)
library(sf)

coord_from_df <- function(df, # data frame with geometry columns
                          epsg, # reference system that the data came in/were reported
                          site_column, # column with a name for each point
                          lon, # longitude (x coord) column
                          lat){ # latitude (y coord) column
  
  points <- df %>% 
    select({{site_column}}, {{lon}}, {{lat}}) %>% 
    transmute(name = {{site_column}},
              lat = as.numeric({{lat}}),
              lon = as.numeric({{lon}})) %>% 
    as_tibble() %>% 
    distinct(name, lat, lon) %>% 
    as.data.frame() %>% 
    st_as_sf(coords = c("lon", "lat")) %>% 
    st_sf(crs = epsg)
  
  return(points)
  
}










#~~~~~~~~~~~~~~~~~~~~~~~~~~
# USGS scientific report 2005-5163: hydrologic site observations

# site locations:
usgs_report_sites <- read_csv("data/USGS_2005-5163/USGS_2005-5163_Parsed_Peiz_Locations.csv")

# data: (list of files with a particular ending)

# datum for locations: NAD83 ("epsg:26912")


usgs_locations <- usgs_report_sites %>% 
  select(peiz_code, utm_northing, utm_easting) %>% 
  transmute(name = peiz_code,
    lat = as.numeric(utm_northing),
         lon = as.numeric(utm_easting)) %>% 
  as_tibble() %>%
  distinct(name, lat, lon) %>%
  as.data.frame() %>% 
  st_as_sf(coords = c("lon", "lat")) %>%  # how do i keep the attributes?
  st_sf(crs = "epsg:26912") #NAD83 (coordinates) 

st_write(usgs_locations, "data/USGS_2005-5163/Piezometer_locations.shp", append = T)

# function version: for converting from a table to a shapefile.

test <- coord_from_df(usgs_report_sites, "epsg:26912", 
                      peiz_code, utm_easting, utm_northing)

# transmute(): acts as "select" and "mutate,"
# where new df only contains specified fields