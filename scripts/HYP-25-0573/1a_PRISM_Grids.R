library(tidyverse)
library(sf)
library(terra)

# 9/17/2025:
# this script identifies which wells are in the same PRISM pixels
# this is the basis upon which EVI will be assessed...
# the model will estimate the median EVI at GW monitoring points in each pixel
# based on the median DTG at those points, as well as landscape (pixel level) PPT + VPDmax

fabric <- st_read("data/prism_4km_mesh/prism_4km_mesh.shp")

rip <- st_read("data/Alluvial_well_locations.shp") %>% 
  st_transform(crs = st_crs(fabric))
up <- st_read("data/General_USPWHIP_well_locations.shp") %>% 
  st_transform(crs = st_crs(fabric))

# only select fabric overlapping with my well locations

fab_rip <- unlist(st_intersects(rip, fabric)) %>% 
  cbind(rip)
colnames(fab_rip) <- c("prism", 'name', 'geometry')
fab_up <- unlist(st_intersects(up, fabric)) %>% 
  cbind(up)
colnames(fab_up) <- c("prism", 'name', 'geometry')

fabric_sub_r <- fabric[(unlist(st_intersects(rip, fabric))),]
length(unique(fabric_sub_r$geometry))

fabric_sub_u <- fabric[(unlist(st_intersects(up, fabric))),]
length(unique(fabric_sub_u$geometry))

# for each geometry, i want to take this info and bind it

ggplot()+
  geom_sf(data = fabric_sub_r, fill = "green", alpha = 0.1)+
  geom_sf(data = fabric_sub_u, fill = "orange", alpha = 0.1)+
  geom_sf(data = rip, color = "blue")+
  geom_sf(data = up)+
  theme_light()


