library(tidyverse)
library(sf)
library(terra)

all_az_sites <- st_read("data/Flux/AZ_Ameriflux_sites.shp")
plot(all_az_sites)

# subset for sites of interest with available data...

az_sites <- all_az_sites %>% 
  filter(SITE_ID %in% c("US-LS1",
                        "US-LS2",
                        "US-CMW",
                        "US-Wkg",
                        "US-Whs",
                        "US-Fmf",
                        "US-Fuf",
                        "US-Fwf",
                        "US-SRM",
                        "US-SRG",
                        "US-MtB",
                        "US-SRC",
                        "US-SRS",
                        "US-xSR")) %>% 
  select(SITE_ID) %>% 
  st_zm(drop = T, what = "ZM")

plot(az_sites)

st_write(az_sites, "data/Flux/AZ_Ameriflux_subset_sites.shp")
