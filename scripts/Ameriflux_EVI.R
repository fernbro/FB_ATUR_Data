library(tidyverse)
library(sf)
library(terra)
source("scripts/Process_PRISM.R")

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
  transmute(name = SITE_ID) %>% 
  st_zm(drop = T, what = "ZM")

plot(az_sites)

st_write(az_sites, "data/Flux/AZ_Ameriflux_subset_sites.shp")

source("scripts/Process_PRISM.R")

flux_ppt <- process_prism(2000:2024, "ppt", az_sites)
flux_tmean <- process_prism(2000:2024, "tmean", az_sites)
flux_vpdmax <- process_prism(2000:2024, "vpdmax", az_sites)

flux_prism <- full_join(flux_ppt, flux_tmean, join_by(name, date)) %>% 
  full_join(flux_vpdmax, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10)

write_csv(flux_prism, "data/AZ_Flux_PRISM.csv")
