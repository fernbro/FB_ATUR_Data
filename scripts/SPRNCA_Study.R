library(tidyverse)
library(terra)
library(sf)

coord_from_df <- function(df, epsg, site_column, lon, lat){
  
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

# wrangling/exploring data from this report:
# https://uppersanpedropartnership.org/wp-content/uploads/2024/04/SPRNCA-Riparian-Report_2023_Dixon_and_Robertson.pdf

whip_well_codes <- c("STD-LI", "SUM-LI", "CONLID", "FBK-LO",
                      "BOQ-UP", "CHB-LI", "MODLND", "COTBLM",
                       "HUN-LI", "HER-LS", "KOL-LO", "PAL-LS",
                       "PLS-LI")

sprnca_sites <- c("STD", "SUM", "CON", "FAI",
                  "BOQ", "CHB", "MOS", "COT",
                  "HUN", "HER", "KOL", "PAL",
                  "PLS")

sprnca_study_wells <- data.frame(whip_well_codes,
                                 sprnca_sites)





