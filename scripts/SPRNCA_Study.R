library(tidyverse)
library(terra)
library(sf)

coord_from_df <- function(df, epsg, sites, lon, lat){
  points <- df %>% 
    select({{sites}}, {{lon}}, {{lat}}) %>% 
    transmute(name = {{sites}},
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

whip_well_codes <- c("STD-LI", 
                     "SUM-LI", 
                     "CONLID", 
                     "FBK-LO",
                     "BOQ-UP", 
                     "CHB-LI", 
                     "MODLND", 
                     "COTBLM",
                     "HUN-LI", 
                     "HER-LS", 
                     "KOL-LO", 
                     "PAL-LS",
                     "PLS-LI")

# matched these to above wells in ArcGIS: (maintain vector order)

sprnca_sites <- c("STD", "SUM", "CON", "FAI",
                  "BOQ", "CHB", "MOS", "COT",
                  "HUN", "HER", "KOL", "PAL",
                  "PLS")

sprnca_study_wells <- data.frame(whip_well_codes,
                                 sprnca_sites)
names(sprnca_study_wells) <- c("whip", "sprnca")

# 2023 SPRNCA report study sites:

sprnca <- read_csv("data/SPRNCA_2023_study_sites.csv")
spr_points <- coord_from_df(sprnca, "epsg:32612", 
                            site_name, easting, northing)

spr_points <- st_transform(spr_points, crs = "epsg:4267")
#st_write(spr_points, "data/SPRNCA_site_points.shp")


# bring in z-score EVI data: (run Wells_EVI.R)

z_scores_nca <- z_scores %>%
  mutate(whip =  sub(".*\\[(.*?)\\].*", "\\1", name)) %>% 
  filter(whip %in% whip_well_codes) %>%
  full_join(sprnca_study_wells)

"[\\s*(.*?)\\s*]"

  
  
  mutate(whip_code = substr(name, 16, 21))
  filter(grepl(paste(whip_well_codes, 
                     collapse = "|"), name)) %>% 
  



