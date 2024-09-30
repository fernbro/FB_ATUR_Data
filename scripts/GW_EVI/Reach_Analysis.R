library(tidyverse)
library(sf)
library(terra)

reach_sf <- st_read("data/SPRNCA/SPRNCA_Reaches.shp")
alluv_wells <- st_read("data/Alluvial_well_locations.shp") %>% 
  st_transform(crs = "epsg:4326")

ggplot()+
  geom_sf(data = reach_sf)+
  geom_sf(data = alluv_wells)

# calculate:
# wells per reach
# distance between wells in each reach
# arcgis will be nice :)


# read in Reach-specific EVI data:

# Landsat 32 d composite EVI

# avg and standard deviation


sprnca3a <- read_csv("data/SPRNCA/EVI/LandsatEVI_SPRNCA3_means.csv")
sprnca3b <- read_csv("data/SPRNCA/EVI/LandsatEVI_SPRNCA3_sd.csv")

format_sd <- function(df){
  
  formatted <- df %>% 
    mutate(date1 = str_sub(`system:index`, 1, 8)) %>% 
    transmute(date = as.POSIXct(date1, tryFormats = "%Y%m%d"),
              sd = EVI, reach = ReachID)
  
  return(formatted)
    
}

format_mean <- function(df){
  
  formatted <- df %>% 
    mutate(date1 = str_sub(`system:index`, 1, 8)) %>% 
    transmute(date = as.POSIXct(date1, tryFormats = "%Y%m%d"),
              mean = EVI, reach = ReachID)
  
  return(formatted)
  
}

sd_3 <- format_sd(sprnca3b)
mean_3 <- format_mean(sprnca3a)

# also extract pixel counts!
# functions for reading in files

reach3 <- full_join(mean_3, sd_3)

ggplot(reach3, aes(x = date, y = mean))+
  geom_point()+
  geom_ribbon(aes(ymin = mean - (2*sd), ymax = mean + (2*sd)),
              alpha = 0.5)

stats <- reach3 %>% 
  summarise(mean = mean(mean, na.rm = T),
            sd = mean(sd, na.rm = T))
