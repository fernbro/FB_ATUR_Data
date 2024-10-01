library(tidyverse)
library(sf)
library(terra)

# avg and standard deviation formatting fxns:

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

wells_by_reach <- read_csv("data/SPRNCA/Wells_Reaches.csv")
hist(wells_by_reach$reach, breaks = 15)
# now attach these to the combo dataframe so that each well & evi obs is paired with
# a reach number!! yay..


# Landsat 32 d composite EVI

reach_means <- list.files("data/SPRNCA/EVI", pattern = "mean.csv", full.names = T)
reach_sds <- list.files("data/SPRNCA/EVI", pattern = "sd.csv", full.names = T)

reach_mean_list <- lapply(reach_means, read_csv)
reach_sd_list <- lapply(reach_sds, read_csv)

reach_mean_proc <- lapply(reach_mean_list, format_mean)
reach_sd_proc <- lapply(reach_sd_list, format_sd)

reach_mean <- bind_rows(reach_mean_proc)
reach_sd <- bind_rows(reach_sd_proc)

reach_evi <- full_join(reach_mean, reach_sd) %>% 
  mutate(cv = sd/mean)

# also extract pixel counts!
# functions for reading in files

ggplot(filter(reach_evi, year(date) %in% c(2003, 2023)), aes(x = date, y = mean, group = reach))+
  geom_line()+
  # geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd),
  #             alpha = 0.3)+
  facet_wrap(~reach + year(date), scales = "free_x")

ggplot(filter(reach3, year(date) == 2012), aes(x = date, y = cv))+
  geom_line()+
  geom_line(aes(y = sd), color = "red")+
  geom_line(aes(y = mean), color = "blue")

stats <- reach3 %>% 
  summarise(mean = mean(mean, na.rm = T),
            sd = mean(sd, na.rm = T))
