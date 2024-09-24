library(tidyverse)
source("scripts/Coordinates_from_csv.R")
library(sf)
library(terra)

### Stream Permanence Monitoring:

stream_perm_raw <- read_csv("data/Streamflow_permanence.csv", skip = 1) %>% 
  filter(no_data <= 100)

stream_perm_yrly <- stream_perm_raw %>% 
  group_by(year) %>% 
  summarise(avg_perm = mean(percent_flowing))

stream_perm <- stream_perm_raw %>% 
  group_by(site, x, y) %>% 
  summarise(ann_perm = mean(percent_flowing),
            start = min(year), end = max(year))


stream_perm_points <- coord_from_df(stream_perm_raw, "epsg:4326",
                                           site, x, y)




# , year %in% seq(2007, 2013)
ggplot(filter(stream_perm_raw, year >= 2007), aes(x = year, y = percent_flowing))+
  geom_point(size = 4, aes(color = site, group = site))+
  # geom_line(aes(color = site, group = site))+
  geom_point(data = filter(stream_perm_yrly, year >= 2007), 
             aes(x = year, y = avg_perm), size = 3, color = "black")+
  geom_line(data = filter(stream_perm_yrly, year >= 2007), 
            aes(x = year, y = avg_perm), color = "black")+
 #  geom_bar() # add bars on bottom for number of sites?? lol
  labs(color = "Site", x = "Year", y = "Percent of measured days flowing")+
  theme_light(base_size = 26)

ggplot(data = alluvial_points_raw, aes(geometry = geometry))+
  geom_sf(color = "orange")+
  geom_sf(data = stream_perm_points, color = "skyblue")+
  theme_light()

ggplot(data = stream_perm_points, aes(geometry = geometry))+
  geom_sf(color = "pink")+
  theme_light()

###########

summary(lm(percent_flowing ~ year, filter(stream_perm_raw, site == "Hereford")))
# no temporal trend

# check R2 to see if there is a temporal trend or Not
# for these sites from 2007-2013 where all sites are monitored, 
# what can we say about EVI sensitivity to groundwater?

summary(lm(percent_flowing ~ year, filter(stream_perm_raw, site == "Fairbank")))
