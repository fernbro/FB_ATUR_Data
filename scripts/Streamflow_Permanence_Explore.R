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
  filter(no_data <= 100) %>% 
  group_by(site, x, y) %>% 
  summarise(avg_perm = mean(percent_flowing), sd_perm = sd(percent_flowing),
            start = min(year), end = max(year))


stream_perm_points <- coord_from_df(stream_perm_raw, "epsg:4326",
                                           site, x, y)

# find closest stream perm point to the wells!

# read in well shapefiles:

alluvial_wells <- st_read("data/Alluvial_well_locations.shp") # in NAD27 (EPSG:4267)
alluvial <- st_transform(alluvial_wells, crs = "epsg:32612")
regional_wells <- st_read("data/General_USPWHIP_well_locations.shp") # in NAD27 (EPSG:4267)
regional <- st_transform(regional_wells, crs = "epsg:32612")

stream_perm_points_rep <- st_transform(stream_perm_points, crs = "epsg:32612")

alluvial_closest <- st_nearest_feature(alluvial, stream_perm_points_rep)
alluvial_perm <- data.frame(dist = st_distance(alluvial, stream_perm_points_rep[alluvial_closest,],
                                               by_element = T, which = "Euclidean")) %>% 
  cbind(alluvial$name, stream_perm_points_rep[alluvial_closest,]$name)
names(alluvial_perm) <- c("distance", "well_name", "perm_name")

alluv_pairings <- alluvial_perm %>% 
  mutate(distance = as.numeric(distance)) %>% 
  group_by(well_name) %>% 
  arrange(distance) %>% 
  mutate(count = row_number()) %>% 
  filter(count == 1, distance <= 250) %>% 
  select(-count)

# write_csv(alluv_pairings, "data/Processed/USP_AlluvialWells_StreamPerm_Pairings.csv")

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

# ggplot(data = alluvial_points_raw, aes(geometry = geometry))+
#   geom_sf(color = "orange")+
#   geom_sf(data = stream_perm_points, color = "skyblue")+
#   theme_light()

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

# 2007 to 2013! woo!

evi_dtg_z <- read_csv("data/USP_GW_EVI_Z_FullYear_10312024.csv")

stream_perm_all <- stream_perm_raw %>% 
  filter(no_data <= 100) %>% 
  select(site, year, percent_flowing) %>% 
  full_join(alluv_pairings, join_by(site == perm_name),
            relationship = "many-to-many") %>% 
  mutate(name = well_name) %>% 
  select(-well_name)

z_scores_perm <- full_join(evi_dtg_z, stream_perm_all)

# colors <- c("firebrick3", "darkorange1", "goldenrod1", 
#             "darkgreen", "blue", "purple")
# names(colors) <- as.factor(seq(4, 9, by = 1))

evi_dtg_perm <- z_scores_perm %>% 
  full_join(stream_perm, join_by(site)) %>% 
  filter(!is.na(percent_flowing)) %>% 
  mutate(perm_z = (percent_flowing - avg_perm)/sd_perm )
  

ggplot(filter(evi_dtg_perm, 
              well == "alluvial",
              !is.na(percent_flowing),
              year >= 2007), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = year))+
  scale_color_gradient2(low = ("orange"), mid = "purple", high = "skyblue", midpoint = 2014)+
  geom_smooth(method = "lm", se = F)+
  theme_light()+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  labs(color = "yearly flow permanence")+ # may not be that helpful bc CALENDAR YEARS???
  ggtitle("JAS EVI vs. DTG z-scores relative to each month")+
  theme(strip.background = element_rect(color = "black", fill = "white"),
        strip.text = element_text(colour = 'black'))
  # theme(strip.text = element_text(colour = 'black'))+
  # facet_wrap(~year)


summary(lm(evi_z ~ dtg_z + percent_flowing, data = filter(z_scores_perm, 
                                        well == "alluvial",
                                        !is.na(percent_flowing)))) 
# R2 = 0.21 (add percent_flowing as an expl var, R2 goes up to 0.23...)



