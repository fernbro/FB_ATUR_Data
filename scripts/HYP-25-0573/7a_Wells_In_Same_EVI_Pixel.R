library(tidyverse)
library(sf)
library(units)

rip <- st_read("data/Alluvial_well_locations.shp") %>% 
  mutate(well_id = seq(1, nrow(.), by = 1), pair_well = well_id)
up <- st_read("data/General_USPWHIP_well_locations.shp")

clust_r <- read_csv("data/Riparian_Clusters.csv")
clust_u <- read_csv("data/Upland_Clusters.csv")




rip_clust <- clust_r %>% 
  mutate(cluster = case_when(CLUSTER_ID == 1 ~ "A",
                             CLUSTER_ID == 2 ~ "B",
                             CLUSTER_ID == 3 ~ "C",
                             CLUSTER_ID == 4 ~ "D",
                             CLUSTER_ID == 5 ~ "E",
                             CLUSTER_ID == 6 ~ "F",
                             CLUSTER_ID == 7 ~ "G",
                             CLUSTER_ID == 8 ~ "H",
                             CLUSTER_ID == 9 ~ "I",
                             CLUSTER_ID == 10 ~ "J",
                             CLUSTER_ID == 11 ~ "K",
                             .default = NA)) %>% 
  mutate(cluster = case_when(is.na(cluster) ~ as.character(FID), .default = cluster)) %>% 
  select(name, cluster)

up_clust <- clust_u %>% 
  mutate(cluster = case_when(CLUSTER_ID == 1 ~ "L",
                             CLUSTER_ID == -1 ~ as.character(OBJECTID + 100))) %>% 
  select(name, cluster)

clust <- rbind(rip_clust, up_clust)








# 
# 
# # goal:
# 
# # for each of these sf objects (rip, up), I want to:
# # 1) identify wells that have another well within 30 meters
# # 2) label/index these in some way using their names 
# # (to indicate that they are likely wihtin the same pixel; check EVI values to verify)
# # 3) then group analyses by pixel and date and take the median DTG value
# 
# # rip1 <- st_distance(rip) %>% 
# #   data.frame()
# # 
# # colnames(rip1) <- rip$name
# # rownames(rip1) <- rip$name
# 
# rip1 <- st_is_within_distance(rip, dist = 30) %>% 
#   data.frame() %>% 
#   filter(row.id != col.id)
# names(rip1) <- c("well_id", "pair_well")
# 
# rip1 <- rip1 %>% 
#   mutate(w_name = rip$name[well_id],
#          p_name = rip$name[pair_well]) %>% 
#   group_by(w_name) %>% 
#   dplyr::summarise(well_pairs = list(p_name))
# 
# which(rip$name == 2)
# 
# # what i can do now...
# # if two wells in rip have the same row index in rip1 
# # then they're a potential pixel pair
# # i can assign these pairs to IDs and group_by that ID.
# 
# 
# 
# 
# 
# rip_pair <- lapply(rip1, length)
# rip_twins <- rip[which(rip_pairs > 1),]
# nrow(rip_twins) # there are 27 wells that have at least one "pair"
# # how to identify what the pairs are?
# 
# rip_tocheck <- filter(rip, name %in% rip_twins$name)
