library(tidyverse)

# Groundwater Measurements from uppersanpedrowhip.org/map

# Alluvial:

alluv_levels <- read_csv("data/Near-stream_alluvial-aquifer_water_levels.csv",
                         skip = 2, col_names = T)

alluvial <- alluv_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, 
                              tryFormats = c("%m/%d/%Y")),
            name = station_nm, 
            lat = y, 
            lon = x, 
            level = lev_va) %>%
  mutate(well = "Riparian")

# Rest of the wells (Upland area; Regional aquifer):

regional_levels <- read_csv("data/Groundwater_levels_below_land_surface.csv")

regional <- regional_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, 
                              tryFormats = c("%m/%d/%y")),
            name = station_nm, 
            lat = y, lon = x, 
            level = lev_va) %>% 
  mutate(well = "Upland") %>% 
  filter(year(date) <= 2024)
# combine groundwater measurements

water_combo <- rbind(regional, alluvial) %>% 
  select(date, name, well, level)

water_stats <- water_combo %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>% 
  filter(!is.na(level)) %>% 
  group_by(name) %>% 
  summarise(dtg_mean = mean(level, na.rm = T),
            dtg_sd = sd(level, na.rm = T),
            dtg_cv = dtg_sd / dtg_mean) %>% 
  ungroup()

groundwater <- full_join(water_combo, water_stats) %>% 
  mutate(dtg_z = (level - dtg_mean)/dtg_sd)

#write_csv(groundwater, "data/Processed/USP_GW_Zscores_11142024.csv")

min(subset(groundwater, well == "Riparian")$level)
max(subset(groundwater, well == "Riparian")$level)
min(subset(groundwater, well == "Upland")$level)
max(subset(groundwater, well == "Upland")$level)

# VISUALIZE:

ggplot(filter(groundwater, well == "Upland", 
              year(date) >= 2000), 
       aes(x = (date), y = dtg_z))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = "free")

ggplot(filter(groundwater, well == "Riparian", 
              year(date) >= 2000), 
       aes(x = (date), y = (-level)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = "free")

ggplot(groundwater, aes(x = well, y = (level*0.3048)))+
  geom_boxplot()+
  theme_light()+
  labs(x = "Well location", y = "Site mean DTG (m)")

ggplot(filter(groundwater, well == "Riparian" & month(date) %in% c(6, 7, 8, 9)), 
       aes(x = month(date), group = month(date), y = (level*0.3048)))+
  geom_boxplot()+
  theme_light()+
  labs(x = "Well location", y = "Site mean DTG (m)")

ggplot(groundwater, aes(x = well, y = -dtg_mean*0.3048))+
  geom_boxplot()+
  theme_light()+
  labs(x = "Well location", y = "Depth to groundwater (m)")
