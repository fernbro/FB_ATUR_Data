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
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%y")),
            name = station_nm, lat = y, lon = x, level = lev_va) %>% 
  mutate(well = "Upland")
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

write_csv(groundwater, "data/Processed/USP_GW_Zscores_11142024.csv")