library(tidyverse)
# install.packages("zoo")
library(zoo)

# need to read in both evi tables

# read in EVI from before 2013??

evi_alluv1 <- read_csv("data/EVI_alluvial_2013_2023.csv")
evi_gen1 <- read_csv("data/EVI_2013_2023_general.csv")
evi_alluv2 <- read_csv("data/EVI_alluvwells_2000_2013.csv")
evi_gen2 <- read_csv("data/EVI_generalwells_2000_2013.csv")

evi_ts <- rbind(evi_alluv1, evi_alluv2)
  
evi <- evi_ts %>%  # alluvial aquifer wells
  mutate(date = make_date(year = year, month = month, day = day),
         evi = EVI, well = "alluvial") %>% 
  select(name, evi, date, well)

evi2 <- evi_ts_general %>% # regional aquifer wells
  mutate(date = make_date(year = year, month = month, day = day),
         evi = EVI, well = "regional") %>% 
  select(name, evi, date, well)

evi_combo <- rbind(evi, evi2)

ggplot(filter(evi_combo), aes(x = date, y = evi, color = well, group = well))+
  # geom_line()+
  geom_smooth()

# according to Nagler et al. 2013, each 0.01 increase in EVI in a riparian area
# corresponds to an increased ET of 0.16 mm/day (58.4 mm/yr)
# see NaglerET product

# Groundwater

      # Alluvial:

alluv_levels <- read_csv("data/Near-stream_alluvial-aquifer_water_levels.csv",
                         skip = 2, col_names = T)

alluvial <- alluv_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%Y")),
            name = station_nm, lat = y, lon = x, level = lev_va) %>% 
  mutate(well = "alluvial")

filter(alluvial) %>% 
  ggplot(aes(x = date, y = -level, group = name))+
  geom_line()+
  facet_wrap(~name, scales = "free")

# alluvial_evi <- full_join(alluvial, evi) %>% 
#   filter(year(date) >= 2013, month(date) %in% c(4,5,6))

      # Rest of the wells:

regional_levels <- read_csv("data/Groundwater_levels_below_land_surface.csv")

regional <- regional_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%y")),
            name = station_nm, lat = y, lon = x, level = lev_va) %>% 
  mutate(well = "regional") %>% 
  filter(year(date) >= 2000, year(date) <= 2024)

filter(regional) %>% 
  ggplot(aes(x = date, y = -level, group = name))+
  geom_line()+
  facet_wrap(~name)

      # combine groundwater measurements

water_combo <- rbind(regional, alluvial)

      # plot gw status by wells

ggplot(water_combo, aes(x = date, y = level, group = well, color = well))+
  geom_smooth()

      # alluvial is much deeper

      # attach EVI

combined_evi <- full_join(water_combo, evi_combo)

stats <- combined_evi %>% # did I do this correctly?
  filter(month(date) == 4) %>% 
  group_by(well, name) %>% 
  summarise(evi_mean = mean(evi, na.rm = T), 
            evi_sd = sd(evi, na.rm = T),
            dtg_mean = mean(level, na.rm = T), 
            dtg_sd = sd(level, na.rm = T),
            evi_cv = evi_sd / evi_mean,
            dtg_cv = dtg_sd / dtg_mean
  ) %>% 
  ungroup()

z_scores <- left_join(combined_evi, stats) %>% 
  filter(!is.na(evi)) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd
  )

ggplot(filter(z_scores), aes(x = dtg_z,
                             y = evi_z))+
  geom_point(aes(color = well))+
  geom_smooth(method = "lm", se = F)

summary(lm(evi_z ~ dtg_z, z_scores))

# places where EVI has dropped from groundwater: priority restoration for recharge?
# decline in evi during a time period vs. decline in gw during a time period
# evi change vs. dtg change