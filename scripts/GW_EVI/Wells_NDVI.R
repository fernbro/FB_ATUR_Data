library(tidyverse)
# install.packages("zoo")
library(zoo)
options(scipen = 99999)

ndvi_ts <- read_csv("data/L8_2013_2024.csv")
#ndvi_ts <- read_csv("data/NDVI_time_series.csv")

ndvi <- ndvi_ts %>% 
  mutate(date = make_date(year = year, month = month, day = day),
         ndvi = NDVI) %>% 
  select(name, ndvi, date) %>% 
  filter(date > "2013-04-10")

# create df of all dates in landsat 8 observation period
full_time <- seq(min(ndvi$date), max(ndvi$date), by = 'day')

# expand the ndvi df to include all these dates

ndvi_full <- ndvi %>% 
  group_by(name) %>% 
  complete(date = full_time) %>% 
  ungroup()

# interpolate (will fill in now that all needed dates are present)

ndvi_inter <- ndvi_full %>% 
  group_by(name) %>% 
  mutate(ndvi = na.approx(ndvi, na.rm = F))

ggplot(ndvi_inter, aes(x = date, y = ndvi, group = name))+
  geom_line()+
  facet_wrap(~name, scales = "free")

# levels for these aquifers:

alluv_levels <- read_csv("data/Near-stream_alluvial-aquifer_water_levels.csv",
                         skip = 2, col_names = T)

alluvial <- alluv_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%Y")),
            name = station_nm, lat = y, lon = x, level = lev_va)

filter(alluvial, year(date) > 2013) %>% 
ggplot(aes(x = date, y = -level, group = name))+
  geom_line()+
  facet_wrap(~name, scales = "free")

# exact date matches... are not a lot.. not a lot of rpt msmsts for sites
# interpolation? (use same methodology as MODIS??)

# could either filter sites in terms of data continuity or utilize the paucity..

# sample size is too low

alluvial_ndvi <- full_join(alluvial, ndvi_inter) %>% 
  filter(year(date) >= 2013)

# filter to dry months (April, May, and June)

dry_months <- filter(alluvial_ndvi)
#, month(date) %in% c(4,5,6)

alluvial_stats <- dry_months %>% 
  group_by(name) %>% 
  summarise(ndvi_mean = mean(ndvi, na.rm = T), 
            ndvi_sd = sd(ndvi, na.rm = T),
            dtg_mean = mean(level, na.rm = T), 
            dtg_sd = sd(level, na.rm = T)
            ) %>% 
  ungroup()

# bind back to the other DF to compute z-scores

z_scores <- left_join(dry_months, alluvial_stats) %>% 
  filter(!is.na(ndvi)) %>% 
  mutate(ndvi_z = (ndvi - ndvi_mean)/ndvi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd
  )

ggplot(z_scores, aes(x = dtg_z, y = ndvi_z))+
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(ndvi_z ~ dtg_z, z_scores))
