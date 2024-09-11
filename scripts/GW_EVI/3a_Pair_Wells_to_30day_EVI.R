library(tidyverse)
library(zoo)


# EVI Measurements

# alluvial wells:

evi_alluv1 <- read_csv("data/EVI_alluvial_2013_2023.csv")
evi_alluv2 <- read_csv("data/EVI_alluvwells_2000_2013.csv")

# regional aquifer wells:
evi_gen1 <- read_csv("data/EVI_2013_2023_general.csv")
evi_gen2 <- read_csv("data/EVI_generalwells_2000_2013.csv")

# spikes and cloud mask???

evi_ts_alluvial <- rbind(evi_alluv1, 
                         evi_alluv2) # bind alluvial evi time series

evi <- evi_ts_alluvial %>%  # alluvial aquifer wells
  mutate(date = make_date(year = year, 
                          month = month, 
                          day = day),
         evi = EVI, 
         well = "alluvial") %>% 
  select(name, evi, date, well)

evi_ts_general <- rbind(evi_gen1, 
                        evi_gen2) # bind regional aqu time series

evi2 <- evi_ts_general %>% # regional aquifer wells
  mutate(date = make_date(year = year, 
                          month = month, 
                          day = day),
         evi = EVI, 
         well = "regional") %>% 
  select(name, evi, date, well)

evi_combo <- rbind(evi, evi2) # combine now that they're labeled



# Groundwater Measurements

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
  mutate(well = "alluvial")

regional_levels <- read_csv("data/Groundwater_levels_below_land_surface.csv")

regional <- regional_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%y")),
            name = station_nm, lat = y, lon = x, level = lev_va) %>% 
  mutate(well = "regional") %>% 
  filter(year(date) >= 2000, year(date) <= 2024)

# combine groundwater measurements

water_combo <- rbind(regional, alluvial) %>% 
  select(date, name, well, level)

# combined_evi <- full_join(water_combo, evi_combo, 
#                           join_by(name, date, well))

# for each groundwater observation (on a date and name),
# pull the 30-day evi average from the evi_combo df
# and join it
# time to write a function....

# for a data frame of wells, dates and evi values:
# group by well
# filter dataframe by window of dates (need to loop through dates?)
# average EVI in the window
# make that a value in a new column!

# for(x in water_combo$date){
#   
#   mean(filter(evi_combo, date %in% seq(x - 15, x + 15, by = 1)) %>% 
#     select(evi))
#   
# }


# rolling means:

# evi_smooth1 <- rollmean(evi_combo[,"evi"], 31, c(NA, NA, NA))
# colnames(evi_smooth1) <- "avg_evi"
# evi_smooth <- cbind(evi_combo, evi_smooth1)

evi_smooth <- evi_combo %>% 
  group_by(name) %>% 
  arrange(name, date) %>% 
  mutate(evi_30 = zoo::rollmean(evi, 31, c(NA, NA, NA))) %>% 
  ungroup()

ggplot(filter(evi_smooth, year(date) == 2015),
       aes(x = date,
           y = evi_30,
           group = name))+
  geom_line()+
  geom_line(color = "red", aes(y = evi))+
  facet_wrap(~well)
