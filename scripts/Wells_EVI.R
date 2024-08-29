library(tidyverse)
# install.packages("zoo")
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

# plot of evi, time series for each well (colored by its aquifer type)
ggplot(filter(evi_combo), 
       aes(x = date, 
           y = avg_evi,
           group = name))+
  geom_line()+
  facet_wrap(~well)+
  theme(legend.position = "none")

# let's do some smoothing

# rolling means:

evi_smooth1 <- rollmean(evi_combo[,"evi"], 28, c(NA, NA, NA))
colnames(evi_smooth1) <- "avg_evi"
evi_smooth <- cbind(evi_combo, evi_smooth1)



# according to Nagler et al. 2013, each 0.01 increase in EVI in a riparian area
# corresponds to an increased ET of 0.16 mm/day (58.4 mm/yr)
# (see NaglerET product)

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
  mutate(well = "alluvial")

filter(alluvial, year(date) == 2016) %>% 
  ggplot(aes(x = date, y = -level, group = name))+
  geom_point(size = 0.1)+
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

      # combine groundwater measurements

water_combo <- rbind(regional, alluvial) %>% 
  select(date, name, well, level)

      # plot gw status by wells

ggplot(water_combo, aes(x = date, y = level, 
                        group = name, color = well))+
  geom_line()

      # alluvial is much deeper

      # attach EVI

combined_evi <- full_join(water_combo, evi_combo, 
                          join_by(name, date, well))

# compute average spring (dry season) EVI and groundwater levels
stats <- combined_evi %>% # did I do this correctly?
  filter(!is.na(evi), !is.na(level),
         month(date) %in% c(7, 8, 9)) %>%
  group_by(well, name) %>% 
  summarise(evi_mean = mean(evi, na.rm = T), 
            evi_sd = sd(evi, na.rm = T),
            dtg_mean = mean(level, na.rm = T), 
            dtg_sd = sd(level, na.rm = T),
            evi_cv = evi_sd / evi_mean,
            dtg_cv = dtg_sd / dtg_mean
  ) %>% 
  ungroup()

z_scores <- full_join(combined_evi, stats) %>% 
  filter(!is.na(evi), !is.na(level),
         month(date) %in% c(7, 8, 9)) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd)

ggplot(filter(z_scores, evi_z > -5),
       aes(x = dtg_z,
                             y = evi_z))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~name, scales = "free")

summary(lm(evi_z ~ dtg_z, filter(z_scores)))
  # slope is significantly different from 0
cor(z_scores$evi_z, z_scores$dtg_z) # positive correlation (0.08)

summary(lm(evi_z ~ dtg_z, filter(z_scores, well == "regional")))
  # slope is NOT significantly different from 0

# places where EVI has dropped from groundwater: priority restoration for recharge?
# decline in evi during a time period vs. decline in gw during a time period
# evi change vs. dtg change

# import vegetation data

vegetation <- rbind(alluvial_landfire, regional_landfire) # alluvial + reg

z_scores <- full_join(z_scores, vegetation) %>% 
  mutate(year = year(date)) %>% 
  full_join(usp_spi_annual, join_by(year)) %>% 
  mutate(drought_year = case_when(drought >= 50 ~ "yes", .default = "no"))

# grepl("Wash", lc2016) 
#| grepl("Riparian", lc2016)
#| grepl("Ruderal", lc2016),
ggplot(filter(z_scores_nca, 
               #evi_z > -5, dtg_z < 10, 
              well == "alluvial"), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = drought))+
  geom_smooth(method = "lm", se = F)+
  theme_light()+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  ggtitle("JAS EVI vs. DTG z-scores relative to JAS")+
  facet_wrap(~sprnca)
ggsave("figures/JASvsJAS_SPRNCA.jpg", width = 10, height = 8, units = "in")

summary(lm(evi_z ~ dtg_z, filter(z_scores, 
                                 evi_z > -5, dtg_z < 10, 
                                 well == "alluvial")))

summary(lm(evi_z ~ dtg_z, filter(z_scores_nca,
                                 sprnca == "SUM",
                                  well == "alluvial"))) # p < 0.001


evi_annual <- evi_combo %>% 
  mutate(year = year(date)) %>%
  filter(year != 2000 & year != 2023) %>% 
  group_by(well, year) %>% 
  summarise(evi = mean(evi, na.rm = T)) %>% 
  inner_join(usp_spi_monsoon, join_by(year))

ggplot(filter(evi_annual), 
       aes(x = year, 
           y = evi))+
  geom_line()+
  geom_point(aes(color = drought), size = 3)+
  facet_wrap(~well)+
  xlab("Year")+
  ylab("Average EVI")+
  theme(legend.position = "none")+
  theme_light()+
  scale_colour_gradient(low = "deepskyblue", high="red")
#ggsave("figures/AnnualEVI_Drought.jpg", width = 7, height = 3, units = "in")
