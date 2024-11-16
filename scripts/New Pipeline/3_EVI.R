library(tidyverse)
# install.packages("zoo")
# library(zoo)
# library(lme4)
# install.packages("lmerTest")
# library(lmerTest)
# library(ggthemes)
# library(broom)
options(scipen = 99999)
# library(viridis)


# EVI Measurements

# alluvial wells:

wells_by_reach <- read_csv("data/SPRNCA/Wells_Reaches.csv")

# MODIS EVI:
# evi_alluv1 <- read_csv("data/EVI_alluvial_2013_2023.csv")
# evi_alluv2 <- read_csv("data/EVI_alluvwells_2000_2013.csv")

# MODIS from MOD09GA:

evi_mod_a <- read_csv("data/MOD09GA_EVI_alluvial.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 10)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y_%m_%d"),
            evi = evi, name = name, well = "alluvial")

# regional aquifer wells:

# MODIS EVI:
# evi_gen1 <- read_csv("data/EVI_2013_2023_general.csv")
# evi_gen2 <- read_csv("data/EVI_generalwells_2000_2013.csv")

# MODIS from MOD09GA:

evi_mod_r <- read_csv("data/MOD09GA_EVI_regional.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 10)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y_%m_%d"),
            evi = evi, name = name, well = "regional")

# spikes and cloud mask???


# For MODIS:
# evi_ts_alluvial <- rbind(evi_alluv1, 
#                          evi_alluv2) # bind alluvial evi time series
#   
# evi <- evi_ts_alluvial %>%  # alluvial aquifer wells
#   mutate(date = make_date(year = year, 
#                           month = month, 
#                           day = day),
#          evi = EVI, 
#          well = "alluvial") %>% 
#   select(name, evi, date, well)
# 
# evi_ts_general <- rbind(evi_gen1, 
#                         evi_gen2) # bind regional aqu time series
# 
# evi2 <- evi_ts_general %>% # regional aquifer wells
#   mutate(date = make_date(year = year, 
#                           month = month, 
#                           day = day),
#          evi = EVI, 
#          well = "regional") %>% 
#   select(name, evi, date, well)
# 
# evi_combo <- rbind(evi, evi2) # combine now that they're labeled

evi_mod09 <- rbind(evi_mod_a, evi_mod_r)

evi_ann_daily <- evi_mod09 %>% 
  mutate(doy = yday(date)) %>% 
  group_by(well, doy) %>% 
  summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T)) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

evi_stats <- evi_mod09 %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>%  # use only complete calendar years
  group_by(name) %>% 
  summarise(evi_mean = mean(evi, na.rm = T),
            evi_sd = sd(evi, na.rm = T)) %>% 
  ungroup()

# %>% 
#   mutate(evi_z = (evi - evi_mean)/evi_sd,
#          month = month(date))

z_model <- full_join(evi_mod09, evi_stats) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         month = month(date))

z_model$method <- "obs relative to whole year - MOD09 daily EVI"

write_csv(z_model, "data/Processed/USP_EVI_Z_11132024.csv")
