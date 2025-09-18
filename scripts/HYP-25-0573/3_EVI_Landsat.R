library(tidyverse)
options(scipen = 99999)
library(parallel)
detectCores()
# library(viridis)


# EVI Measurements

# alluvial wells:

wells_by_reach <- read_csv("data/SPRNCA/Wells_Reaches.csv")


# Landsat EVI from LANDSAT/COMPOSITES/C02/T1_L2_8DAY_EVI

rip_evi <- read_csv("data/Landsat_EVI/LS_Comp_EVI_Riparian_2000-2024.csv")  %>% 
  mutate(date1 = str_sub(`system:index`, 1, 8)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y%m%d"),
            evi = EVI, name = name, well = "Riparian")
rip_wells <- unique(rip_evi$name)

up_evi <- read_csv("data/Landsat_EVI/LS_Comp_EVI_Upland_2000-2024.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 8)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y%m%d"),
            evi = EVI, name = name, well = "Upland")
up_wells <- unique(up_evi$name)

# well locations:


# combining stuff for weather modeling:
evi_lsc <- rbind(rip_evi, up_evi) %>% 
  mutate(szn = case_when(month(date) %in% c(1, 2, 3) ~ "JFM",
                         month(date) %in% c(4, 5, 6) ~ "AMJ",
                         month(date) %in% c(7, 8, 9) ~ "JAS",
                         month(date) %in% c(10, 11, 12) ~ "OND"))

evi_stats <- evi_lsc %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>%  # use only complete calendar years
  group_by(well, name, szn) %>% 
  summarise(evi_mean = mean(evi, na.rm = T),
            evi_sd = sd(evi, na.rm = T)) %>% 
  ungroup()

z_model <- full_join(evi_lsc, evi_stats) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         month = month(date))

z_model$method <- "Landsat composites - seasonal averages"
z_model$szn <- factor(z_model$szn, levels = c("JFM", "AMJ", "JAS", "OND"))

write_csv(z_model, "data/Processed/USP_EVI_Z_Seasonal_Landsat_09182025")

# now i'll interpolate lol
# but the z-scores are still computed based on observed data ONLY. 
# raw and z-score values are interpolated....

# Now interpolate for GW comparisons

all_study_dates <- seq(as.POSIXct("2000-01-01"), as.POSIXct("2024-12-26"), by = "day") %>% 
  data.frame()
colnames(all_study_dates) <- "date"

z_dates <- z_model %>% 
  select(name, date, evi) %>% 
  group_by(name) %>% 
  complete(date=seq(min(date), max(date), by = "1 day"))

z_interp <- z_dates %>% 
  group_by(name) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(
    evi_int = ifelse(
      is.na(evi), # only interpolate where evi is NA
      approx(x = date, y = evi, xout = date, rule = 1)$y, # rule=1 does not allow extrapolation
      evi
    )
  ) %>%
  ungroup()

z_gw <- z_interp %>% 
  mutate(evi = evi_int) %>% 
  select(-evi_int) %>% 
  mutate(szn = case_when(month(date) %in% c(1, 2, 3) ~ "JFM",
                         month(date) %in% c(4, 5, 6) ~ "AMJ",
                         month(date) %in% c(7, 8, 9) ~ "JAS",
                         month(date) %in% c(10, 11, 12) ~ "OND")) %>% 
  mutate(well = case_when(name %in% rip_wells ~ "Riparian",
                          name %in% up_wells ~ "Upland")) %>% 
  full_join(evi_stats) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         month = month(date))

# only thing different is that EVI got interpolated.
# i rebound the stats so that i could compute z-scores based on those interpolations.
# yippee!
write_csv(z_gw, "data/Processed/USP_EVI_Z_Seasonal_Landsat_09182025_Interpolated")

####################

ggplot(filter(z_gw, year(date) == 2006), aes(x = date, y = evi))+
  geom_line(aes(group = name, color = well), alpha = 0.4)+
  geom_smooth(method = "gam", aes(color = well))

###################

