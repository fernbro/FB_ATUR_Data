library(tidyverse)
library(broom)

# function for calculating well temporal coverage:

# proportion of measurements for each well in each month

well_covg <- function(df){
  
  info <- df %>% 
    mutate(yr = year(date), mth = month(date)) %>%
    group_by(name, mth) %>% 
    summarise(yr_s = min(yr), yr_f = max(yr),
              msmts = n(), yr_dur = case_when(yr_s < 2000 ~ (yr_f - 2000)+1,
                                              yr_s >= 2000 ~ (yr_f - yr_s)+1))
  
  return(info)
  
}



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

# ggplot(filter(alluvial, year(date) %in% seq(2000,2024,by=1)), 
#        aes(x = date, y = level, group = name))+
# #   geom_point()+
# #   geom_smooth(method = "lm")
# # looks to be an increase of a mean of about 0.85 m per 24 years
# # 0.035 m per year (mean decline of 35 mm per year!!!)
# # well trends are variable...
# 
# alluv_covg <- well_covg(alluvial)
# 
# alluv_trends <- alluvial %>% 
#   filter(year(date) %in% seq(2000,2024,by=1)) %>% 
#   group_by(name) %>% 
#   reframe(tidy(lm(level ~ year(date)))) %>% 
#   filter(term == "year(date)", p.value <= 0.05)
# mean(alluv_trends$estimate) # increase in DTG of about 0.02 ft/yr (fifth of an inch)
# 0.0199 if only keeping significant trends
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

reg_trends <- regional %>% 
  filter(year(date) %in% seq(2000,2024,by=1)) %>% 
  group_by(name) %>% 
  reframe(tidy(lm(level ~ year(date)))) %>% 
  filter(term == "year(date)", p.value <= 0.05)
# "for wells with statistically significant (p <= 0.05) trends over time (2000 to 2024),
# rates of DTG change ranged from x to y"

min(reg_trends$estimate)
max(reg_trends$estimate)
median(reg_trends$estimate) # but what do i do with confidence intervals?
mean(reg_trends$estimate) # increase in DTG of about 0.39 ft/year (~4.7 inches) (am i supposed to avg the CI bounds?)
# 0.512 ft/year if only keeping significant trends
# maybe an average is not the parameter that i'm interested in
# give range of estimates and range of confidence intervals???

ggplot(filter(regional, year(date) %in% seq(2000,2024,by=1)), 
       aes(x = date, y = level, group = name))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~name, scales = "free_y")
# should average all of the rates calculated for each well (name)
# histogram of slope trends


## combine ##
# combine groundwater measurements

water_combo <- rbind(regional, alluvial) %>% 
  select(date, name, well, level) %>% 
  mutate(szn = case_when(month(date) %in% c(1, 2, 3) ~ "JFM",
                         month(date) %in% c(4, 5, 6) ~ "AMJ",
                         month(date) %in% c(7, 8, 9) ~ "JAS",
                         month(date) %in% c(10, 11, 12) ~ "OND"))

water_stats <- water_combo %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>% 
  filter(!is.na(level)) %>% 
  group_by(name, szn) %>% 
  summarise(dtg_mean = mean(level, na.rm = T),
            dtg_sd = sd(level, na.rm = T),
            dtg_cv = dtg_sd / dtg_mean) %>% 
  ungroup()

groundwater <- full_join(water_combo, water_stats) %>% 
  mutate(dtg_z = (level - dtg_mean)/dtg_sd)

ggplot(filter(groundwater, well == "Riparian"), 
       aes(x = date, y = level))+
  geom_point()+
  geom_line()+
  facet_wrap(~name, scales = "free")

groundwater$method <- "seasonal stats"

#write_csv(groundwater, "data/Processed/USP_GW_Zscores_11142024.csv")
write_csv(groundwater, "data/Processed/USP_GW_Zscores_Seasonal_01032025.csv")


min(subset(groundwater, well == "Riparian")$level)
max(subset(groundwater, well == "Riparian")$level)
min(subset(groundwater, well == "Upland")$level)
max(subset(groundwater, well == "Upland")$level)

# VISUALIZE:

ggplot(filter(groundwater, well == "Upland", 
              year(date) >= 2000), 
       aes(x = (date), y = dtg_z))+
  geom_point()+
  geom_smooth(method = 'lm')
summary(lm(dtg_z ~ year(date), data = filter(groundwater, well == "Upland", 
                                    year(date) >= 2000)))
# +
#   facet_wrap(~name, scales = "free")

ggplot(filter(groundwater, well == "Riparian", 
              year(date) >= 2000), 
       aes(x = (date), y = (dtg_z)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~name, scales = "free")

ggplot(groundwater, aes(x = well, y = (dtg_mean*0.3048)))+
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
