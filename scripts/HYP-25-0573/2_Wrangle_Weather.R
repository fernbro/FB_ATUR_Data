library(tidyverse)
# install.packages("RcppRoll")
library(RcppRoll)
library(viridis)

# Read in PRISM data extracted for riparian wells
ppt1 <- read_csv("data/USP_AlluvialWells_PRISM_ppt.csv") 
tmean1 <- read_csv("data/USP_AlluvialWells_PRISM_tmean.csv") 
vpdmax1 <- read_csv("data/USP_AlluvialWells_PRISM_vpdmax.csv")

# Combine dataframes
weather1 <- full_join(ppt1, tmean1, join_by(name, date)) %>% 
  full_join(vpdmax1, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, # convert hPa to kPa
         well = "alluvial", # create well column so that we can bind with the regional well data
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T))) # remove time from dates

# Read in PRISM data extracted for upland wells
ppt2 <- read_csv("data/USP_RegionalWells_PRISM_ppt.csv")
tmean2 <- read_csv("data/USP_RegionalWells_PRISM_tmean.csv")
vpdmax2 <- read_csv("data/USP_RegionalWells_PRISM_vpdmax.csv")

weather2 <- full_join(ppt2, tmean2, join_by(name, date)) %>% 
  full_join(vpdmax2, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, # convert hPa to kPa
         well = "regional", # create well column so that we can bind with the riparian well data
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T))) # remove time from dates


weather <- rbind(weather1, weather2) # bind rows of the riparian and alluvial weather data
# mutate(year = year(date)) %>% # add year column
# group_by(name) %>% 
# arrange(date) %>% # for each well, we are gonna arrange by date to create rolling 30-day sums n such
# mutate(cum_vpd_10d = RcppRoll::roll_sum(vpdmax, n = 10, 
#                                         fill = NA, align = "right"),  # 10 day cumulative daily max VPD (kPa)
#        tmean_10d = RcppRoll::roll_mean(tmean, n = 10, 
#                                        fill = NA, align = "right"), # 10 day cumulative mean temp (C)
#        cum_ppt_10d = RcppRoll::roll_sum(ppt, n = 10, fill = NA, # 10 day cumulative precipitation (mm)
#                                         align = "right"),
#        max_vpd_10d = RcppRoll::roll_max(vpdmax, n = 10, fill = NA, # 10 day maximum daily max VPD (kPa)
#                                         align = "right"),
#        cum_vpd_30d = RcppRoll::roll_sum(vpdmax, n = 30, # 30 day cumulative daily max VPD (kPa)
#                                         fill = NA, align = "right"),
#        tmean_30d = RcppRoll::roll_mean(tmean, n = 30, # 30 day mean daily mean T (C)
#                                        fill = NA, align = "right"),
#        cum_ppt_30d = RcppRoll::roll_sum(ppt, n = 30, fill = NA, # 30 day cumulative precip (mm)
#                                         align = "right"),
#        cum_tmean_30d = RcppRoll::roll_sum(tmean, n = 30, fill = NA, # 30 day cumulative daily mean temperature (C)
#                                           align = "right"),
#        max_vpd_30d = RcppRoll::roll_max(vpdmax, n = 30, fill = NA, # 30 day maximum daily max VPD (kPa)
#                                         align = "right"),
#        max_tmean_30d = RcppRoll::roll_max(tmean, n = 30, fill = NA, # 30 day maximum mean temperature (C)
#                                           align = "right"),
#        max_tmean_10d= RcppRoll::roll_max(tmean, n = 10, fill = NA, # 10 day maximum mean temperature (C)
#                                          align = "right")) %>% 
# ungroup()

# colors = c("red", "orange", "yellow", "green", "blue", "purple")
# ggplot(filter(weather, month(date) %in% c(4, 5, 6, 7, 8, 9)) %>%   
#          mutate(well = case_when(well == "alluvial" ~ "Riparian",
#                                  well == "regional" ~ "Upland")), 
#        aes(y = vpdmax, x = as.factor(month(date)),
#            fill = as.factor(month(date))))+
#   geom_boxplot()+
#   theme_light(base_size = 26)+
#   scale_fill_manual(values = colors,
#                     labels = c("April", "May", "June",
#                                "July", "August", "September"))+
#   labs(fill = "Month")+
#   xlab("Month")+
#   ylab("Daily max VPD")+
#   theme(legend.position = "none")+
#   facet_wrap(~well)+
#   theme(strip.background = element_rect(color = "black", fill = "white"))+
#   theme(strip.text = element_text(colour = 'black'))

# dplyr cumsum()

# UNITS:
# ppt: mm per day
# tmean: celsius
# vpdmax: hPa (10^2 pascals); converted to kPa

# create some weather variables that accumulate more time than just Day Of vals:

# rolling 10 day sum of precipitation?
# use site as random effect for a differential response to VPD

weather_ann_daily <- weather %>%
  mutate(doy = yday(date)) %>% 
  group_by(well, doy) %>% 
  summarise(n = n(), mean_vpdmax = mean(vpdmax, na.rm = T), sd_vpdmax = sd(vpdmax, na.rm = T),
            mean_ppt = mean(ppt, na.rm = T), sd_ppt = sd(ppt, na.rm = T),
            mean_tmean = mean(tmean, na.rm = T), sd_tmean = sd(tmean, na.rm = T)) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

# ggplot()+
#   geom_line(data = weather_ann_daily, linetype = 1,
#             linewidth = 0.5,
#             aes(x = doy, y = mean_vpdmax, color = well))+
#   geom_ribbon(data = weather_ann_daily, alpha = 0.2, 
#               aes(fill = well, x = doy, y = mean_vpdmax, 
#                   ymin = mean_vpdmax - sd_vpdmax,
#                   ymax = mean_vpdmax + sd_vpdmax))+
#   geom_line(data = evi_ann_daily, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
#             linewidth = 1.2,
#             aes(x = doy, y = mean_evi*10, color = well))+
#   geom_ribbon(data = evi_ann_daily, alpha = 0.2, 
#               aes(fill = well, x = doy, y = mean_evi*10, 
#                   ymin = 10*(mean_evi - sd_evi),
#                   ymax = 10*(mean_evi + sd_evi)))+
#   theme_light(base_size = 20)+
#   labs(x = "DOY", y = "Mean daily maximum VPD (kPa) / Mean daily EVI x 10")
# 
# ggplot()+
#   geom_line(data = weather_ann_daily, linetype = 1,
#             linewidth = 0.5,
#             aes(x = doy, y = mean_vpdmax, color = well))+
#   geom_ribbon(data = weather_ann_daily, alpha = 0.2, 
#               aes(fill = well, x = doy, y = mean_vpdmax, 
#                   ymin = mean_vpdmax - sd_vpdmax,
#                   ymax = mean_vpdmax + sd_vpdmax))+
#   theme_light(base_size = 20)+
#   labs(x = "DOY", y = "Mean daily maximum VPD (kPa)")
# # ggsave("figures/vpd_anncycle.jpg", last_plot(), height = 5, width = 7, units = "in")
# ggplot()+
#   geom_line(data = evi_ann_daily, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
#             linewidth = 0.5,
#             aes(x = doy, y = mean_evi, color = well))+
#   geom_ribbon(data = evi_ann_daily, alpha = 0.2, 
#               aes(fill = well, x = doy, y = mean_evi*10, 
#                   ymin = (mean_evi - sd_evi),
#                   ymax = (mean_evi + sd_evi)))+
#   theme_light(base_size = 20)+
#   labs(x = "DOY", y = "Mean daily EVI")
# ggsave("figures/evi_anncycle.jpg", last_plot(), height = 5, width = 7, units = "in")

monthly_weather <- weather %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(well, name, year, month) %>% 
  summarise(ppt = sum(ppt), tmean = mean(tmean, na.rm = T),
            vpdmax = mean(vpdmax, na.rm = T))

# we want to understand antecedent moisture conditions.
# prev. water year's precipitation; calculate precipitation 
# precipitation up to date in z score dataframe

'%!in%' <- function(x,y)!('%in%'(x,y))

# weather year precipitation prior to monsoon:
weather_wy <- weather %>%
  mutate(water_year = case_when(month(date) >= 10 ~ year(date) + 1,
                                .default = year(date))) %>% 
  filter(water_year > 2000, water_year < 2024) %>% # we don't have data for 1999 so WY2000 is incomplete, and WY 2024 isn't over in this data download
  group_by(well, name, water_year) %>% 
  filter(month(date) %!in% c(7, 8, 9)) %>% # exclude july, august, and september: summing total rain from oct-june
  summarise(pre_mnsn_ppt = sum(ppt))

# Calculate cumulative sums for Water Year (using October-September as per AZ state definition) and rolling sums
weather_annual_cumul <- weather %>% 
  mutate(water_year = case_when(month(date) >= 10 ~ year(date) + 1,
                                .default = year(date))) %>%  # create water year column
  filter(water_year > 2000, water_year < 2024) %>%  # remove WYs 2000 and 2024
  arrange(date) %>% # order by the date
  group_by(name) %>% # group by name
  # compute rolling sums:
  mutate(cum_vpd_30d = RcppRoll::roll_sum(vpdmax, n = 30, 
                                          fill = NA, align = "right"),
         cum_ppt_30d = RcppRoll::roll_sum(ppt, n = 30, fill = NA, 
                                          align = "right"),
         vpd_5d = RcppRoll::roll_sum(vpdmax, n = 5, 
                                     fill = NA, align = "right"),
         vpd_10d = RcppRoll::roll_sum(vpdmax, n = 10, 
                                      fill = NA, align = "right"),
         vpd_20d = RcppRoll::roll_sum(vpdmax, n = 20, 
                                      fill = NA, align = "right"),
         vpd_30d = RcppRoll::roll_sum(vpdmax, n = 30, 
                                      fill = NA, align = "right"),
         vpd_45d = RcppRoll::roll_sum(vpdmax, n = 45, 
                                      fill = NA, align = "right"),
         vpd_60d = RcppRoll::roll_sum(vpdmax, n = 60, 
                                      fill = NA, align = "right"),
         vpd_75d = RcppRoll::roll_sum(vpdmax, n = 75, 
                                      fill = NA, align = "right"),
         vpd_90d = RcppRoll::roll_sum(vpdmax, n = 90, 
                                      fill = NA, align = "right"),
         vpd_105d = RcppRoll::roll_sum(vpdmax, n = 105, 
                                      fill = NA, align = "right"),
         vpd_120d = RcppRoll::roll_sum(vpdmax, n = 120, 
                                      fill = NA, align = "right"),
         ppt_5d = RcppRoll::roll_sum(ppt, n = 5, 
                                     fill = NA, align = "right"),
         ppt_10d = RcppRoll::roll_sum(ppt, n = 10, 
                                      fill = NA, align = "right"),
         ppt_20d = RcppRoll::roll_sum(ppt, n = 20, 
                                      fill = NA, align = "right"),
         ppt_30d = RcppRoll::roll_sum(ppt, n = 30, 
                                      fill = NA, align = "right"),
         ppt_45d = RcppRoll::roll_sum(ppt, n = 45, 
                                      fill = NA, align = "right"),
         ppt_60d = RcppRoll::roll_sum(ppt, n = 60, 
                                      fill = NA, align = "right"),
         ppt_75d = RcppRoll::roll_sum(ppt, n = 75, 
                                      fill = NA, align = "right"),
         ppt_90d = RcppRoll::roll_sum(ppt, n = 90, 
                                      fill = NA, align = "right")
  ) %>% 
  ungroup() %>%  # reset groups
  group_by(name, water_year) %>% # now compute water year rolling sums
  mutate(cum_maxvpd = cumsum(vpdmax), cum_ppt = cumsum(ppt))  %>% 
  ungroup()

write_csv(weather_annual_cumul, "data/Processed/Weather_Cumulative.csv")