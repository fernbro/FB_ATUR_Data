library(tidyverse)

# Well pairings:
well_pairings <- read_csv("data/Processed/USP_AlluvialWells_StreamPerm_Pairings.csv")

#Z scores, EVI and GW values, and weather variables
total_z <- read_csv("data/Processed/GW_EVI_Weather.csv")


# Stream permanence: raw data
stream_perm_raw <- read_csv("data/Streamflow_permanence.csv", skip = 1) %>% 
  filter(no_data <= 100) # filter out years where there are at least 100 days of missing data

stream_perm_yrly <- stream_perm_raw %>% # averages permanence across all sites for each year
  group_by(year) %>% 
  summarise(avg_perm = mean(percent_flowing))

stream_perm_sites <- stream_perm_raw %>% # calculates temporal stats for each site
  group_by(site, x, y) %>% 
  summarise(avg_perm = mean(percent_flowing), sd_perm = sd(percent_flowing),
            start = min(year), end = max(year))

stream_perm <- stream_perm_raw %>% 
  group_by(site, year) %>% 
  select(site, year, percent_flowing) %>% 
  rename(perm_name = site) %>% 
  full_join(well_pairings) %>% 
  rename(name = well_name)

stream_perm_avgs <- stream_perm_raw %>% 
  select(site, percent_flowing, year) %>% 
  group_by(site) %>% 
  summarise(avg_perm = mean(percent_flowing),
            start_yr = min(year), end_yr = max(year)) %>% 
  rename(perm_name = site) %>% 
  select(perm_name, avg_perm, start_yr, end_yr)
# now join the evi data and the formatted streamflow data:

evi_gw_stream <- full_join(total_z, stream_perm, join_by(year, name)) %>% 
  filter(!is.na(percent_flowing)) %>%
  full_join(stream_perm_avgs) %>% 
  filter(year >= start_yr & year <= end_yr)

summary(vpd_model1 <- lm(evi ~ cum_vpd_30d, filter(total_z, well == "Riparian", cum_ppt_30d == 0))) # 0.302

summary(vpd_model2 <- lm(evi ~ cum_vpd_30d, 
                         filter(evi_gw_stream, well == "Riparian", 
                                cum_ppt_30d == 0))) #0.4836 w/ stream permanence

stripchart(dtg_z ~ perm_name, evi_gw_stream, vertical = T)
sf_model <- lm(evi ~ cum_ppt_30d*percent_flowing, evi_gw_stream)
summary(sf_model) 

# not evidence of an interactive relationship between precipitation and the streamflow

sf_vpd_model <- lm(evi ~ cum_vpd_30d*percent_flowing, filter(evi_gw_stream, cum_ppt_30d == 0)) # streamflow doesn't interact with vpd either
summary(sf_vpd_model)
vpd_model <- lm(evi ~ cum_vpd_30d, filter(evi_gw_stream, cum_ppt_30d == 0))
summary(vpd_model)

sf_avg_model <- lm(evi ~ cum_ppt_30d*avg_perm, evi_gw_stream)
summary(sf_avg_model)

zero_ppt <- filter(evi_gw_stream, cum_ppt_30d == 0)

sf_avg_vpd <- lm(evi ~ cum_vpd_30d*avg_perm, zero_ppt)
summary(sf_avg_vpd)

zero_ppt$predicted<- predict(sf_avg_vpd)

ggplot(zero_ppt, aes(x = cum_vpd_30d, y = evi))+
  geom_point(color = "orange")+
  geom_smooth(method = "lm", linewidth = 0.5, alpha = 0.5)+
  # geom_point(aes(x = cum_vpd_30d, y = predicted), color = "green")+
  theme_light()
summary(aov(dtg_z ~ perm_name, zero_ppt))

ggplot(zero_ppt, aes(x = cum_vpd_30d, y = evi))+
  geom_point(aes(color = dtg_z))+
  geom_smooth(method = "lm")+
  facet_wrap(~perm_name)
