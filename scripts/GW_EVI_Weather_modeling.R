library(tidyverse)

z_scores <- read_csv("data/Processed/USP_GW_EVI_Z.csv") %>% 
  mutate(date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T)))

ppt <- read_csv("data/USP_AlluvialWells_PRISM_ppt.csv")
tmean <- read_csv("data/USP_AlluvialWells_PRISM_tmean.csv")
vpdmax <- read_csv("data/USP_AlluvialWells_PRISM_vpdmax.csv")

weather <- full_join(ppt, tmean, join_by(name, date)) %>% 
  full_join(vpdmax, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, 
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T)))

# UNITS:
# ppt: mm per day
# tmean: celsius
# vpdmax: hPa (10^2 pascals); converted to kPa

weather_z <- inner_join(z_scores, weather, join_by(name, date))

summary(lm(evi_z ~ dtg_z + vpdmax, weather_z))

ggplot(filter(weather_z), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = vpdmax))+
  scale_colour_gradient(low = "deepskyblue", high="red")+
  geom_smooth(method = "lm", se = T)+
  theme_light()+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  ggtitle("JAS EVI vs. DTG z-scores relative to entire year")
