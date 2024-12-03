library(tidyverse)
library(correlation)

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date))
evi <- read_csv("data/Processed/USP_EVI_Z_11132024.csv") %>% 
  mutate(date = date(date))

weather_evi <- full_join(weather, evi)

ppt_cor <- weather_evi %>% 
  select(well, evi, ppt, ppt_30d:ppt_90d) %>% 
  group_by(well) %>% 
  correlation(method = "pearson") %>% 
  filter(Parameter1 == "evi") %>% 
  mutate(days = case_when(
                          Parameter2 == "ppt_30d" ~ 30,
                          Parameter2 == "ppt_45d" ~ 45,
                          Parameter2 == "ppt_60d" ~ 60,
                          Parameter2 == "ppt_75d" ~ 75,
                          Parameter2 == "ppt_90d" ~ 90
  ))

ggplot(ppt_cor, aes(x = days, y = r,
                       group = Group))+
  geom_point(aes(color = Group))+
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.3)+
  geom_line(linetype = 2)+
  theme_light()
# 
# ggplot(weather_evi, aes(x = ppt_30d, y = evi))+
#   geom_point()
summary(lm(evi ~ ppt_60d, subset(weather_evi, well == "alluvial"))) # highest R2 of interactions and day types

vpd_cor <- weather_evi %>% 
  select(well, evi, vpdmax, vpd_5d:vpd_120d) %>% 
  group_by(well) %>% 
  correlation(method = "pearson") %>% 
  filter(Parameter1 == "evi") %>% 
  mutate(days = case_when(Parameter2 == "vpdmax" ~ 1,
                          Parameter2 == "vpd_5d" ~ 5,
                          Parameter2 == "vpd_10d" ~ 10,
                          Parameter2 == "vpd_20d" ~ 20,
                          Parameter2 == "vpd_30d" ~ 30,
                          Parameter2 == "vpd_45d" ~ 45,
                          Parameter2 == "vpd_60d" ~ 60,
                          Parameter2 == "vpd_75d" ~ 75,
                          Parameter2 == "vpd_90d" ~ 90,
                          Parameter2 == "vpd_105d" ~ 105,
                          Parameter2 == "vpd_120d" ~ 120
  ))

vpd_0p_cor <- weather_evi %>% 
  select(well, evi, vpdmax, vpd_5d:vpd_120d) %>% 
  group_by(well) %>% 
  correlation(method = "pearson") %>% 
  filter(Parameter1 == "evi") %>% 
  mutate(days = case_when(Parameter2 == "vpdmax" ~ 1,
                          Parameter2 == "vpd_5d" ~ 5,
                          Parameter2 == "vpd_10d" ~ 10,
                          Parameter2 == "vpd_20d" ~ 20,
                          Parameter2 == "vpd_30d" ~ 30,
                          Parameter2 == "vpd_45d" ~ 45,
                          Parameter2 == "vpd_60d" ~ 60,
                          Parameter2 == "vpd_75d" ~ 75,
                          Parameter2 == "vpd_90d" ~ 90,
                          Parameter2 == "vpd_105d" ~ 105,
                          Parameter2 == "vpd_120d" ~ 120
  ))

ggplot(vpd_cor, aes(x = as.numeric(days), y = r, 
                    group = Group))+
  geom_point(aes(color = Group))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high))+
  geom_line(linetype = 2)+
  theme_light()
