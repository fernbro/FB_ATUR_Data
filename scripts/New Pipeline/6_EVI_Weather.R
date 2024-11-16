library(tidyverse)
library(broom)

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date))
evi <- read_csv("data/Processed/USP_EVI_Z_11132024.csv") %>% 
  mutate(date = date(date))

weather_evi <- full_join(weather, evi) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland")) %>% 
  filter(!is.na(month)) # where do the NA months even come from anyway?

# Site-based analysis

filter(weather_evi, well == "Riparian") %>% 
ggplot(aes(x = vpd_105d, y = evi))+
  geom_point()+
  facet_wrap(~name)

# Correlation Analysis

correlations_vpd <- weather_evi %>% 
  group_by(well, month) %>% 
  summarise(cor = (cor.test(evi, cum_vpd_30d))$estimate,
            ci_lo = (cor.test(evi, cum_vpd_30d))$conf.int[1],
            ci_up = (cor.test(evi, cum_vpd_30d))$conf.int[2],
            p = (cor.test(evi, cum_vpd_30d))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "p <= 0.05",
                         p > 0.05 ~ " p > 0.05"))

correlations_vpd_0p <- weather_evi %>% 
  filter(cum_ppt_30d == 0, !is.na(month)) %>% 
  group_by(well, month) %>% 
  summarise(cor = (cor.test(evi, vpd_105d))$estimate,
            ci_lo = (cor.test(evi, vpd_105d))$conf.int[1],
            ci_up = (cor.test(evi, vpd_105d))$conf.int[2],
            p = (cor.test(evi, vpd_105d))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "p <= 0.05",
                         p > 0.05 ~ " p > 0.05"))

ggplot(correlations_vpd_0p, aes(x = as.factor(month), y = cor, shape = sig))+
  geom_hline(yintercept = 0, color = "gray60")+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  theme_light(base_size = 20)+
  labs(x = "Month", y = "Correlation coefficient", 
       color = "Well location", shape = "p(r ≠ 0)")

# 
