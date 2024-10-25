library(tidyverse)

flux_weather <- read_csv("data/Flux/Daily_Ameriflux_ET_Weather.csv") %>% 
  transmute(date = date, site = site,
            ET = ET, tmean_f = tmean, vpdmax_f = vpdmax,
            ppt_f = ppt) # _f: from Flux tower 

flux_prism <- read_csv("data/AZ_Flux_PRISM.csv") %>% 
  transmute(site = str_sub(name, 4, 6),
         date = date(date),
         tmean_p = tmean, vpdmax_p = vpdmax, 
         ppt_p = ppt) #_p: from Prism
  
full_flux_w <- full_join(flux_weather, flux_prism, by = join_by(site, date))

san_pedro <- c("CMW", "Wkg", "Whs", "LS1", "LS2")

ggplot(filter(full_flux_w, site %in% san_pedro), aes(x = ppt_p, y = ppt_f))+
  geom_point(aes(color = month(date)))+
  geom_smooth(method = "lm")+
  geom_abline(a = 1, b = 0, color = "red")+
  theme_light()+
  facet_wrap(~site)

summary(lm(vpdmax_f ~ vpdmax_p, full_flux_w)) # R2 = 0.8107
summary(lm(ppt_f ~ ppt_p, filter(full_flux_w, ppt_f < 100))) # R2 = 0.1079
summary(lm(tmean_f ~ tmean_p, full_flux_w)) # R2 = 0.9045
