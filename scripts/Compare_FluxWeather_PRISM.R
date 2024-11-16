library(tidyverse)
library(RcppRoll)

flux_weather <- read_csv("data/Flux/Daily_Ameriflux_ET_Weather.csv") %>% 
  transmute(date = date, site = site,
            ET = ET, tmean_f = tmean, vpdmax_f = vpdmax,
            ppt_f = ppt) # _f: from Flux tower 

flux_prism <- read_csv("data/AZ_Flux_PRISM.csv") %>% 
  transmute(site = str_sub(name, 4, 6),
         date = date(date),
         tmean_p = tmean, vpdmax_p = vpdmax, 
         ppt_p = ppt) #_p: from Prism
  
full_flux_w <- full_join(flux_weather, flux_prism, by = join_by(site, date)) %>% 
  group_by(site) %>% 
  arrange(date) %>% 
  mutate(ppt30_p = roll_sum(ppt_p, n = 30, fill = NA, align = "right"), 
         ppt30_f = roll_sum(ppt_f, n = 30, fill = NA, align = "right"),
         vpd30_p = roll_sum(vpdmax_p, n = 30, fill = NA, align = "right"),
         vpd30_f = roll_sum(vpdmax_f, n = 30, fill = NA, align = "right"))

san_pedro <- c("CMW", "Wkg", "Whs", "LS1", "LS2") # flux tower names in USP watershed

ggplot(filter(full_flux_w, site %in% san_pedro), aes(x = ppt30_p, y = ppt30_f))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(a = 1, b = 0, color = "red")+
  theme_light()+
  facet_wrap(~site)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "PRISM daily max VPD (kPa)", y = "Tower daily max VPD (kPa)")

ggplot(filter(full_flux_w, site %in% san_pedro), aes(x = tmean_p, y = tmean_f))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_abline(a = 1, b = 0, color = "red")+
  theme_light()+
  facet_wrap(~site)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "PRISM daily mean temp (C)", y = "Tower daily mean temp (C)")

full_flux_usp <- full_flux_w %>% 
  filter(site %in% san_pedro) %>% 
  mutate(site = case_when(site == "CMW" ~ "US-CMW",
                          site == "LS1" ~ "US-LS1",
                          site == "LS2" ~ "US-LS2",
                          site == "Wkg" ~ "US-Wkg",
                          site == "Whs" ~ "US-Whs"))
  
ggplot(filter(full_flux_usp), aes(x = vpd30_p, y = vpd30_f))+
  geom_point(aes(color = site), pch = 1)+
  geom_smooth(method = "lm", linetype = 2)+
  geom_abline(a = 1, b = 0, color = "red")+
  theme_light(base_size = 20)+
  # theme(strip.background = element_rect(color = "black", fill = "white"))+
  # theme(strip.text = element_text(colour = 'black'))+
  labs(x = "PRISM 30-day VPD (kPa)", y = "Flux tower 30-day VPD (kPa)",
       color = "Site")
cor.test(full_flux_usp$vpd30_p, full_flux_usp$vpd30_f)

summary(lm(vpdmax_f ~ vpdmax_p, full_flux_w)) # R2 = 0.8117
# can I compare to the R2 of a model that says they're equal?
summary(lm(ppt_f ~ ppt_p, full_flux_w)) # R2 = 0.1033
summary(lm(tmean_f ~ tmean_p, full_flux_w)) # R2 = 0.9047
