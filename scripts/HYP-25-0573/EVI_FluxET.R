library(tidyverse)

# flux <- read_csv('data/Processed/Daily_Ameriflux_ET.csv')
flux <- read_csv('data/Flux/Daily_Ameriflux_ET_Weather_04232025.csv')

prism_raw <- read_csv('data/AZ_Flux_PRISM.csv')
prism <- prism_raw %>% 
  mutate(site = substr(name, 4, 6),
         date = date(date)) %>% 
  select(-name)

evi_raw <- read_csv('data/MOD09GA_EVI_Flux.csv')
evi <- evi_raw %>% 
  rename(date_string = `system:index`) %>% 
  transmute(date = make_date(year = substr(date_string, 1, 4), 
                             month = substr(date_string, 6, 7), 
                             day = substr(date_string, 9, 10)),
            site = substr(name, 4, 6), evi = evi)

evi_flux <- full_join(flux, evi) %>% 
  full_join(prism)

monthly <- evi_flux %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ET = mean(ET, na.rm = T), evi = mean(evi, na.rm = T), 
            ppt = sum(ppt), tmean = mean(tmean, na.rm = T),
            vpdmax = max(vpdmax)) %>% 
  mutate(hydro = case_when(site %in% c("CMW", "LS1", "LS2") ~ "Riparian",
                           .default = "Upland"))

san_pedro <- c('CMW', 'LS1', 'LS2', 'Wkg', 'Whs')

#site != "xSR", site != "Fmf", site != "Fwf", site != "Fuf", site != "MtB"

ggplot(filter(monthly, site %in% san_pedro), 
       aes(x = evi, y = ET))+
  geom_point(size = 4, alpha = 0.8, aes(color = site))+
  geom_smooth(method = "lm", linewidth = 1, se = F, aes(linetype = site), color = "grey40")+
  geom_smooth(method = "lm", linewidth = 2, se = F, color = "black")+
  xlab("Monthly average EVI")+
  ylab("Monthly ET (mm/day)")+
  labs(color = "AmeriFlux site", linetype = "AmeriFlux site")+
  theme_light(base_size = 30)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
# theme(legend.position = 'none')
summary(lm(ET ~ evi, filter(monthly, site %in% san_pedro))) # R2 71.3%
