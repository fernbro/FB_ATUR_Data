library(tidyverse)
library(lme4)

# read in files: DAILY LEVEL
flux <- read_csv('data/Processed/Daily_Ameriflux_ET.csv')
evi_raw <- read_csv('data/EVI_Daily_Flux.csv')
prism_raw <- read_csv('data/AZ_Flux_PRISM.csv')

evi <- evi_raw %>% 
  transmute(date = make_date(year = year, month = month, day = day),
            site = substr(name, 4, 6), evi = EVI)

prism <- prism_raw %>% 
  mutate(site = substr(name, 4, 6),
         date = date(date)) %>% 
  select(-name)

evi_flux <- full_join(flux, evi) %>% 
  full_join(prism) 
# %>% 
#   filter(ET >= 0, !is.na(ET), !is.na(evi), !is.na(vpdmax)) # filter for NA values and <0 ET

ggplot(filter(evi_flux), aes(x = evi, y = ET))+
  geom_point(size = 0.2)+
  geom_smooth(se = F, method = "lm")+
  facet_wrap(~site)

model <- lmer(ET ~ vpdmax + evi + tmean + ppt + (1 + evi | site), evi_flux)
 # ppt on any given day may not tell much.
  # what about ppt within the last 3 days? or a time period?

# plot(fitted(model), evi_flux$ET)
abline(a = 0, b = 1, col = "red")

# Aggregate all variables to Monthly

monthly <- evi_flux %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ET = mean(ET, na.rm = T), evi = mean(evi, na.rm = T), 
            ppt = sum(ppt), tmean = mean(tmean, na.rm = T),
            vpdmax = max(vpdmax))

ggplot(filter(monthly, site %in% c('CMW', 'LS1', 'LS2', 'Wkg', 'Whs')), 
       aes(x = evi, y = ET))+
  geom_point(size = 0.4, aes(color = site))+
  geom_smooth(se = F, method = "lm", linewidth = 0.5)+
  ggtitle("Monthly ET vs. average EVI")+
  xlab("EVI")+
  ylab("ET (mm/day)")+
  theme_light()+
  #facet_wrap(~site)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("figures/Monthly_ETvEVI_USPFlux.jpg", last_plot(), width = 5, height = 5, units = "in")

summary(lm(ET ~ evi, data = filter(monthly, site == "Whs")))

annual <- evi_flux %>% 
  mutate(year = year(date)) %>% 
  group_by(site, year) %>% 
  summarise(ET = mean(ET, na.rm = T), evi = mean(evi, na.rm = T), 
            ppt = sum(ppt), tmean = mean(tmean, na.rm = T),
            vpdmax = max(vpdmax))

ggplot(filter(annual), aes(x = evi, y = ET))+
  geom_point(size = 0.2)+
  geom_smooth(se = F, method = "lm")+
  xlab("EVI")+
  ylab("Monthly ET (mm/day)")
  facet_wrap(~site)
