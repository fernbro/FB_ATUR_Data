library(tidyverse)
library(lme4)
library(MuMIn)
library(emmeans)
library(sf)
# install.packages("mapview")
library(mapview)
library(viridis)

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date), year = year(date), month = month(date)) %>% 
  filter(well == "alluvial") %>% 
  select(date, ppt, vpdmax, name, year, month) %>% 
  group_by(name, year, month) %>% 
  summarise(ppt = sum(ppt), vpdmax = max(vpdmax))

dist <- read_csv("data/Alluvial_Wells_Stream_Distance.csv") %>% 
  transmute(name = name, dist = NEAR_DIST)

rip_et_raw <- read_csv("data/OpenET_GEE_RiparianWells.csv")
# et data is in mm/month 

rip_et <- rip_et_raw %>% 
  mutate(yr = as.numeric(str_sub(`system:index`, 5, 8)),
         mo = as.numeric(str_sub(`system:index`, 9, 10)),
         dy = as.numeric(str_sub(`system:index`, 11, 12))) %>% 
  mutate(date = make_date(month = mo, year = yr, day = dy)) %>% 
  transmute(name = name, et_mm_monthly = et_ensemble_mad, 
            well = "Riparian", date = date,
            year = year(date), month = month(date)) %>% 
  mutate(season = case_when(month %in% c(1,2,3) ~ "JFM",
                            month %in% c(4, 5, 6) ~ "AMJ",
                            month %in% c(7, 8, 9) ~ "JAS",
                            month %in% c(10, 11, 12) ~ "OND"))

# example sites:
ggplot(filter(rip_et, name %in% c("D-20-21 15DBD1 [BOQ-LI]", "D-20-21 10AAC2 [FBK-UP]")),
       aes(x = date, y = et_mm_monthly))+
  geom_line()+
  facet_wrap(~name)

# integrate annualy for example sites:

ann_et <- filter(rip_et, name %in% c("D-20-21 15DBD1 [BOQ-LI]", 
                                     "D-20-21 10AAC2 [FBK-UP]")) %>% 
  group_by(name, year) %>% 
  summarise(et_mm_yrly = sum(et_mm_monthly))

ggplot(ann_et,
       aes(x = year, y = et_mm_yrly))+
  geom_bar(stat = "identity", fill = "coral")+
  labs(x = "Year", y = "ET (mm/year)")+
  theme_light(base_size = 26)+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("2000", "2005", "2010", "2015", "2020"))+
  facet_wrap(~name)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


# we can also try normalizing ET... this prob makes the most sense too

et_stats <- rip_et %>% 
  group_by(name, season) %>% 
  summarise(et_mean = mean(et_mm_monthly),
            et_sd = sd(et_mm_monthly))


ripar_et <- full_join(rip_et, et_stats) %>% 
  mutate(et_z = (et_mm_monthly - et_mean)/et_sd)

#how do i want to merge this.....
# how to treat these dates
# most simple: wrangle DTG data so that there is one value per site per month
# if there are more than one which is probably rare but not impossible- average them lol
# then just connect w/ ET data


alluv_levels <- read_csv("data/Near-stream_alluvial-aquifer_water_levels.csv",
                         skip = 2, col_names = T)
riparian_dtg <- alluv_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, 
                              tryFormats = c("%m/%d/%Y")),
            name = station_nm, 
            level = lev_va/3.281,
            month = month(date), year = year(date)) %>% 
  mutate(season = case_when(month %in% c(1,2,3) ~ "JFM",
                            month %in% c(4, 5, 6) ~ "AMJ",
                            month %in% c(7, 8, 9) ~ "JAS",
                            month %in% c(10, 11, 12) ~ "OND"))

rip_stats <- riparian_dtg %>% 
  group_by(name, season) %>% 
  summarise(dtg_mean = mean(level),
            dtg_sd = sd(level))

rip_dtg1 <- full_join(riparian_dtg, rip_stats) %>% 
  mutate(dtg_z = (level - dtg_mean)/dtg_sd)

rip_dtg <- rip_dtg1 %>%
  group_by(name, year, month) %>%
  summarise(dtg_z = mean(dtg_z),
            dtg = mean(level))

dtg_et <- full_join(rip_dtg, ripar_et) %>% 
  # filter(month %in% c(7, 8, 9)) %>% 
  filter(!is.na(dtg)) %>% 
  mutate(anom = case_when(dtg_z < -1 ~ "Shallower",
                          dtg_z <= 1 & dtg_z >= -1 ~ "±1 SD",
                          dtg_z > 1 ~ "Deeper")) %>% 
  mutate(season = case_when(month %in% c(1,2,3) ~ "JFM",
                            month %in% c(4, 5, 6) ~ "AMJ",
                            month %in% c(7, 8, 9) ~ "JAS",
                            month %in% c(10, 11, 12) ~ "OND"))

# max_ann <- ripar_et %>% 
#   group_by(name, year) %>% 
#   summarise(max_et = max(et_mm_monthly))
#   

monsoon <- filter(dtg_et, season == "JAS")
cor.test(monsoon$et_mm_monthly, monsoon$dtg, method = "spearman", exact = F) # compute rank correlation

# models for analysis:
mixed_rip <- lmer(et_mm_monthly ~ dtg | name, data = monsoon)
# confint(mixed_rip)
r.squaredGLMM(mixed_rip) # 78.2% variance explained

ggplot(filter(monsoon, !is.na(dtg), !is.na(et_mm_monthly)), aes(x = dtg, y = et_mm_monthly))+
  geom_point(alpha = 0.5)+
  geom_line(aes(y = predict(mixed_rip), group = name))
  # facet_wrap(~name, scales = "free")


# marginal plots? emmeans

# plot(emtrends(mixed_rip), var = "dtg")



# graph corresponding to model structure above:
ggplot(filter(monsoon, name %in% c("D-20-21 15DBD1 [BOQ-LI]", "D-20-21 10AAC2 [FBK-UP]")),
       aes(x = dtg, y = et_mm_monthly, color = name))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme(legend.position = "none")+
  facet_wrap(~name, scales = "free")

# rank these wells by distance to channel????

hist(coef(mixed_rip)$name$dtg, breaks = 30)

ggplot(coef(mixed_rip)$name, aes(x = dtg))+
  geom_vline(xintercept = 0, color = "orange")+
  geom_histogram(binwidth = 0.5, aes(group = row.names(coef(mixed_rip)$name)), 
                 color = "white")+
  theme_light(base_size = 26)+
  labs(x = "Site ET response to DTG (mm/m)", y = "# of sites")

ggplot(coef(mixed_rip)$name, aes(x = dtg, y = "Riparian"))+
  geom_vline(xintercept = 0, color = "orange")+
  geom_point(position = "jitter", pch = 1, size = 3)+
  geom_boxplot(alpha = 0.5)+
  theme_light(base_size = 26)+
  labs(x = "Site ET response to DTG (mm/m)", y = "Well location")


mean(coef(mixed_rip)$name$dtg)
median(coef(mixed_rip)$name$dtg)
 # the mean response to groundwater: for every 1-foot increase in DTG (1 foot drop in water table), 
# ET drops 4.26 mm/mo (median = -3.61 mm/mo)
betas <- data.frame(rownames(coef(mixed_rip)$name), coef(mixed_rip)$name$dtg)
colnames(betas) <- c("name", "dtg")


betas <- full_join(betas, dist) %>% 
  full_join(filter(rip_stats, season == "JAS"))
hist(log(betas$dist))

# ggplot(filter(betas, season == "JAS"), aes(x = dtg_mean, y = (dtg)))+
#   geom_point()+
#   geom_hline(yintercept = 0)+
#   geom_smooth(method = "lm")+
#   labs(x = "Site mean DTG (m)", y = "Monthly ET vs. DTG slope (mm/m)")

ggplot(betas, aes(x = dist, y = (dtg)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_smooth(method = "lm")+
  labs(x = "Distance from stream to well (m)", y = "Monthly ET vs. DTG slope (mm/m)")

distmod <- lm(dtg ~ log(dist), betas)
# plot(distmod) 
summary(distmod) # r2 = 0.2208; increasing the log(dist) by 1, decrease ET sensitivity to groundwater by -2.55?

fixef(mixed_rip)
ranef(mixed_rip)


hist(filter(dtg_et, season == "JAS")$et_mm_monthly)


#########




dtg_et$anom <- factor(dtg_et$anom, c("Shallower", "±1 SD", "Deeper"))

dtg_et %>% 
  filter(month %in% c(7, 8, 9)) %>% 
  filter(name %in% c("D-20-21 15DBD1 [BOQ-LI]", "D-20-21 10AAC2 [FBK-UP]")) %>% 
ggplot(aes(x = dtg_z, y = et_z))+
  geom_point(aes(color = name))+
  geom_smooth(method = "lm", se = T, aes(fill = name))+
  theme_light()+
  # theme(legend.position = "none")+
  facet_wrap(~season)
# for july, august, and september, an increase in DTG from 0 to 10 (non-normalized)
# led to a decrease in monthly ET of about 50 mm!

dtg_et %>% 
  filter(month %in% c(7, 8, 9)) %>% 
  ggplot(aes(x = dtg, y = (et_mm_monthly)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_light(base_size = 26)+
  theme(legend.position = "none")+
  labs(x = "Depth to groundwater (m)", y = "ET (mm/month)")

summary(lm(et_mm_monthly ~ dtg, data = filter(dtg_et, month %in% c(7, 8, 9))))

filter(monsoon, !is.na(dtg), !is.na(et_mm_monthly)) %>% 
  ggplot(aes(x = dtg, y = et_mm_monthly))+
  geom_point()+
  geom_line(aes(y = predict(mixed_rip), group = name))+
  geom_smooth(method = "lm")+
  theme_light(base_size = 26)+
  theme(legend.position = "none")+
  labs(x = "Depth to groundwater (m)", y = "ET (mm/month)")
summary(lm(et_mm_monthly ~ dtg, data = monsoon))

dtg_et %>% 
  filter(month %in% c(7, 8, 9)) %>% 
  ggplot(aes(x = dtg_z, y = (et_z)))+
  geom_point(aes(color = name))+
  geom_smooth(method = "lm", se = T)+
  theme_light()+
  theme(legend.position = "none")+
  facet_wrap(~season)

dtg_et %>% 
  ggplot(aes(x = dtg_z, y = (et_z)))+
  geom_point(aes(color = name))+
  geom_smooth(method = "lm", se = T)+
  theme_light()+
  theme(legend.position = "none")+
  facet_wrap(~season)


ggplot(dtg_et, aes(x = anom, y = et_z))+
  geom_boxplot()+
  theme_light()+
  theme(legend.position = "none")

# ggplot(rip_et, aes(x = date, y = et_ensemble_mad, group = name))+
  # geom_line()




# Weather-ET combo:
rip_et_w <- inner_join(rip_et, weather)

ppt_mod <- lmer(et_mm_monthly ~ ppt + season + (1 | name), rip_et_w)
vpd_mod <- lmer(et_mm_monthly ~ vpdmax + season + (1 | name), rip_et_w)
weather_mod <- lmer(et_mm_monthly ~ ppt + vpdmax + season + (1 | name), rip_et_w)

r.squaredGLMM(ppt_mod) # 71.7
r.squaredGLMM(vpd_mod) # 75.2
summary(weather_mod)  ; r.squaredGLMM(weather_mod) # 75.5
anova(weather_mod)  
  
# isolate for monsoon szns:

mons_et_w <- filter(rip_et_w, month %in% c(7, 8, 9))

cor.test(mons_et_w$et_mm_monthly, mons_et_w$vpdmax)

cor.test(mons_et_w$et_mm_monthly, mons_et_w$ppt)

ggplot(mons_et_w, aes(x = vpdmax, y = et_mm_monthly))+
  geom_point()+
  geom_smooth(method = "lm", aes(group = name), se = F)

# growing season ET correlations:

gs <- rip_et_w %>% 
  filter(month >= 3 & month <= 10) %>% 
  group_by(name, year) %>% 
  summarise(gs_et = sum(et_mm_monthly), 
            gs_ppt = sum(ppt),
            gs_vpdmax = max(vpdmax))

cor.test(gs$gs_et, gs$gs_ppt)
cor.test(gs$gs_et, gs$gs_vpdmax)

ggplot(gs, aes(x = gs_ppt, y = gs_et))+
  geom_point()+
  geom_smooth()
ggplot(gs, aes(x = gs_vpdmax, y = gs_et))+
  geom_point()+
  geom_smooth(method = "lm")


# map the slopes spatially?
well_loc <- st_read("data/Alluvial_well_locations.shp")

wells <- full_join(well_loc, betas) %>% 
  st_as_sf()

wells_disp <- st_jitter(wells, amount = 0.04)

mapview(wells_disp[2], col.regions = inferno)

# infer <- palette(hcl.colors(8, "inferno"))
# 
# plot(wells_disp[2], col = infer)
# 
# 
# ggplot(wells_disp, aes())+
#   geom_sf()
# 


  
  