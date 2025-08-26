library(tidyverse)
library(lme4)
library(MuMIn)

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
       aes(x = date, y = et_mm_monthly, color = name))+
  geom_line()




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

# rank these wells by distance to channel????

hist(coef(mixed_rip)$name$dtg, breaks = 30)
mean(coef(mixed_rip)$name$dtg)
median(coef(mixed_rip)$name$dtg)
 # the mean response to groundwater: for every 1-foot increase in DTG (1 foot drop in water table), 
# ET drops 4.26 mm/mo (median = -3.61 mm/mo)
betas <- data.frame(rownames(coef(mixed_rip)$name), coef(mixed_rip)$name$dtg)
colnames(betas) <- c("name", "dtg")


betas <- full_join(betas, dist)
hist(log(betas$dist))

ggplot(betas, aes(x = log(dist), y = (dtg)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_smooth(method = "lm")+
  labs(x = "ln(Distance from stream to well (m))", y = "Monthly ET vs. DTG slope (mm/ft)")

ggplot(betas, aes(x = dist, y = (dtg)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_smooth(method = "lm")+
  labs(x = "Distance from stream to well (m)", y = "Monthly ET vs. DTG slope (mm/ft)")

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
  geom_point(aes(color = name))+
  geom_smooth(method = "lm", se = T)+
  theme_light()+
  theme(legend.position = "none")+
  facet_wrap(~season)

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