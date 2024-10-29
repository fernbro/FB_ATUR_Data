library(tidyverse)
library(lme4)
# install.packages("RcppRoll")
library(RcppRoll)
# install.packages("MuMIn")
library(MuMIn)
# install.packages("ggcorrplot")
library(ggcorrplot)
# install.packages('ggforce')
library(ggforce)

ppt1 <- read_csv("data/USP_AlluvialWells_PRISM_ppt.csv")
tmean1 <- read_csv("data/USP_AlluvialWells_PRISM_tmean.csv")
vpdmax1 <- read_csv("data/USP_AlluvialWells_PRISM_vpdmax.csv")

weather1 <- full_join(ppt1, tmean1, join_by(name, date)) %>% 
  full_join(vpdmax1, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, well = "alluvial",
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T)))

ppt2 <- read_csv("data/USP_RegionalWells_PRISM_ppt.csv")
tmean2 <- read_csv("data/USP_RegionalWells_PRISM_tmean.csv")
vpdmax2 <- read_csv("data/USP_RegionalWells_PRISM_vpdmax.csv")

weather2 <- full_join(ppt2, tmean2, join_by(name, date)) %>% 
  full_join(vpdmax2, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, well = "regional",
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T)))

  
weather <- rbind(weather1, weather2) %>% 
  mutate(year = year(date)) %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(cum_vpd_10d = RcppRoll::roll_sum(vpdmax, n = 10, 
                                          fill = NA, align = "right"),
         tmean_10d = RcppRoll::roll_mean(tmean, n = 10, 
                                         fill = NA, align = "right"),
         cum_ppt_10d = RcppRoll::roll_sum(ppt, n = 10, fill = NA, 
                                          align = "right"),
         max_vpd_10d = RcppRoll::roll_max(vpdmax, n = 10, fill = NA, 
                                          align = "right"),
         cum_vpd_30d = RcppRoll::roll_sum(vpdmax, n = 30, 
                                          fill = NA, align = "right"),
         tmean_30d = RcppRoll::roll_mean(tmean, n = 30, 
                                         fill = NA, align = "right"),
         cum_ppt_30d = RcppRoll::roll_sum(ppt, n = 30, fill = NA, 
                                          align = "right"),
         max_vpd_30d = RcppRoll::roll_max(vpdmax, n = 30, fill = NA, 
                                          align = "right"),
         max_tmean_30d = RcppRoll::roll_max(tmean, n = 30, fill = NA, 
                                          align = "right"),
         max_tmean_10d= RcppRoll::roll_max(tmean, n = 10, fill = NA, 
                                          align = "right")) %>% 
  ungroup()

colors = c("red", "orange", "yellow", "green", "blue", "purple")
ggplot(filter(weather, month(date) %in% c(4, 5, 6, 7, 8, 9)) %>%   
         mutate(well = case_when(well == "alluvial" ~ "Riparian",
                                 well == "regional" ~ "Upland")), 
       aes(y = vpdmax, x = as.factor(month(date)),
           fill = as.factor(month(date))))+
  geom_boxplot()+
  theme_light(base_size = 26)+
  scale_fill_manual(values = colors,
                    labels = c("April", "May", "June",
                               "July", "August", "September"))+
  labs(fill = "Month")+
  xlab("Month")+
  ylab("Daily max VPD")+
  theme(legend.position = "none")+
  facet_wrap(~well)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

# dplyr cumsum()

# UNITS:
# ppt: mm per day
# tmean: celsius
# vpdmax: hPa (10^2 pascals); converted to kPa

# create some weather variables that accumulate more time than just Day Of vals:

# rolling 10 day sum of precipitation?
# use site as random effect for a differential response to VPD

weather_ann_daily <- weather %>% 
  mutate(doy = yday(date)) %>% 
  group_by(well, doy) %>% 
  summarise(mean_vpdmax = mean(vpdmax, na.rm = T), sd_vpdmax = sd(vpdmax, na.rm = T),
         mean_ppt = mean(ppt, na.rm = T), sd_ppt = sd(ppt, na.rm = T),
         mean_tmean = mean(tmean, na.rm = T), sd_tmean = sd(tmean, na.rm = T)) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

ggplot(weather_ann_daily, aes(x = doy, y = mean_ppt, 
                              group = well, color = well))+
  geom_line()+
  theme_light()+
  labs(x = "DOY", y = "Mean daily precipitation (mm)")
  # geom_ribbon(aes(ymin = mean_vpdmax - (sd_vpdmax), 
  #                 ymax = mean_vpdmax + (sd_vpdmax)),
  #             alpha = 0.7)

# ggplot()+
#   geom_line(data = weather_ann_daily, linetype = 2,
#             aes(x = doy, y = mean_vpdmax, color = well))+
#   geom_line(data = evi_ann_daily, linetype = 1,
#             aes(x = doy, y = mean_evi*10, color = well))+
#   theme_light(base_size = 20)+
#   labs(x = "DOY", y = "Mean daily maximum VPD (kPa) / Mean daily EVI x 10")

monthly_weather <- weather %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(well, name, year, month) %>% 
  summarise(ppt = sum(ppt), tmean = mean(tmean, na.rm = T),
            vpdmax = mean(vpdmax, na.rm = T))

# we want to understand antecedent moisture conditions.
# prev. water year's precipitation; calculate precipitation 
# precipitation up to date in z score dataframe

'%!in%' <- function(x,y)!('%in%'(x,y))

# weather year precipitation PRIOR to monsoon:
weather_wy <- weather %>%
  mutate(water_year = case_when(month(date) >= 11 ~ year(date) + 1,
                                .default = year(date))) %>% 
  filter(water_year != 2000, water_year != 2024) %>% # we don't have data for 1999, and WY 2024 is not over
  group_by(name, well, water_year) %>% 
  filter(month(date) %!in% c(7, 8, 9)) %>% # exclude july, august, and september: summing total rain from oct-june
  summarise(wy_ppt = sum(ppt)) %>% 
  rename(year = water_year)

wy_avgs <- weather_wy %>%
  group_by(year) %>% 
  summarise(wy_ppt = mean(wy_ppt))

ggplot(wy_avgs, aes(x = year, y = wy_ppt))+
  geom_point()+
  geom_line()+
  theme_light(base_size = 26)+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Pre-monsoon precip (Nov - June) (mm)")+
  geom_hline(yintercept = mean(weather_wy$wy_ppt), linetype = 2)


wy_stats <- weather_wy %>% 
  group_by(name) %>% 
  summarise(wy_avg = mean(wy_ppt, na.rm = T),
            wy_sd = sd(wy_ppt, na.rm = T))

wy_z <- weather_wy %>% 
  full_join(wy_stats) %>% 
  mutate(wy_z = (wy_ppt - wy_avg)/wy_sd)

# Calculate cumulative sums for Water Year. Using October-September as per AZ state definition...
weather_annual_cumul <- rbind(weather1, weather2) %>% 
  mutate(water_year = case_when(month(date) >= 10 ~ year(date) + 1,
                                .default = year(date))) %>% 
  filter(water_year > 2000) %>% 
  arrange(date) %>%
  group_by(name, water_year) %>%
  mutate(cum_maxvpd = cumsum(vpdmax), cum_ppt = cumsum(ppt))  %>% 
  ungroup() %>%
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(cum_vpd_10d = RcppRoll::roll_sum(vpdmax, n = 10, 
                                          fill = NA, align = "right"),
         tmean_10d = RcppRoll::roll_mean(tmean, n = 10, 
                                         fill = NA, align = "right"),
         cum_ppt_10d = RcppRoll::roll_sum(ppt, n = 10, fill = NA, 
                                          align = "right"),
         max_vpd_10d = RcppRoll::roll_max(vpdmax, n = 10, fill = NA, 
                                          align = "right"),
         cum_vpd_30d = RcppRoll::roll_sum(vpdmax, n = 30, 
                                          fill = NA, align = "right"),
         tmean_30d = RcppRoll::roll_mean(tmean, n = 30, 
                                         fill = NA, align = "right"),
         cum_ppt_30d = RcppRoll::roll_sum(ppt, n = 30, fill = NA, 
                                          align = "right"),
         max_vpd_30d = RcppRoll::roll_max(vpdmax, n = 30, fill = NA, 
                                          align = "right"),
         max_tmean_30d = RcppRoll::roll_max(tmean, n = 30, fill = NA, 
                                            align = "right"),
         max_tmean_10d= RcppRoll::roll_max(tmean, n = 10, fill = NA, 
                                           align = "right")) %>% 
  ungroup()


ggplot(filter(weather, year(date) == 2015), 
       aes(x = date,
           y = ppt))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = vpdmax))+
  theme_light()


ggplot(filter(weather), aes(x = tmean))+
  geom_histogram(binwidth = 0.5)+
  theme_light()

#~~~~~~~~~

# Read in Z-score data:

z_scores <- read_csv("data/Processed/USP_GW_EVI_Z_10172024.csv") %>%
  mutate(date = as.POSIXct(gsub(date, pattern=" 07:00:00", 
                                replacement="", fixed=T))) %>% 
  select(date, name, well, level, evi, evi_mean, evi_sd, dtg_mean,
         dtg_sd, evi_sd, dtg_sd, evi_z, dtg_z, month, reach, year)

alluvial_landfire <-read_csv("data/Landcover/USP_AlluvialWells_LF2016.csv") %>% 
  select(-year)

weather_z <- full_join(z_scores, weather_annual_cumul) %>%  # alluvial areas
  filter(well == "alluvial") %>% 
  inner_join(wy_z) %>% 
  mutate(winter = case_when(wy_z < -0.5 ~ "dry",
                                wy_z > 0.5 ~ "wet",
                            .default = "avg"),
         sdr_30d = ((cum_ppt_30d)/(max_vpd_30d)),
         sdr_wy = cum_ppt/cum_maxvpd) %>%  # supply demand ratio
  full_join(alluvial_landfire, relationship = "many-to-many") %>% 
  filter(!is.na(evi_z), !is.na(sdr_30d))
# log_sdr <- weather_z %>% 
#   filter(sdr > 0) %>% 
#   mutate(log_sdr = log(sdr))
              
              model_456 <- weather_z %>% 
                filter(month(date) %in% c(4, 5, 6)) %>% 
                filter(!is.na(evi_z))
              
              model_789 <- weather_z %>% 
                filter(month(date) %in% c(7, 8, 9)) %>% 
                filter(!is.na(evi_z)) %>% 
                mutate(doy = yday(date))
  
upland_z <- full_join(z_scores, weather_annual_cumul) %>%  # upland areas
  filter(well == "regional") %>% 
  inner_join(wy_z) %>% 
  mutate(winter = case_when(wy_z < -0.5 ~ "dry",
                                wy_z > 0.5 ~ "wet",
                            .default = "avg"),
         sdr_30d = ((cum_ppt_30d)/(max_vpd_30d)),
         sdr_wy = cum_ppt/cum_maxvpd) %>%  # supply demand ratio
  full_join(alluvial_landfire, relationship = "many-to-many") %>% 
  filter(!is.na(evi_z), !is.na(sdr_30d))
  
ggplot(upland_z, aes(x = vpdmax, y = evi_z))+
  geom_point(aes(color = month))+
  theme_light(base_size = 20)+
  labs(x = "Maximum daily VPD (kPa)", y = "Daily EVI Z-score")

total_z <- rbind(upland_z, weather_z) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

ggplot(filter(total_z), aes(x = sdr_30d, y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT:VPD (mm/kPa)", y = "EVI")+
  facet_wrap(~well)+
  geom_smooth(method = "lm", color = "gray80")+
  geom_point(aes(color = month))+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

# compare regressions
upland_model <- lm(evi ~ sdr_30d, upland_z)
riparian_model <- lm(evi ~ sdr_30d, weather_z)
summary(upland_model) # R2 0.5779, slope 0.0046996
summary(riparian_model) # R2 0.4368, slope 0.0072236 # less dependent on ppt but more responsive to declines in vpd

upland_cor <- select(upland_z, evi, sdr_30d)
cor.test(upland_z$evi, upland_z$sdr_30d) #0.76
cor.test(weather_z$evi, weather_z$sdr_30d) # 0.66
cor.test(upland_z$evi, upland_z$cum_vpd_30d) # -0.20
cor.test(weather_z$evi, weather_z$cum_vpd_30d) # 0.11
cor.test(upland_z$evi, upland_z$cum_ppt_30d) # 0.75
cor.test(weather_z$evi, weather_z$cum_ppt_30d) # 0.67

upland_0 <- filter(upland_z, cum_ppt_30d == 0)
riparian_0 <- filter(weather_z, cum_ppt_30d == 0)

cor.test(upland_0$evi, upland_0$cum_vpd_30d) # 0.113
cor.test(riparian_0$evi, riparian_0$cum_vpd_30d) # 0.518
cor.test(upland_0$evi, upland_0$vpdmax) # 0.14
cor.test(riparian_0$evi, riparian_0$vpdmax) # 0.48
cor.test(riparian_0$evi, riparian_0$level) # 0.04
cor.test(upland_0$evi, upland_0$level) # -0.18

plot(upland_model, 1)
plot(riparian_model, 1)

.ggplot(total_ppt0, aes(x = cum_vpd_30d, y = evi))+
  theme_light(base_size = 20)+
  labs(x = "Cumulative 30-day maximum VPD (kPa)", y = "EVI")+
  geom_smooth(aes(fill = well), method = "lm")+
  geom_point(aes(color = month, shape = well))
summary(lm(evi_z ~ cum_vpd_30d, upland_ppt0))


ggplot(weather_z, aes(x = evi_z))+
  geom_density()
ggplot(filter(filter(weather_z, well == "alluvial"), !is.na(month), reach != 0), 
       aes(x = level, y = evi_z))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_light()+
  facet_wrap(~year)

# get a regression slope for each year and plot vs. year?

regr_slopes_year <- weather_z %>% 
  filter(well == "alluvial") %>% 
  group_by(year) %>%
  reframe(tidy(lm(evi_z ~ dtg_z))) %>%
  ungroup() %>%
  mutate(term = case_when(term == "(Intercept)" ~ "intercept",
                          term == "dtg_z" ~ "slope")) %>% 
  filter(term == "slope") %>% 
  filter(p.value <= 0.05)

ggplot(regr_slopes_year, aes(x = year, y = estimate))+
  geom_point()+
  geom_smooth(method = "lm")+ #r2 = 0.35
  theme_light()+
  labs(x = "Year", y = "EVI Z - DTG Z slope across SPRNCA")
# summary(lm(estimate ~ year, regr_slopes_year))

regr_slopes_name <- weather_z %>% 
  filter(well == "alluvial") %>% 
  group_by(name) %>%
  reframe(tidy(lm(evi_z ~ dtg_z))) %>%
  ungroup() %>%
  mutate(term = case_when(term == "(Intercept)" ~ "intercept",
                          term == "dtg_z" ~ "slope")) %>% 
  filter(term == "slope") %>% 
  filter(p.value <= 0.05) %>% 
  full_join(weather_z)

ggplot(regr_slopes_name, aes(x = dtg_mean, y = estimate))+
  geom_point()+
  geom_smooth(method = "lm")+ #r2 = 0.26
  theme_light()+
  labs(x = "Mean depth to groundwater (ft)", y = "EVI Z - DTG Z slope")
summary(lm(estimate ~ dtg_mean, regr_slopes_name))

# cool!

# what does it look like with NO recent 30d precip??? what are the controling factors in this situation?
# (arbitrary day length...)

weather_ppt0 <- filter(weather_z, cum_ppt_30d == 0)
upland_ppt0 <- filter(upland_z, cum_ppt_30d == 0)
total_ppt0 <- filter(total_z, cum_ppt_30d == 0)

ggplot(filter(weather_ppt0, !is.na(month)), 
       aes(x = cum_vpd_30d, y = dtg_z))+
  geom_point(aes(color = level))+
  geom_smooth(method = "lm")+
  theme_light()+
  ggtitle("no 30-day precip")
summary(lm(evi_z ~ cum_vpd_30d, filter(weather_ppt0, !is.na(month))))
# dtg_z is more correlated with vpd than evi is

ggplot(filter(total_ppt0, !is.na(month)), 
       aes(x = cum_vpd_30d, y = evi))+
  geom_point(aes(color = level))+
  geom_smooth(method = "lm")+
  theme_light()+
  ggtitle("no 30-day precip")+
  facet_wrap(~well)
summary(lm(evi_z ~ cum_vpd_30d, filter(weather_ppt0, !is.na(month))))

hist(weather_z$sdr_30d, breaks = seq(0,40, by = 0.1)) # longest tail ever lol
hist(subset(weather_z, sdr_30d == 0)$evi_z, breaks = seq(-2, 3, 0.1)) # 0 observed precipitation in the past 30 days 

ggplot(weather_z, aes(x = sdr_30d, y = evi))+
  geom_point(aes(color = as.factor(month)))+
  geom_smooth(method = "lm")+
  theme_light(base_size = 26)
# test how well the different month responses relate to each other within a site

summary(lm(evi ~ sdr_30d, weather_z)) # r2 0.4368

summary(modelx <- lm(evi_z ~ sdr_30d + dtg_z, weather_z)) #adj r2 0.4719
# positive dtg_z term
summary(modela <- lm(evi_z ~ sdr_30d*dtg_z, weather_z)) #adj r2 0.48

cor(weather_z$evi_z, weather_z$dtg_z)

summary(modely <- lm(evi_z ~ sdr_30d, weather_z)) #adj r2 0.4617

hist(fitted(modely), breaks = seq(-2, 5, 0.01))
hist(fitted(modelx), breaks = seq(-2, 5, 0.01)) # dist is quite clustered around 0 compared to observed data
hist(weather_z$evi_z, breaks = seq(-2, 5, 0.01))

plot(fitted(modelx), weather_z$evi_z)
abline(a = 0, b = 1, col = "red")

plot(fitted(modely), weather_z$evi_z)
abline(a = 0, b = 1, col = "red")

plot(fitted(modela), weather_z$evi_z)
abline(a = 0, b = 1, col = "red")

summary(modelz <- lmer(evi_z ~ sdr + (1 + dtg_z | name), weather_z))

plot(fitted(modelz), weather_z$evi_z)
abline(a = 0, b = 1, col = "red")
r.squaredGLMM(modelz) # this model has the best conditional R2 (random intercept and slope model)
# now compare if sites respond similarly to SDR but differently to DTG (random effect just for DTG) 
# this is definitely an improved model because it explains the response across many months 

r.squaredGLMM(modely)
r.squaredGLMM(modelx)

pearson_reaches <- weather_z %>% 
  filter(reach != 0, well == "alluvial") %>% 
  group_by(reach) %>% 
  summarise(cor_dtg = cor(evi_z, dtg_z, method = "pearson"),
            cor_sdr = cor(evi_z, sdr_30d, method = "pearson"))
summary(lm(cor_sdr ~ cor_dtg, data = pearson_reaches)) # R2 = 0.61 without Reach 0

pearson_names <- weather_z %>% 
  filter(reach != 0, well == "alluvial") %>% 
  group_by(reach, name) %>% 
  summarise(cor_dtg = cor(evi_z, dtg_z, method = "pearson"),
            cor_sdr = cor(evi_z, sdr_30d, method = "pearson"),
            n = n())
summary(lm(cor_dtg ~ cor_sdr, data = pearson_names)) # R2 = 0.29 without reach 0
hist(pearson_names$n)
# ~

ggplot(pearson_reaches, aes(x = cor_dtg, y = cor_sdr))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", alpha = 0.2, color = "gray50")+
  theme_light(base_size = 20)+
  labs(color = "Month", x = "Correlation of EVI Z and DTG Z",
       y = "Correlation of 30D cumulative PPT:VPD and EVI Z")
# 

ggplot(pearson_names, aes(y = cor_sdr, x = cor_dtg, label = as.factor(reach)))+
  geom_point(aes(color = as.factor(reach)), size = 2)+
  geom_smooth(method = "lm", alpha = 0.2, color = "gray50")+
  theme_light(base_size = 20)+
  labs(color = "SPRNCA Reach", y = "Correlation of 30D cumulative PPT:VPD and EVI Z",
       x = "Correlation of EVI Z and DTG Z")+
  geom_text()
   # error bars to correlations?
  #geom_mark_ellipse(aes(group = reach))

pearson_789 <- model_789 %>% 
  group_by(name) %>% 
  summarise(cor_dtg = cor(evi_z, dtg_z, method = "pearson"),
            cor_ppt = cor(evi_z, cum_ppt_30d, method = "pearson"),
            season = "JAS")

pearson_sites_dtg <- rbind(pearson_456, pearson_789) %>% 
  select(name, cor_dtg, season) %>% 
  pivot_wider(values_from = cor_dtg, names_from = season)

ggplot(pearson_sites_dtg, aes(x = JAS, y = AMJ))+
  geom_point()

pearson_sites_ppt <- rbind(pearson_456, pearson_789) %>% 
  select(name, cor_ppt, season) %>% 
  pivot_wider(values_from = cor_ppt, names_from = season)

ggplot(pearson_sites_ppt, aes(x = JAS, y = AMJ))+
  geom_point()


cor.test(model_789$evi_z, model_789$dtg_z, method = "p") # -0.5066061  (significant)
cor.test(model_456$evi_z, model_456$dtg_z, method = "p") # 0.25 (significant)

ggplot(pearson_456, aes(x = cor_dtg, y = cor_ppt))+
  geom_point()+
  geom_smooth(method = "lm")
summary(lm(cor_ppt ~ cor_dtg, pearson_456)) # R2 = 0.2964 (F test p = 0.000007)

ggplot(pearson_789, aes(x = cor_dtg, y = cor_ppt))+
  geom_point()+
  geom_smooth(method = "lm")
summary(lm(cor_ppt ~ cor_dtg, pearson_789)) # R2 = 0.09695 (F test p = 0.01054)

create_corr <- function(df){
  
  data <- df %>% 
    transmute(EVI = evi, 
              `EVI Z` = evi_z, 
              `DTG` = level, 
              `DTG Z` = dtg_z, 
              `Daily P` = ppt, 
              `Daily mean T` = tmean, 
              `Daily max VPD` = vpdmax, 
              #`10-day cumulative VPD` = cum_vpd_10d, 
              `30-D cumul VPD` = cum_vpd_30d, 
              #`10-day cumulative Precipitation` = cum_ppt_10d, 
              `30-D P` = cum_ppt_30d,
              #`10-day max VPD` = max_vpd_10d, 
              `30-D max VPD` = max_vpd_30d, 
              #`30-day mean Temp` = tmean_30d, 
              #`10-day max daily mean Temp` = max_tmean_10d,
              `30-D max mean T` = max_tmean_30d, 
              `Nov-June P` = wy_ppt
    )
  
  result <- cor(data, method = "pearson")
  
  return(result)
  
}

# at some point I arbitrarily decided to exclude october....... maybe this needs some more thinking/justification
filter(weather_z, month(date) == 4 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank")

filter(weather_z, month(date) == 5 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank")

filter(weather_z, month(date) == 6 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank")

filter(weather_z, month(date) == 7 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank")

filter(weather_z, month(date) == 8 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank")

filter(weather_z, month(date) == 9 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank")


cor_789 <- create_corr(model_789)
# corrplot::corrplot(cor_789, method = "color",
#                    title = "July August September Correlation")
ggcorrplot(cor_789,
           colors = c("#E46726", "white", "#002abb"),
           type = "lower")

cor_456 <- create_corr(model_456)
# corrplot::corrplot(cor_456, method = "color")
ggcorrplot(cor_456,
           colors = c("#E46726", "white", "#002abb"),
           type = "lower")

model_simple <- lm(evi ~ level, model_789)
summary(model_simple)


model <- (lm(formula = evi_z ~ dtg_z*cum_vpd_30d + dtg_z*cum_ppt_30d, 
             data = model_456))
summary(model)
hist((model_789$evi_z))

model <- (lm(formula = evi_z ~ dtg_z + wy_z + cum_vpd_30d + cum_ppt_30d, 
             data = model_789))
model <- (lm(formula = dtg_z ~ cum_vpd_30d, 
             data = model_789))
summary(model)

ggplot(model_789, aes(x = cum_ppt_30d, y = evi))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_light()

model2 <- (lmer())
summary(model2)
r.squaredGLMM(model2)


plot(model_789$evi_z, fitted(model2))
abline(a = 0, b = 1, col = "red")

# model <- (lm(formula = evi_z ~ dtg_z*wy_z + dtg_z*cum_vpd_30d + dtg_z*cum_ppt_30d, 
#              data = model_789))
# 0.4984
# model <- (lm(formula = evi_z ~ dtg_z*wy_z + dtg_z*cum_vpd_30d + cum_ppt_30d, 
#              data = model_789))
# 0.497
# model <- (lm(formula = evi_z ~ dtg_z*wy_z + cum_vpd_30d + cum_ppt_30d
#              + tmean, 
#              data = model_789))
# 0.4861
# model <- (lm(formula = evi_z ~ dtg_z + wy_z + cum_vpd_30d + cum_ppt_30d, 
#              data = model_789))
# 0.4814
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model <- (lm(evi_z ~ dtg_z*wy_z + ppt + cum_vpd_10d,
#              model_789))
# dtg_z * wy_z has a somewhat significant term! (0.0547)
# R2 = 0.3969
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model <- (lm(evi_z ~ dtg_z*wy_z + ppt + dtg_z*cum_vpd_10d,
#              model_789))
# R2 = 0.4063
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# model <- (lm(evi_z ~ dtg_z*wy_z + dtg_z*ppt + dtg_z*cum_vpd_10d,
#              model_789))
# 0.4084
# model <- (lm(evi_z ~ dtg_z + wy_z + cum_vpd_30d + tmean,
#              model_789))
# 0.4453
# model <- (lm(evi_z ~ dtg_z + wy_z + cum_vpd_30d + tmean_30d,
#              model_789))
# 0.4483



model <- (lm(evi_z ~ dtg_z*wy_z + dtg_z*ppt + dtg_z*cum_vpd_10d,
             model_789))
# these are all significant!
summary(model)

# anova(model_simple, model)

plot(model_789$evi_z, fitted(model))
abline(a = 0, b = 1, col = "red")

model_mixed <- (lmer(evi_z ~ (1 + dtg_z | name),
                     model_789))
summary(model_mixed)
plot(model_789$evi_z, fitted(model_mixed))
abline(a = 0, b = 1, col = "red")


#~~~ 456:


model <- (lm(evi_z ~ month + dtg_z
             #+ cum_ppt_10d + wy_ppt + cum_vpd_10d
             ,
             model_456))
summary(model)

model_mixed <- (lmer(evi_z ~ (max_vpd_10d + wy_z | name),
                   model_456))
summary(model_mixed)
plot(model_456$evi_z, fitted(model_mixed))
abline(a = 0, b = 1, col = "red")


plot(model_456$evi_z, fitted(model))
abline(a = 0, b = 1, col = "red")



ggplot(model_456, aes(y = evi_z))





# 
# plot(filter(weather_z, well == "alluvial")$evi_z, fitted(model))
# abline(a = 0, b = 1, col = "red")
# 
# hist(residuals(model), breaks = 100)
# 
# ggplot(filter(weather_z, dtg_z < 5), aes(x = dtg_z, y = evi_z))+
#   geom_point(aes(color = name))+
#   geom_smooth(method = "lm", se = F)+
#   theme_light()+
#   facet_wrap(~year(date))+
#   theme(legend.position = "none")
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Monthly weather patterns:
# 
# monthly_normals <- monthly_weather %>% 
#   group_by(well, name, month) %>% 
#   summarise(ppt = mean(ppt),
#             tmean = mean(tmean),
#             vpdmax = mean(vpdmax))
# 
# ggplot(monthly_normals, aes(x = as.factor(month), y = ppt, 
#                             color = well, group = name))+
#   geom_line()+
#   theme_light()+
#   xlab("Month")+
#   ylab("Average monthly precipitation (mm)")+
#   theme(legend.position = "none")
# # ggsave("figures/well_ppt_trends.jpg", last_plot(), 
#        # width = 5, height = 5, units = "in")
# 
# ggplot(monthly_normals, aes(x = as.factor(month), y = vpdmax, 
#                             color = well, group = name))+
#   geom_line()+
#   theme_light()+
#   xlab("Month")+
#   ylab("Average daily maximum VPD (kPa)")
# # ggsave("figures/well_vpdmax_trends.jpg", last_plot(), 
#        # width = 6, height = 5, units = "in")
# 
# ggplot(monthly_normals, aes(x = as.factor(month), y = tmean, 
#                             color = well, group = name))+
#   geom_line()+
#   theme_light()+
#   xlab("Month")+
#   ylab("Average daily mean temperature (degC)")
# # ggsave("figures/well_tmean_trends.jpg", last_plot(), 
#        width = 6, height = 5, units = "in")
