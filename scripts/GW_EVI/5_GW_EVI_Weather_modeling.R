library(tidyverse)
library(lme4)
library(RcppRoll)
library(MuMIn)
library(ggcorrplot)
library(ggforce)
library(viridis)
library(broom)

# Read in processed weather data (4-5_Weather_Wrangling.R)
weather_annual_cumul <- read_csv("data/Processed/Weather_Cumulative.csv")

# Read in Z-score data:

z_scores <- read_csv("data/Processed/USP_GW_EVI_Z_FullYear_10312024.csv") %>%
  mutate(date = as.POSIXct(gsub(date, pattern=" 07:00:00", 
                                replacement="", fixed=T))) %>% 
  select(date, name, well, level, evi, evi_mean, evi_sd, dtg_mean,
         dtg_sd, evi_sd, dtg_sd, evi_z, dtg_z, month, reach, year)

alluvial_landfire <-read_csv("data/Landcover/USP_AlluvialWells_LF2016.csv") %>% 
  select(-year)

weather_z <- full_join(z_scores, weather_annual_cumul) %>%  # alluvial areas
  filter(well == "alluvial") %>% 
  full_join(alluvial_landfire, relationship = "many-to-many")
  
upland_z <- full_join(z_scores, weather_annual_cumul) %>%  # upland areas
  filter(well == "regional") %>% 
  full_join(alluvial_landfire, relationship = "many-to-many")
  
# ggplot(upland_z, aes(x = vpdmax, y = evi_z))+
#   geom_point(aes(color = month))+
#   theme_light(base_size = 20)+
#   labs(x = "Maximum daily VPD (kPa)", y = "Daily EVI Z-score")

total_z <- rbind(upland_z, weather_z) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

# write_csv(total_z, "data/Processed/GW_EVI_Weather.csv")

upland_0 <- filter(upland_z, cum_ppt_30d == 0)
riparian_0 <- filter(weather_z, cum_ppt_30d == 0)
total_0 <- filter(total_z, cum_ppt_30d == 0)

correlations_dtg <- total_z %>% 
  group_by(well, month) %>% 
  summarise(cor = (cor.test(evi_z, dtg_z))$estimate,
            ci_lo = (cor.test(evi_z, dtg_z))$conf.int[1],
            ci_up = (cor.test(evi_z, dtg_z))$conf.int[2],
            p = (cor.test(evi_z, dtg_z))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "p <= 0.05",
                         p > 0.05 ~ " p > 0.05"))

correlations_vpd <- total_z %>% 
  group_by(well, month) %>% 
  summarise(cor = (cor.test(evi, cum_vpd_30d))$estimate,
            ci_lo = (cor.test(evi, cum_vpd_30d))$conf.int[1],
            ci_up = (cor.test(evi, cum_vpd_30d))$conf.int[2],
            p = (cor.test(evi, cum_vpd_30d))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "p <= 0.05",
                         p > 0.05 ~ " p > 0.05"))

correlations_ppt <- total_z %>% 
  group_by(well, month) %>% 
  summarise(cor = (cor.test(evi, cum_ppt_30d))$estimate,
            ci_lo = (cor.test(evi, cum_ppt_30d))$conf.int[1],
            ci_up = (cor.test(evi, cum_ppt_30d))$conf.int[2],
            p = (cor.test(evi, cum_ppt_30d))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "p <= 0.05",
                         p > 0.05 ~ " p > 0.05"))

ggplot(correlations_ppt, aes(x = as.factor(month), y = cor, shape = sig))+
  geom_line(aes(group = well), linetype = 2, color = "gray60")+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 3)+
  theme_light(base_size = 20)+
  geom_hline(yintercept = 0, color = "gray70")+
  labs(x = "Month", y = "Correlation coefficient", 
       color = "Well", shape = "Different from zero?")

ggplot(filter(total_z), aes(x = (cum_ppt_30d), y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", color = "Month")+
  facet_wrap(~well)+
  geom_smooth(method = "lm", color = "gray80")+
  scale_color_viridis()+
  # scale_colour_gradient2(low = "red", mid = "purple", high = "blue3",
  #                       midpoint = 6)+
  geom_point(alpha = 0.5, aes(color = month))+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

ggplot(filter(total_0), aes(x = cum_vpd_30d, y = (evi)))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative max VPD (kPa)", y = "EVI", color = "Month")+
  #scale_color_viridis()+
  scale_colour_gradient2(low = "red", mid = "purple", high = "blue3",
                         midpoint = 6)+
  geom_smooth(method = "lm", color = "gray80")+
  geom_point(alpha = 0.5, aes(color = month))+
  facet_wrap(~well)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

# compare regressions
mult_model <- lm(evi ~ cum_ppt_30d*well, total_z)
summary(mult_model)
plot(mult_model, 1)

anova(lm(evi ~ cum_ppt_30d, upland_z), lm(evi ~ cum_ppt_30d*cum_vpd_30d, upland_z))
anova(lm(evi ~ cum_ppt_30d, weather_z), lm(evi ~ cum_ppt_30d*cum_vpd_30d, weather_z))

upland_model <- lm(evi ~ cum_ppt_30d, upland_z)
summary(upland_model) # adj R2 0.3999; slope 0.00091057 (p < 0.00001)
plot(upland_model, 1)

riparian_model <- lm(evi ~ cum_ppt_30d, weather_z) # evi is more closely coupled with temperature than vpd
summary(riparian_model) # adj R2 0.3035; slope 0.00126 (p < 0.00001) (38% greater than upland slope)
plot(riparian_model, 1)


# ZERO PPT - VPD MODELS:

upland_0m <- lm(evi ~ cum_vpd_30d, upland_0)
summary(upland_0m) # adj R2 -0.001675; slope 0.00003163 (p = 0.459)
# slope is probably not different than zero
riparian_0m <- lm(evi ~ cum_vpd_30d, riparian_0) # R2 0.2673
summary(riparian_0m) # adj R2 0.302; slope = 0.00073437 (p < 0.00001)

total_0m1 <- lm(evi ~ cum_vpd_30d + well, total_0)
summary(total_0m1)
total_0m2 <- lm(evi ~ cum_vpd_30d * well, total_0)
summary(total_0m2)

summary(lm(evi~ dtg_z, riparian_0)) #0.0908
summary(lm(evi~ cum_vpd_30d, riparian_0)) #0.0908
#cor(riparian_0$evi_z, riparian_0$dtg_z)



ggplot(filter(total_0), aes(x = cum_vpd_30d, y = evi))+
  geom_point(aes(color = month(date)))+
  theme_light()+
  geom_smooth(method = "lm")+
  facet_wrap(~well)+
  labs(x = "Cumulative 30-day daily max VPD (kPa)", y = "EVI",
       color = "Month", fill = "Month")

# Correlations:
upland_cor <- select(upland_z, evi, sdr_30d)
cor.test(upland_z$evi, upland_z$sdr_30d) #0.76
cor.test(weather_z$evi, weather_z$sdr_30d) # 0.66
cor.test(upland_z$evi, upland_z$cum_vpd_30d) # -0.20
cor.test(weather_z$evi, weather_z$cum_vpd_30d) # 0.11
cor.test(upland_z$evi, upland_z$cum_ppt_30d) # 0.75
cor.test(weather_z$evi, weather_z$cum_ppt_30d) # 0.67

cor.test(upland_0$evi, upland_0$cum_vpd_30d) # 0.113
cor.test(riparian_0$evi, riparian_0$cum_vpd_30d) # 0.518
cor.test(upland_0$evi, upland_0$vpdmax) # 0.14
cor.test(riparian_0$evi, riparian_0$vpdmax) # 0.48
cor.test(riparian_0$evi, riparian_0$level) # 0.04
cor.test(upland_0$evi, upland_0$level) # -0.18

ggplot(riparian_0, aes(x = vpdmax, y = evi_z))+
  geom_point()+
  #facet_wrap(~reach)+
  geom_smooth(method = "lm")
ggplot(riparian_0, aes(x = dtg_z, y = evi_z))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~reach)

plot(fitted(upland_model), resid(upland_model))
plot(fitted(riparian_model), resid(riparian_model))

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
             type = "lower", lab = T, sig.level = 0.05, insig = "blank",
             title = "April")

filter(weather_z, month(date) == 5 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank",
             title = "May")

filter(weather_z, month(date) == 6 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank",
             title = "June")

filter(weather_z, month(date) == 7 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank",
             title = "July")

filter(weather_z, month(date) == 8 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank",
             title = "August")

filter(weather_z, month(date) == 9 & !is.na(evi_z)) %>% 
  create_corr() %>% 
  ggcorrplot(colors = c("#E46726", "white", "#002abb"),
             type = "lower", lab = T, sig.level = 0.05, insig = "blank",
             title = "September")


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
