library(tidyverse)
library(cars)
library(lme4)
library(MuMIn)
library(nlme)
install.packages("lmerTest")
library(lmerTest)
# install.packages("emmeans")
library(emmeans)
# install.packages("pbkrtest")
library(pbkrtest)

# weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
#   mutate(date = date(date))
# evi <- read_csv("data/Processed/USP_EVI_Z_Seasonal_01032025.csv") %>% 
#   mutate(date = date(date))
# weather_evi <- full_join(weather, evi) %>% 
#   mutate(well = case_when(well == "alluvial" ~ "Riparian",
#                           well == "regional" ~ "Upland")) %>% 
#   filter(!is.na(month))
# weather_evi_monthly <- weather_evi %>% 
#   mutate(month = month(date), year = year(date)) %>% 
#   group_by(well, name, szn, month, year) %>% 
#   summarise(evi_mean = mean(evi, na.rm = T), evi_max = max(evi),
#             ppt = sum(ppt), vpd_max = max(vpdmax),
#             vpdmean = mean(vpdmax, na.rm = T))
# weather_evi_monthly$szn <- factor(weather_evi_monthly$szn, levels = c("JFM", "AMJ", "JAS", "OND"))
# monsoon_data <- filter(weather_evi_monthly, month %in% c(7, 8, 9))
# # rip_mon <- filter(monsoon_data, well == "Riparian")
# # up_mon <- filter(monsoon_data, well == "Upland")
# rip_all <- filter(weather_evi_monthly, well == "Riparian")
# up_all <- filter(weather_evi_monthly, well == "Upland")

# rip_all <- filter(weather_evi_monthly, well == "Riparian") %>% 
#   mutate(case_when(month(date) %in% c(1, 2, 3) ~ "JFM",
#                    month(date) %in% c(4, 5, 6) ~ "AMJ",
#                    month(date) %in% c(7, 8, 9)))

model_in <- read_csv("data/Processed/ModelInputs_AggregatedtoPRISMPixel_Landsat_09172025.csv")
rip_all <- filter(weather_evi_monthly, well == "Riparian") %>% 
  filter(!is.na(ppt), !is.na(vpd_max), !is.na(evi)) %>% 
  mutate(ppt = scale(ppt), vpd_max = scale(vpd_max))
up_all <- filter(weather_evi_monthly, well == "Upland") %>% 
  filter(!is.na(ppt), !is.na(vpd_max), !is.na(evi)) %>% 
  mutate(ppt = scale(ppt), vpd_max = scale(vpd_max))

# not straightforward to me whether it's better to scale or not.

rip_jfm <- filter(rip_all, month %in% c(1, 2, 3))
rip_amj <- filter(rip_all, month %in% c(4, 5, 6))
rip_jas <- filter(rip_all, month %in% c(7, 8, 9))
rip_ond <- filter(rip_all, month %in% c(10, 11, 12))

up_jfm <- filter(up_all, month %in% c(1, 2, 3))
up_amj <- filter(up_all, month %in% c(4, 5, 6))
up_jas <- filter(up_all, month %in% c(7, 8, 9))
up_ond <- filter(up_all, month %in% c(10, 11, 12))

# model selection! ~

emm_options(lmerTest.limit = 20000)

# Riparian JFM: REPEAT this exact analysis for the rest of the seasons/landscapes
    # incorporate DTG into the same model?
    # but also talk to Henry

cor.test(rip_jfm$ppt, rip_jfm$vpd_max) # pretty significantly correlated, shouldn't include together

# now we ask which single variable is more predictive?

AIC(lmer(evi ~ ppt + (1 | prism), data = rip_jfm, REML = F))
r.squaredGLMM(lmer(evi ~ ppt + (1 | prism), data = rip_jfm, REML = F))

AIC(lmer(evi ~ vpd_max + (1 | prism), data = rip_jfm, REML = F))
r.squaredGLMM(lmer(evi ~ vpd_max + (1 | prism), data = rip_jfm, REML = F))

best_rip_jfm <- lmer(evi ~ vpd_max + (1 | prism), data = rip_jfm, REML = F)

anova(best_rip_jfm)

# Riparian AMJ:

cor.test(rip_amj$ppt, rip_amj$vpd_max) # abs(correlation) is <0.2 so I'm gonna include it
# is that a threshold that makes sense?
# for variables less correlated than (xxxxxx).......
# what if i use 0.2 for cutoff??? lol I NEED TO FIND A PAPER sldfkjlskg

AIC(lmer(evi ~ ppt + (1 | prism), data = rip_amj, REML = F))
r.squaredGLMM(lmer(evi ~ ppt + (1 | prism), data = rip_amj, REML = F))

AIC(lmer(evi ~ vpd_max + (1 | prism), data = rip_amj, REML = F))
r.squaredGLMM(lmer(evi ~ vpd_max + (1 | prism), data = rip_amj, REML = F))

AIC(lmer(evi ~ vpd_max*ppt + (1 | prism), data = rip_amj, REML = F))
r.squaredGLMM(lmer(evi ~ vpd_max*ppt + (1 | prism), data = rip_amj, REML = F))

best_rip_amj <- lmer(evi ~ vpd_max + (1 | prism), data = rip_amj, REML = F)
anova(best_rip_amj)
r.squaredGLMM(best_rip_amj)

# Riparian JAS:

cor.test(rip_jas$ppt, rip_jas$vpd_max) # abs val corr < 0.2... include!

AIC(lmer(evi ~ scale(ppt) + (1 | prism), data = rip_jas, REML = F))
r.squaredGLMM(lmer(evi ~ scale(ppt) + (1 | prism), data = rip_jas, REML = F))

AIC(lmer(evi ~ scale(vpd_max) + (1 | prism), data = rip_jas, REML = F))
r.squaredGLMM(lmer(evi ~ scale(vpd_max) + (1 | prism), data = rip_jas, REML = F))

AIC(lmer(evi ~ scale(vpd_max)*scale(ppt) + (1 | prism), data = rip_jas, REML = F))
r.squaredGLMM(lmer(evi ~ scale(vpd_max)*scale(ppt) + (1 | prism), data = rip_jas, REML = F))

anova(lmer(evi ~ scale(vpd_max):scale(ppt) + scale(vpd_max) + (1 | prism), data = rip_jas, REML = F))

# remove lone vpd variable (but keep interaction) as the best model:
best_rip_jas <- lmer(evi ~ log(ppt) + log(ppt):vpd_max + (1 | prism), data = rip_jas, REML = F)
anova(best_rip_jas)



# Riparian OND:

cor.test(rip_ond$ppt, rip_ond$vpd_max) # cor much less than 0.1


# Upland:

AIC(lmer(evi_mean ~ ppt + (1 | name), data = up_all, REML = F))
summary(lmer(evi_mean ~ ppt + (1 | name), data = up_all, REML = F))
r.squaredGLMM(lmer(evi_mean ~ ppt + (1 | name), data = up_all, REML = F))

AIC(lmer(evi_mean ~ vpd_max + (1 | name), data = up_all, REML = F))
r.squaredGLMM(lmer(evi_mean ~ vpd_max + (1 | name), data = up_all, REML = F))

AIC(lmer(evi_mean ~ vpd_max*ppt + (1 | name), data = up_all, REML = F))
r.squaredGLMM(lmer(evi_mean ~ vpd_max*ppt + (1 | name), data = up_all, REML = F))

AIC(lmer(evi_mean ~ szn*ppt + (1 | name), data = up_all, REML = F))
r.squaredGLMM(lmer(evi_mean ~ szn*ppt + (1 | name), data = up_all, REML = F))

AIC(lmer(evi_mean ~ vpd_max*szn + (1 | name), data = up_all, REML = F))
r.squaredGLMM(lmer(evi_mean ~ vpd_max*szn + (1 | name), data = up_all, REML = F))

AIC(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F))
AIC(lmer(evi_mean ~ ppt + vpd_max + szn + ppt:szn + vpd_max:szn + ppt:vpd_max:szn + (1 | name), 
         data = up_all, REML = F))

r.squaredGLMM(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all))
r.squaredGLMM(lmer(evi_mean ~ ppt + vpd_max + szn + ppt:szn + vpd_max:szn + ppt:vpd_max:szn + (1 | name), 
                   data = up_all, REML = F))

anova(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F))
summary(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F))


r.squaredGLMM(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F))


emmeans(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F), specs = "szn")

plot(emtrends(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F), var = "ppt"))
plot(emtrends(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = rip_all, REML = F),
        var = "ppt"))

# PPT effect sizes:
emmip(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all, REML = F), ppt ~ vpd_max | szn, 
      at=list(vpd_max=c(2.93, 4.1, 5.3)), # estimated at different quantiles of VPD
      CIs=TRUE)+
  xlab("max VPD quantiles (25, 50, 75%)")+
  ylab("Precipitation effect size on EVI (mm-1)")+
  theme_light(base_size = 26)+
  ylim(c(0, 0.4))
emmip(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = rip_all), ppt ~ vpd_max | szn, 
      at=list(vpd_max=c(3.06, 4.3, 5.5)), # estimated at different quantiles of VPD
      CIs=TRUE)+
  xlab("max VPD quantiles (25, 50, 75%)")+
  ylab("Precipitation effect size on EVI (mm-1)")+
  theme_light(base_size = 26)+
  ylim(c(0,0.4))


# VPD effect sizes:
emmip(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = up_all), vpd_max ~ ppt | szn, 
      at=list(ppt=c(0.69, 4.65, 18.84)), # estimated at different quantiles of PPT
      CIs=TRUE)+
  xlab("PPT quantiles (25, 50, 75%)")+
  ylab("Max VPD effect size on EVI (kPa-1)")+
  theme_light(base_size = 26)+
  ylim(c(0, 0.3))
emmip(lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), data = rip_all), vpd_max ~ ppt | szn, 
      at=list(ppt=c(0.47, 4.2, 17.27)), # estimated at different quantiles of PPT
      CIs=TRUE)+
  xlab("PPT quantiles (25, 50, 75%)")+
  ylab("Max VPD effect size on EVI (kPa-1)")+
  theme_light(base_size = 26)+
  ylim(c(0,0.3))

#

mod1 <- lm(evi_mean ~ ppt + vpd_max + szn,
            data = rip_all)
#r.squaredGLMM(mod1)
#AIC(mod1)
summary(mod1)$adj.r.squared

mod2 <- lm(evi_mean ~ ppt*vpd_max + szn, data = rip_all)
summary(mod2)$adj.r.squared

mod3 <- lm(evi_mean ~ ppt + vpd_max + ppt:vpd_max + vpd_max:szn + ppt:szn,
           data = rip_all)
summary(mod3)$adj.r.squared

mod4 <- lm(evi_mean ~ ppt*vpd_max*szn, data = rip_all)
summary(mod4)$adj.r.squared # 74.9%

mod5 <- lm(evi_mean ~ ppt*vpd_max*szn, data = up_all)
summary(mod5)$adj.r.squared # 57.7%

# adding in random structure:

mod_u1 <- lme4::lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), 
                     data = up_all)
r.squaredGLMM(mod_u1) # 57 m ; 69% c
# a better model compared to no fixed effects

mod_r1 <- lme4::lmer(evi_mean ~ ppt*vpd_max*szn + (1 | name), 
                     data = rip_all)
summary(mod_r1)
r.squaredGLMM(mod_r1) # 75 m ; 78% 
# a better model compared to no fixed effects

AIC(mod_r1) < AIC(mod4)
AIC(mod_u1) < AIC(mod5)



######
mod2 <- lme(evi_mean ~ ppt*szn + vpd_max*szn, random = ~ 1 + ppt + vpd_max + szn | name,
            na.action = na.omit, data = rip_all)
r.squaredGLMM(mod2)


# mod1 <- lme(evi_mean ~ ppt*szn + vpd_max*szn, random = ~ ppt*szn + vpd_max*szn | name,
#             data = rip_all, na.action = na.omit) # such an insane number of parameters being estimated
r.squaredGLMM(mod1)

mod2 <- lmer(evi_mean ~ (ppt + vpd_max + szn | name),
           REML = T, data = up_all)
summary(mod2) # 45.78%
r.squaredGLMM(mod2)


# models just to assess 

#monsoon:
ppt_rip_mon <- lme(evi_mean ~ ppt, method = "REML",
                   random = ~ ppt | name, data = rip_mon,
                   na.action = na.omit)
summary(ppt_rip_mon)
r.squaredGLMM(ppt_rip_mon)
ranef(ppt_rip_mon)

vpd_rip_mon <- lme(evi_mean ~ vpd_max, method = "REML",
                   random = ~ vpd_max | name, data = rip_mon,
                   na.action = na.omit)
summary(vpd_rip_mon)
ranef(vpd_rip_mon)
r.squaredGLMM(vpd_rip_mon)

      ppt_up_mon <- lme(evi_mean ~ ppt, method = "REML",
                         random = ~ ppt | name, data = up_mon,
                         na.action = na.omit) # no convergence
      ppt_up_mon2 <- lmer(evi_mean ~ ppt + (ppt|name), data = up_mon, REML = T)
      r.squaredGLMM(ppt_up_mon2)
      vpd_up_mon <- lme(evi ~ vpd_max, method = "REML",
                         random = ~ vpd_max | name, data = up_mon,
                         na.action = na.omit) # no convergence
      vpd_up_mon2 <- lmer(evi_mean ~ vpd_max + (vpd_max | name), data = up_mon, REML = T)
      r.squaredGLMM(vpd_up_mon2)

# year round:
ppt_rip_all <- lme(evi_mean ~ ppt, method = "REML",
                   random = ~ ppt | name, data = rip_all,
                   na.action = na.omit)
summary(ppt_rip_all)
r.squaredGLMM(ppt_rip_all)

vpd_rip_all <- lme(evi_mean ~ vpd_max, method = "REML",
                   random = ~ vpd_max | name, data = rip_all,
                   na.action = na.omit)
r.squaredGLMM(vpd_rip_all)
summary(vpd_rip_all)

      vpd_up_all <- lme(evi_mean ~ vpd_max, method = "REML",
                         random = ~ vpd_max | name, data = up_all,
                         na.action = na.omit) # no convergence ERROR
      vpd_up_all2 <- lmer(evi_mean ~ vpd_max + (vpd_max | name), data = up_all,
                          REML = T)
      r.squaredGLMM(vpd_up_all2)

ppt_up_all <- lme(evi ~ ppt, method = "REML",
                   random = ~ ppt | name, data = up_all,
                   na.action = na.omit)
summary(ppt_up_all)
ppt_up_all2 <- lmer(evi_mean ~ ppt + (ppt | name), data = up_all,
                    REML = T)
r.squaredGLMM(ppt_up_all2)

# models that converged:
# Full year riparian ppt & vpd; full year upland ppt; both riparian monsoon models




# Year-round PPT models:
ppt1 <- (gls(model = evi ~ 1 + ppt, method = "REML", 
     data = weather_evi_monthly, 
     na.action = "na.omit"))
summary(ppt1)
ppt2 <- lme(evi ~ ppt, data = weather_evi_monthly,
            random = ~ 1 | well, method = "REML",
            na.action = "na.omit")
summary(ppt2)
ppt3 <- lme(evi ~ ppt, data = weather_evi_monthly,
            random = ~ 1 + ppt | well, method = "REML",
            na.action = "na.omit") # random slope and intercept model fits best based on AIC
summary(ppt3); r.squaredGLMM(ppt3) # 34.6%
ppt4 <- lme(evi ~ ppt, data = weather_evi_monthly,
            random = ~ 1 + ppt | name, method = "REML", 
            na.action = "na.omit")
summary(ppt4); r.squaredGLMM(ppt4) # 37.6 # varying by site does not explain that much more var
AIC(ppt4) < AIC(ppt3) # but ppt4 is th better model even with more parameters?

# Year-round VPD models:
vpd1 <- gls(model = evi ~ 1 + vpd_max, method = "REML",
            data = weather_evi_monthly, na.action = "na.omit")
summary(vpd1)
vpd2 <- lme(evi ~ vpd_max, data = weather_evi_monthly,
            random = ~ 1 | well, method = "REML",
            na.action = "na.omit")
summary(vpd2)
vpd3 <- lme(evi ~ vpd_max, data = weather_evi_monthly,
            random = ~ 1 + vpd_max | well, method = "REML",
            na.action = "na.omit")
summary(vpd3)
vpd4 <- lme(evi ~ vpd_max, data = weather_evi_monthly,
            random = ~ (1 + vpd_max) | name, method = "REML",
            na.action = "na.omit")
summary(vpd4) # better model!
r.squaredGLMM(vpd3); r.squaredGLMM(vpd4)
anova(vpd1, vpd4)

# monsoon models:

ppt1_m <- (gls(model = evi ~ 1 + ppt, method = "REML", 
             data = monsoon_data, 
             na.action = "na.omit"))
summary(ppt1_m)
ppt2_m <- lme(evi ~ ppt, data = monsoon_data,
            random = ~ 1 | well, method = "REML",
            na.action = "na.omit")
summary(ppt2_m)
ppt3_m <- lme(evi ~ 1, data = monsoon_data,
            random = ~ 1 + ppt | well, method = "REML",
            na.action = "na.omit") # random slope and intercept model varying by well type fits best based on AIC
summary(ppt3_m); r.squaredGLMM(ppt3_m) # 32.9% # having precip as just a random effect improves model R2

ppt3_m <- lmer(evi ~ 1 + ppt + (1 + ppt | well), data = monsoon_data, REML = T)
ranova(ppt3_m) # likelihood ratio test: p < 0.05, so evidence suggests the random effects are noteable
r.squaredGLMM(ppt3_m) # 33.2%

# ppt4_m <- lme(evi ~ 1+ ppt, data = monsoon_data, # NO convergence
#             random = ~ 1 + ppt | name, method = "REML", 
#             na.action = "na.omit") # best model (AIC)

vpd1_m <- gls(model = evi ~ 1 + vpd_max, method = "REML",
            data = monsoon_data, na.action = "na.omit")
summary(vpd1_m)
vpd2_m <- lme(evi ~ vpd_max, data = monsoon_data,
            random = ~ 1 | well, method = "REML",
            na.action = "na.omit")
summary(vpd2_m)
vpd3_m <- lme(evi ~ vpd_max, data = monsoon_data,
            random = ~ 1 + vpd_max | well, method = "REML",
            na.action = "na.omit")
summary(vpd3_m)
vpd4_m <- lme(evi ~ 1 + vpd_max, data = monsoon_data,
              random = ~ 1 + vpd_max | name, method = "REML",
              na.action = "na.omit")
r.squaredGLMM(vpd4_m)
summary(vpd4_m)
ranef(vpd4_m) ; coef
vpd4_m$coefficients$fixed

ggplot(mapping = aes(x = filter(monsoon_data, !is.na(vpd_max))$evi, y = fitted(vpd4_m)))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

AIC(vpd1_m)
AIC(vpd2_m) # better model
AIC(vpd3_m)
AIC(vpd4_m) # lowest model

# PPT models:
summary(lm(evi_mean ~ ppt, data = filter(weather_evi_monthly, well == "Riparian")))
summary(lm(evi_mean ~ ppt, data = filter(weather_evi_monthly, well == "Upland")))
summary(lm(evi_mean ~ ppt, data = filter(monsoon_data, well == "Riparian")))
summary(lm(evi_mean ~ ppt, data = filter(monsoon_data, well == "Upland")))

# VPD models:
summary(lm(evi_mean ~ vpd_max, data = filter(weather_evi_monthly, well == "Riparian")))
summary(lm(evi_mean ~ vpd_max, data = filter(weather_evi_monthly, well == "Upland")))
summary(lm(evi_mean ~ vpd_max, data = filter(monsoon_data, well == "Riparian")))
summary(lm(evi_mean ~ vpd_max, data = filter(monsoon_data, well == "Upland")))

# Mixed models:

mixed_ppt_m <- lmer(evi ~ ppt + (1 | well), data = monsoon_data)
summary(mixed_ppt_m)
r.squaredGLMM(mixed_ppt_m) # 33.02%
# AIC(mixed_ppt_m)

mixed_vpd_m <- lmer(evi ~ vpd_max + (1 | well), data = monsoon_data) # only have vpd slope as random?
summary(mixed_vpd_m)
r.squaredGLMM(mixed_vpd_m) # is failing to converge a bad thing or just a "warning"?
# 54.39%

# no fixed fx: 96.49% (95% fixed fx)
# adding 0 outside of parentheses: No fixed effects, even for intercept
# add 0 inside parenthesis: no random intercepts
# no specification: random effect of variable with fixed + random intercept
# intercept fixed: 66.9% (same as not including any 0 or 1)
# intercept random: 96.4%

# need to double check how to denote the dummy variables for random intercepts n whatnot

mixed_ppt <- lmer(evi ~ 0 + (ppt | name), data = weather_evi_monthly)
r.squaredGLMM(mixed_ppt) # 87.44%
# can i use these mixed models to assess if within-well variance is higher 
# at either riparian or upland areas? how to make this "nested" model?

mixed_vpd <- lmer(evi ~ 0 + (vpd_max | name), data = weather_evi_monthly)
r.squaredGLMM(mixed_vpd) # 90.6% # only random fx

mixed_vpd_w <- lmer(evi ~ 0 + (vpd_max | well), data = weather_evi_monthly)
r.squaredGLMM(mixed_vpd_w) # 89%

# soooo.. vpdmax and ppt are not correlated at all (insignificant; p = 0.5893)