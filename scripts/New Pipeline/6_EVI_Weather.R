library(tidyverse)
library(broom)

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date))
evi <- read_csv("data/Processed/USP_EVI_Z_11132024.csv") %>% 
  mutate(date = date(date))

sites <- select(evi, name, well) %>% 
  distinct()
# write_csv(sites, "data/Wells_Not_Masked.csv")

weather_evi <- full_join(weather, evi) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland")) %>% 
  filter(!is.na(month)) # where do the NA months even come from anyway?


# after running 3_EVI.R and 2_Wrangle_Weather.R

well_colors <- c("slateblue", "darkgoldenrod3")

ggplot()+
  geom_ribbon(data = weather_ann_daily, alpha = 0.3,
              aes(fill = well, x = doy, y = mean_vpdmax,
                  ymin = mean_vpdmax - sd_vpdmax,
                  ymax = mean_vpdmax + sd_vpdmax))+
  geom_line(data = weather_ann_daily, linetype = 1,
            linewidth = 0.5,
            aes(x = doy, y = mean_vpdmax, color = well))+
  scale_color_manual(values = well_colors)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  labs(x = "DOY", y = "Mean daily maximum VPD (kPa)", fill = "Well", color = "Well")
# ggsave("figures/vpd_anncycle.jpg", last_plot(), height = 5, width = 7, units = "in")
ggplot()+
  geom_ribbon(data = evi_ann_daily, alpha = 0.3,
              aes(fill = well, x = doy, y = mean_evi,
                  ymin = (mean_evi - sd_evi),
                  ymax = (mean_evi + sd_evi)))+
  geom_line(data = evi_ann_daily, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
            linewidth = 0.5,
            aes(x = doy, y = mean_evi, color = well))+
  scale_color_manual(values = well_colors)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  labs(x = "DOY", y = "Mean daily EVI", fill = "Well", color = "Well")
#ggsave("figures/evi_anncycle.jpg", last_plot(), height = 5, width = 7, units = "in")

rsq <- data.frame(
  c("Riparian", "Riparian", "Riparian", "Riparian", 
    "Upland", "Upland", "Upland", "Upland"),
  c("PPT 30d", "VPD 30d", "PPT 30d", "VPD 30d",
    "PPT 30d", "VPD 30d", "PPT 30d", "VPD 30d"),
  c("Full year", "Full year", "Monsoon", "Monsoon",
    "Full year", "Full year", "Monsoon", "Monsoon")
)
names(rsq) <- c("well", "var", "time")

rsq$r_sq <- c(34.2, 25.6, 29, 28.7,
              35, 8, 29.4, 30)

rsq <- rsq %>% 
  mutate(var = case_when(var == "PPT 30d" ~ "30-day precipitation",
                         var == "VPD 30d" ~ "30-day daily max VPD"))
  
ggplot(rsq, aes(x = time, 
                y = r_sq, 
                fill = well))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~var)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Time period", y = "Coefficient of determination",
       fill = "Well location")+
  theme(legend.position = "none")


summary(lm(evi ~ cum_vpd_30d, subset(weather_evi, well == "Riparian" 
                                     & month %in% c(7, 8, 9))))


ggplot(weather_evi, aes(x = cum_ppt_30d, y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", color = "Month")+
  facet_wrap(~well)+
  geom_smooth(method = "lm", color = "gray80")+
  scale_colour_gradient2(low = "red", mid = "purple", high = "blue3",
                        midpoint = 6)+
  geom_point(alpha = 0.5, aes(color = month), pch = 1)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

ggplot(weather_evi, aes(x = cum_ppt_30d, y = evi, color = well))+
  theme_minimal(base_size = 40)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", color = "Month")+
  geom_smooth(method = "lm")+
  ylim(c(0, 0.5))+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
summary(lm(evi ~ cum_ppt_30d, subset(weather_evi, well == "Upland")))

ggplot(weather_evi, aes(x = cum_vpd_30d, y = evi, color = well))+
  theme_minimal(base_size = 40)+
  ylim(c(0, 0.5))+
  #geom_point(aes(color = well), pch = 1, alpha = 0.2)+
  labs(x = "30-day cumulative VPD (kPa)", y = "EVI", color = "Month")+
  geom_smooth(method = "lm", se = T)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

cor.test(weather_evi$cum_ppt_30d, 
         weather_evi$cum_vpd_30d) # r = 0.06

summary(lm(evi ~ cum_ppt_30d + cum_vpd_30d,
           subset(weather_evi, well == "Riparian" & month %in% c(7,8,9))))
summary(lm(evi ~ cum_ppt_30d,
           subset(weather_evi, well == "regional")))

# Regression coefficient confidence intervals
# estimate + SE*t
# critical t-value: Riparian regression (EVI - PPT)
qt(0.975, 352743)
(1.326*(10^(-3))) - ((3.097*(10^(-6)))*(qt(0.975, 352743)))
(1.326*(10^(-3))) + ((3.097*(10^(-6)))*(qt(0.975, 352743)))

# critical t-value: Upland regression (EVI - PPT)
qt(0.975, 197572)
(9.352*(10^(-4))) - (2.867*(10^(-6))*qt(0.975, 197572))
(9.352*(10^(-4))) + (2.867*(10^(-6))*qt(0.975, 197572))


summary(lm(evi ~ cum_vpd_30d, subset(weather_evi, well == "Upland")))

# riparian regression for VPD
(1.032*(10^(-3))) - (2.96*(10^(-6))*qt(0.975, 352743))
(1.032*(10^(-3))) + (2.96*(10^(-6))*qt(0.975, 352743))
qt(0.975, 352743)

# upland regression for VPD
(4.533*(10^(-4))) - (3.454*(10^(-6))*qt(0.975, 197572))
(4.533*(10^(-4))) + (3.454*(10^(-6))*qt(0.975, 197572))

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
