library(tidyverse)
library(broom)

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date))
evi <- read_csv("data/Processed/USP_EVI_Z_Seasonal_01032025.csv") %>% 
  mutate(date = date(date))

# # name == "D-22-22 17BDD2 [COTUWD]"
# ggplot(filter(evi, well == "alluvial"), aes(x = date, y = evi))+
#   geom_line()+
#   geom_smooth()

sites <- filter(evi, !is.na(evi)) %>% 
  select(name, well) %>% 
  distinct()
# write_csv(sites, "data/Wells_Not_Masked.csv")

weather_evi <- full_join(weather, evi) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland")) %>% 
  filter(!is.na(month)) # where do the NA months even come from anyway?

weather %>% 
  group_by(well) %>% 
  filter(month(date) %in% c(7, 8, 9)) %>% 
  summarise(cor(cum_vpd_30d, cum_ppt_30d)) 
# VPD - PPT cor during monsoon ranges -0.638 in uplands, -0.668 in riparian

weather %>% 
  group_by(well) %>% 
  filter(!is.na(cum_ppt_30d)) %>% 
  summarise(cor(cum_vpd_30d, cum_ppt_30d))
# VPD - PPT cor during entire period ranges 0.076 in uplands, 0.101 in riparian

summary(lm(evi ~ cum_ppt_30d + cum_vpd_30d + well, data = weather_evi))

# after running 3_EVI.R and 2_Wrangle_Weather.R

well_colors <- c("slateblue", "darkgoldenrod3")

# create monthly cycles:
monthly_cycles <- weather_evi %>% 
  mutate(month = month(date)) %>% 
  group_by(well, month) %>% 
  summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T),
            mean_vpd = mean(vpdmax, na.rm = T), sd_vpd = sd(vpdmax, na.rm = T),)

ggplot()+
  geom_ribbon(data = monthly_cycles, alpha = 0.3,
              aes(fill = well, x = month, y = mean_evi,
                  ymin = (mean_evi - sd_evi),
                  ymax = (mean_evi + sd_evi)))+
  geom_line(data = monthly_cycles, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
            linewidth = 0.5,
            aes(x = month, y = mean_evi, color = well))+
  scale_color_manual(values = well_colors)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  facet_wrap(~well)+
  labs(x = "Month", y = "Mean daily EVI", fill = "Well", color = "Well")


# add shaded bar from X1 to X2
ggplot()+
  geom_ribbon(data = monthly_cycles, alpha = 0.3,
              aes(fill = well, x = month, y = mean_vpd,
                  ymin = mean_vpd - sd_vpd,
                  ymax = mean_vpd + sd_vpd))+
  geom_line(data = monthly_cycles, linetype = 1,
            linewidth = 0.5,
            aes(x = month, y = mean_vpd, color = well))+
  scale_color_manual(values = well_colors)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  facet_wrap(~well)+
  labs(x = "Month", y = "Mean daily maximum VPD (kPa)", fill = "Well", color = "Well")
# ggsave("figures/vpd_anncycle.jpg", last_plot(), height = 5, width = 7, units = "in")
ggplot()+
  geom_rect(aes(xmin = 182, xmax = 273, ymin = -Inf, ymax = Inf),
            fill = "lightgray", alpha = 0.5)+
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


monsoon_cor1 <- weather_evi %>%
  filter(month %in% c(7, 8, 9)) %>% 
  group_by(well) %>% 
  dplyr::summarise(cor = (cor.test(x = evi, cum_ppt_30d, use = "pairwise.complete.obs"))$estimate,
                   lo = (cor.test(x = evi, cum_ppt_30d, use = "pairwise.complete.obs"))$conf.int[1],
                   hi = (cor.test(x = evi, cum_ppt_30d, use = "pairwise.complete.obs"))$conf.int[2]) %>% 
  mutate(period = "Monsoon", var = "30-day PPT")

monsoon_cor2 <- weather_evi %>%
  filter(month %in% c(7, 8, 9)) %>% 
  group_by(well) %>% 
  dplyr::summarise(cor = (cor.test(x = evi, cum_vpd_30d, use = "pairwise.complete.obs"))$estimate,
                   lo = (cor.test(x = evi, cum_vpd_30d, use = "pairwise.complete.obs"))$conf.int[1],
                   hi = (cor.test(x = evi, cum_vpd_30d, use = "pairwise.complete.obs"))$conf.int[2],) %>% 
  mutate(period = "Monsoon", var = "30-day VPD")
monsoon_cor <- rbind(monsoon_cor1, monsoon_cor2)

full_cor1 <- weather_evi %>%
  group_by(well) %>% 
  dplyr::summarise(cor = (cor.test(x = evi, cum_ppt_30d, use = "pairwise.complete.obs"))$estimate,
                   lo = (cor.test(x = evi, cum_ppt_30d, use = "pairwise.complete.obs"))$conf.int[1],
                   hi = (cor.test(x = evi, cum_ppt_30d, use = "pairwise.complete.obs"))$conf.int[2]) %>% 
  mutate(period = "Full Year", var = "30-day PPT")
full_cor2 <- weather_evi %>%
  group_by(well) %>% 
  dplyr::summarise(cor = (cor.test(x = evi, cum_vpd_30d, use = "pairwise.complete.obs"))$estimate,
                   lo = (cor.test(x = evi, cum_vpd_30d, use = "pairwise.complete.obs"))$conf.int[1],
                   hi = (cor.test(x = evi, cum_vpd_30d, use = "pairwise.complete.obs"))$conf.int[2],) %>% 
  mutate(period = "Full Year", var = "30-day VPD")
full_cor <- rbind(full_cor1, full_cor2)


cor_table <- rbind(monsoon_cor, full_cor)


# rsq <- data.frame(
#   c("Riparian", "Riparian", "Riparian", "Riparian", 
#     "Upland", "Upland", "Upland", "Upland"),
#   c("PPT 30d", "VPD 30d", "PPT 30d", "VPD 30d",
#     "PPT 30d", "VPD 30d", "PPT 30d", "VPD 30d"),
#   c("Full year", "Full year", "Monsoon", "Monsoon",
#     "Full year", "Full year", "Monsoon", "Monsoon")
# )
# names(rsq) <- c("well", "var", "time")
# 
# rsq$r_sq <- c(34.2, 25.6, 29, 28.7,
#               35, 8, 29.4, 30)
# 
# rsq <- rsq %>% 
#   mutate(var = case_when(var == "PPT 30d" ~ "30-day precipitation",
#                          var == "VPD 30d" ~ "30-day daily max VPD"))
#   
# ggplot(rsq, aes(x = time, 
#                 y = r_sq, 
#                 fill = well))+
#   geom_bar(stat = "identity", position = "dodge")+
#   facet_wrap(~var)+
#   scale_fill_manual(values = well_colors)+
#   theme_light(base_size = 30)+
#   theme(strip.background = element_rect(color = "black", fill = "white"))+
#   theme(strip.text = element_text(colour = 'black'))+
#   labs(x = "Time period", y = "Coefficient of determination",
#        fill = "Well location")+
#   theme(legend.position = "none")

ggplot(cor_table, aes(x = period, 
                y = cor, 
                fill = well,
                group = well))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin = lo, ymax = hi, group = interaction(period, well)), 
                position = position_dodge(),
                alpha = 0.5)+
  facet_wrap(~var)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x = "Time period", y = "Correlation coefficient",
       fill = "Well location")+
  theme(legend.position = "none")


summary(lm(evi ~ cum_ppt_30d, subset(weather_evi, well == "Upland" 
                                     )))
# & month %in% c(7, 8, 9)

ggplot(filter(weather_evi, month %in% c(7, 8, 9)), 
              aes(x = cum_ppt_30d, y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", color = "Month")+
  facet_wrap(~well)+
  scale_colour_gradient2(low = "red", mid = "purple", high = "blue3",
                        midpoint = 8)+
  geom_point(alpha = 0.5, aes(color = month), pch = 1)+
  geom_smooth(method = "lm", color = "gray60")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

ggplot(filter(weather_evi, month %in% c(7, 8, 9)),
       aes(x = cum_vpd_30d, y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative max VPD (kPa)", y = "EVI", color = "Month")+
  facet_wrap(~well)+
  scale_colour_gradient2(low = "red", mid = "purple", high = "blue3",
                         midpoint = 8)+
  geom_point(alpha = 0.5, aes(color = month), pch = 1)+
  geom_smooth(method = "lm", color = "gray60")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))



#####################
ggplot(filter(weather_evi, month %in% c(7, 8, 9)), 
              aes(x = cum_ppt_30d, y = evi, color = well))+
  theme_minimal(base_size = 40)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", color = "Month")+
  geom_smooth(method = "lm")+
  ylim(c(0, 0.5))+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
summary(lm(evi ~ cum_ppt_30d, filter(weather_evi, well == "Upland")))

ggplot(filter(weather_evi, month %in% c(7, 8, 9)), 
       aes(x = cum_vpd_30d, y = evi, color = well))+
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
