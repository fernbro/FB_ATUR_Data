library(tidyverse)
library(broom)
# install.packages("ggnewscale")
library(ggnewscale)
library(lme4)
library(MuMIn)
library(DescTools)

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date))
evi <- read_csv("data/Processed/USP_EVI_Z_Seasonal_01032025.csv") %>% 
  mutate(date = date(date))

well_colors <- c("slateblue1", "darkgoldenrod3")

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


# get to a monthly scale:
weather_evi_monthly <- weather_evi %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(well, name, month, year) %>% 
  summarise(evi = mean(evi, na.rm = T),
            ppt = sum(ppt), vpd_max = max(vpdmax),
            vpdmean = mean(vpdmax, na.rm = T))


ppt_cor_monthly_full <- weather_evi_monthly %>% 
  filter(!is.na(evi), !is.na(ppt)) %>% 
  group_by(well, month) %>% 
  dplyr::summarise(n = n(),
            cor = (cor.test(evi, ppt, use = "pairwise.complete.obs", conf.level = 0.95))$estimate,
            ci_lo = (cor.test(evi, ppt, use = "pairwise.complete.obs", conf.level = 0.95))$conf.int[1],
            ci_up = (cor.test(evi, ppt, use = "pairwise.complete.obs", conf.level = 0.95))$conf.int[2]
  ) %>% 
  mutate(var = "ppt")

vpd_cor_monthly_full <- weather_evi_monthly %>% 
  filter(!is.na(evi), !is.na(vpd_max)) %>% 
  group_by(well, month) %>% 
  summarise(n = n(),
            cor = (cor.test(evi, vpd_max, use = "pairwise.complete.obs", conf.level = 0.95))$estimate,
            ci_lo = (cor.test(evi, vpd_max, use = "pairwise.complete.obs", conf.level = 0.95))$conf.int[1],
            ci_up = (cor.test(evi, vpd_max, use = "pairwise.complete.obs", conf.level = 0.95))$conf.int[2]
  ) %>% 
  mutate(var = "vpd")

ggplot(ppt_cor_monthly_full, aes(x = as.factor(month), y = cor))+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  scale_color_manual(values = well_colors)+
  theme_light(base_size = 30)+
  geom_hline(yintercept = 0, color = "gray60")+
  labs(x = "Month", y = "Pearson's correlation", 
       color = "Well location")

ggplot(vpd_cor_monthly_full, aes(x = as.factor(month), y = cor))+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  scale_color_manual(values = well_colors)+
  theme_light(base_size = 30)+
  geom_hline(yintercept = 0, color = "gray60")+
  labs(x = "Month", y = "Pearson's correlation", 
       color = "Well location")

combo_monthly_cor <- rbind(ppt_cor_monthly_full, vpd_cor_monthly_full) %>% 
  pivot_wider(names_from = var, values_from = c("cor", "ci_lo", "ci_up"))

ggplot(data = combo_monthly_cor, aes(x = cor_ppt, y = cor_vpd, color = well))+
  theme_light(base_size = 20)+
  labs(x = "PPT-EVI correlation", y = "VPD-EVI correlation", color = "Well location")+
  geom_smooth(size = 0, alpha = 0.2, method = "lm")+
  stat_smooth(geom = "line", method = "lm")+
  geom_point(size = 4)+
  scale_color_manual(values = well_colors)

cor_ppt_avgs <- full_join(combo_monthly_cor, monthly_ppt) %>% 
  select(-n) %>% 
  full_join(monthly_cycles)
  
ggplot(data = combo_monthly_cor, aes(x = well, y = (cor_ppt)))+
  geom_boxplot()
ggplot(data = combo_monthly_cor, aes(x = well, y = (cor_vpd)))+
  geom_boxplot()
ggplot(data = arrange(cor_ppt_avgs, month), aes(x = mean_ppt, y = cor_vpd, label = month))+
  labs(x = "Mean precipitation (mm)", y = "EVI - max VPD correlation")+
  geom_point(alpha = 0.5)+
  theme_light(base_size = 20)+
  geom_text(check_overlap = T, nudge_y = 0.02)+
  geom_path(alpha = 0.3)+
  facet_wrap(~well)+
  geom_smooth(aes(group = well), method = "lm")

rip_cor <- filter(cor_ppt_avgs, well == "Riparian")
up_cor <- filter(cor_ppt_avgs, well == "Upland")

cor.test(up_cor$cor_vpd, up_cor$mean_vpd) # -0.658! (95% CI = -0.839 to -0.347)


anova(lm(cor_vpd ~ mean_ppt, data = cor_ppt_avgs))
# monthly mean precipitation explained 40% of the variability in monthly VPD correlation.
# with random effect of well (riparian v. upland), explains 52% of variability
# vpd did not drive the PPT correlations (10% var explains w/ the mixed model)

# aggregated to monsoon?

# monsoon <- weather_evi %>%
#   filter(szn == "JAS") %>% 
#   mutate(year = year(date)) %>% 
#   group_by(well, name, year) %>% 
#   summarise(evi = mean(evi, na.rm = T),
#             ppt = sum(ppt), vpd_max = max(vpdmax))

winter_data <- filter(weather_evi_monthly, month %in% c(1, 2, 3))
spring_data <- filter(weather_evi_monthly, month %in% c(4, 5, 6))
monsoon_data <- filter(weather_evi_monthly, month %in% c(7, 8, 9))
fall_data <- filter(weather_evi_monthly, month %in% c(10, 11, 12))

# weather %>% 
#   group_by(well) %>% 
#   filter(month(date) %in% c(7, 8, 9)) %>% 
#   summarise(cor(cum_vpd_30d, cum_ppt_30d)) 
# VPD - PPT cor during monsoon ranges -0.638 in uplands, -0.668 in riparian

# weather %>% 
#   group_by(well) %>% 
#   filter(!is.na(cum_ppt_30d)) %>% 
#   summarise(cor(cum_vpd_30d, cum_ppt_30d))
# VPD - PPT cor during entire period ranges 0.076 in uplands, 0.101 in riparian
ggplot(filter(monsoon), aes(x = (vpd_max), y = evi, group = interaction(well)))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", 
        color = "Well location")+
  geom_point(alpha = 0.2, aes(shape = well))+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))

# total monsoon season precipitation relationship looks good......
summary(lm(evi ~ vpd_max, filter(monsoon, well == "Riparian"))) # R2 still low lol



# Final figures (6, 7):

well_colors <- c("slateblue1", "darkgoldenrod3")
month_colors <-  c("red", "purple", "blue3")
names(month_colors) <- factor(c(7, 8, 9))
monsoon_colors <-  c("red", "purple", "blue3")
names(monsoon_colors) <- factor(c(7, 8, 9))

ggplot(filter(monsoon_data), aes(x = (ppt), y = evi, group = well))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(fill = factor(month), shape = well))+
  scale_fill_manual(values = month_colors, aesthetics = "fill", name = "Month")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))

ggplot(filter(fall_data), aes(x = (ppt), y = evi, group = well))+
  theme_light(base_size = 20)+
  ylim(c(0,0.6))+
  labs(x = "Monthly PPT (mm)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(fill = factor(month), shape = well))+
  #scale_fill_manual(values = month_colors, aesthetics = "fill", name = "Month")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  # scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))

ggplot(monsoon_data,
       aes(x = (vpdmax), y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative max VPD (kPa)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(fill = factor(month), shape = well))+
  scale_fill_manual(values = month_colors, aesthetics = "fill", name = "Month")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))

# full year:

ggplot(weather_evi, 
       aes(x = cum_ppt_30d, y = evi, group = well))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative PPT (mm)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.1, aes(shape = well,  fill = as.numeric(month(date))))+
  scale_fill_gradient(low = "red", high = "blue3", aesthetics = "fill")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)
#summary(lm(evi ~ (cum_ppt_30d), filter(weather_evi, well == "Upland")))

ggplot(weather_evi,
       aes(x = cum_vpd_30d, y = evi))+
  theme_light(base_size = 20)+
  labs(x = "30-day cumulative max VPD (kPa)",  y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.1, aes(shape = well,  fill = as.numeric(month(date))))+
  scale_fill_gradient(low = "red", high = "blue3", aesthetics = "fill")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)

# Monthly:
ggplot(weather_evi_monthly, 
       aes(x = (ppt), y = evi_mean, group = well))+
  theme_light(base_size = 26)+
  labs(x = "Monthly PPT (mm)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(shape = well,  fill = month))+
  scale_fill_gradient(low = "red", high = "blue3", aesthetics = "fill")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)

ggplot(monsoon_data, 
       aes(x = (ppt), y = evi, group = well))+
  theme_light(base_size = 26)+
  labs(x = "Monthly PPT (mm)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(shape = well,  fill = factor(month)))+
  scale_fill_manual(values = month_colors, aesthetics = "fill", name = "Month")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))
summary(lm(evi ~ ppt, filter(monsoon_data, well == "Riparian")))
summary(lm(evi ~ ppt, filter(monsoon_data, well == "Upland"))) 
# about half the slope of in the riparian area

ggplot(weather_evi_monthly, 
       aes(x = (vpd_max), y = evi, group = well))+
  theme_light(base_size = 26)+
  labs(x = "Monthly max VPD (kPa)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(shape = well,  fill = month))+
  scale_fill_gradient(low = "red", high = "blue3", aesthetics = "fill")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)

ggplot(monsoon_data, 
       aes(x = (vpd_max), y = evi, group = name))+
  theme_light(base_size = 26)+
  labs(x = "Monthly max VPD (kPa)", y = "EVI", 
       fill = "Month", color = "Well location")+
  geom_point(alpha = 0.2, aes(shape = well,  fill = factor(month)))+
  scale_fill_manual(values = month_colors, aesthetics = "fill", name = "Month")+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", se = F, aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))


# Table 1 and Model Selection:

# PPT models:
summary(lm(evi ~ ppt, data = filter(weather_evi_monthly, well == "Riparian")))
summary(lm(evi ~ ppt, data = filter(weather_evi_monthly, well == "Upland")))
summary(lm(evi ~ ppt, data = filter(monsoon_data, well == "Riparian")))
summary(lm(evi ~ ppt, data = filter(monsoon_data, well == "Upland")))

# VPD models:
summary(lm(evi ~ vpdmax, data = filter(weather_evi_monthly, well == "Riparian")))
summary(lm(evi ~ vpdmax, data = filter(weather_evi_monthly, well == "Upland")))
summary(lm(evi ~ vpdmax, data = filter(monsoon_data, well == "Riparian")))
summary(lm(evi ~ vpdmax, data = filter(monsoon_data, well == "Upland")))

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

# after running 3_EVI.R and 2_Wrangle_Weather.R

well_colors <- c("slateblue1", "darkgoldenrod3")

        # create monthly cycles:
        monthly_cycles <- weather_evi %>% 
          mutate(month = month(date)) %>% 
          group_by(well, month) %>% 
          summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T),
                    mean_vpd = mean(vpdmax, na.rm = T), sd_vpd = sd(vpdmax, na.rm = T),
                    mean_t = mean(tmean, na.rm = T), sd_t = sd(tmean, na.rm = T))
        
        monthly_ppt <- weather_evi %>% 
          mutate(month = month(date), year = year(date)) %>% 
          filter(!is.na(ppt)) %>% 
          arrange(date) %>% 
          group_by(year, month, well, name) %>% 
          summarise(ppt_accum = sum(ppt, na.rm = T)) %>% 
          ungroup() %>% 
          group_by(well, month) %>% 
          summarise(mean_ppt = mean(ppt_accum),
                    sd_ppt = sd(ppt_accum))
        
        ggplot()+
          geom_ribbon(data = monthly_cycles, alpha = 0.3,
                      aes(fill = well, x = factor(month), 
                          y = mean_evi, group = well,
                          ymin = (mean_evi - sd_evi),
                          ymax = (mean_evi + sd_evi)))+
          geom_line(data = monthly_cycles, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
                    linewidth = 2,
                    aes(x = factor(month), y = mean_evi, group = well,
                        color = well))+
          scale_color_manual(values = well_colors)+
          scale_fill_manual(values = well_colors)+
          theme_light(base_size = 30)+
          #facet_wrap(~well)+
          labs(x = "Month", y = "Mean daily EVI", 
               fill = "Well location", color = "Well location")
        
        
        # add shaded bar from X1 to X2
        ggplot()+
          geom_ribbon(data = monthly_cycles, alpha = 0.3,
                      aes(fill = well, x = factor(month), group = well,
                          y = mean_vpd,
                          ymin = mean_vpd - sd_vpd,
                          ymax = mean_vpd + sd_vpd))+
          geom_line(data = monthly_cycles, linetype = 1,
                    linewidth = 2,
                    aes(x = factor(month), y = mean_vpd, color = well, group = well))+
          scale_color_manual(values = well_colors)+
          scale_fill_manual(values = well_colors)+
          theme_light(base_size = 30)+
          #facet_wrap(~well)+
          labs(x = "Month", y = "Mean daily maximum VPD (kPa)", fill = "Well", color = "Well")
        # ggsave("figures/vpd_anncycle.jpg", last_plot(), height = 5, width = 7, units = "in")
        
        ggplot()+
          geom_ribbon(data = monthly_cycles, alpha = 0.3,
                      aes(fill = well, x = factor(month), group = well,
                          y = mean_t,
                          ymin = mean_t - sd_t,
                          ymax = mean_t + sd_t))+
          geom_line(data = monthly_cycles, linetype = 1,
                    linewidth = 2,
                    aes(x = factor(month), y = mean_t, color = well, group = well))+
          scale_color_manual(values = well_colors)+
          scale_fill_manual(values = well_colors)+
          theme_light(base_size = 30)+
          #facet_wrap(~well)+
          labs(x = "Month", y = "Mean daily temperature (ºC)", fill = "Well", color = "Well")
        
        ggplot()+
          geom_errorbar(data = monthly_ppt, alpha = 0.5, position = "dodge",
                      aes(x = factor(month), group = well,
                          ymin = mean_ppt,
                          ymax = mean_ppt + sd_ppt))+
          geom_bar(data = monthly_ppt, linetype = 1, stat = "identity",
                    linewidth = 0.5, position = "dodge",
                    aes(x = factor(month), y = mean_ppt,
                        fill = well, group = well))+
          scale_color_manual(values = well_colors)+
          scale_fill_manual(values = well_colors)+
          theme_light(base_size = 30)+
          #facet_wrap(~well)+
          labs(x = "Month", y = "Mean monthly precipitation (mm)", fill = "Well location", color = "Well")
        
        # ggplot()+
        #   geom_rect(aes(xmin = 182, xmax = 273, ymin = -Inf, ymax = Inf),
        #             fill = "lightgray", alpha = 0.5)+
        #   geom_ribbon(data = evi_ann_daily, alpha = 0.3,
        #               aes(fill = well, x = doy, y = mean_evi,
        #                   ymin = (mean_evi - sd_evi),
        #                   ymax = (mean_evi + sd_evi)))+
        #   geom_line(data = evi_ann_daily, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
        #             linewidth = 0.5,
        #             aes(x = doy, y = mean_evi, color = well))+
        #   scale_color_manual(values = well_colors)+
        #   scale_fill_manual(values = well_colors)+
        #   theme_light(base_size = 30)+
        #   labs(x = "DOY", y = "Mean daily EVI", fill = "Well", color = "Well")
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


      plot(lm(evi ~ (cum_vpd_30d), subset(monsoon_data, well == "Upland")), 1)


# month_colors <-  c("red" = factor(7),
#                  "purple" = factor(8),
#                  "blue3" = factor(9))

summary(lm(evi ~ vpdmax, data = filter(weather_evi_monthly, well == "Upland")))

# plot(lm(evi ~ (cum_vpd_30d), 
#         data = filter(weather_evi, well == "Upland")), 1)

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
