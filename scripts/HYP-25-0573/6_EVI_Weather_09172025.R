library(tidyverse)
library(broom)
# install.packages("ggnewscale")
library(ggnewscale)
library(lme4)
library(MuMIn)
library(DescTools)

source("scripts/HYP-25-0573/1a_PRISM_Grids.R")

weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date)) %>% 
  select(date, name, ppt, vpdmax, tmean)
  
# evi <- read_csv("data/Processed/USP_EVI_Z_Seasonal_01032025.csv") %>% 
#   mutate(date = date(date))
evi <- read_csv("data/Processed/USP_EVI_Z_Seasonal_Landsat_09182025_Interpolated") %>% 
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

# bind pixel ID to weather data (extracted on name basis)
fab_id <- rbind(fab_rip, fab_up)
weather_fab <- full_join(weather, fab_id) %>% 
  select(date, prism, ppt, vpdmax, tmean) %>% 
  unique() %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(prism, month, year) %>% 
  summarise(ppt = sum(ppt),
            vpd_max = max(vpdmax),
            tmean = mean(tmean, na.rm = T))

# get median evi values separately
evi_fab <- evi %>% 
  full_join(fab_id) %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(month, year, well, prism) %>% 
  dplyr::summarise(evi = median(evi))

# combine them
# relationship is many to many since some prism pixels get assigned to multiple well types and all
we_prism <- full_join(weather_fab, evi_fab, 
                      relationship = "many-to-many")

write_csv(we_prism, "data/Processed/ModelInputs_AggregatedtoPRISMPixel_Landsat_09172025.csv")

# prism cell 444746 is an example of one that ends up assigned to wells from both areas


ppt_cor_monthly_full <- we_prism %>% 
  filter(!is.na(evi), !is.na(ppt)) %>% 
  group_by(well, month) %>% 
  dplyr::summarise(n = n(),
            cor = (cor.test(evi, ppt, use = "pairwise.complete.obs", conf.level = 0.95))$estimate,
            ci_lo = (cor.test(evi, ppt, use = "pairwise.complete.obs", conf.level = 0.95))$conf.int[1],
            ci_up = (cor.test(evi, ppt, use = "pairwise.complete.obs", conf.level = 0.95))$conf.int[2]
  ) %>% 
  mutate(var = "ppt")

vpd_cor_monthly_full <- we_prism %>% 
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
  geom_smooth(alpha = 0.2, method = "lm")+
  stat_smooth(geom = "line", method = "lm")+
  geom_point(size = 4)+
  scale_color_manual(values = well_colors)

# cor_ppt_avgs <- full_join(combo_monthly_cor, monthly_ppt) %>% 
#   select(-n) %>% 
#   full_join(monthly_cycles)
  
ggplot(data = combo_monthly_cor, aes(x = well, y = (cor_ppt)))+
  geom_boxplot()
ggplot(data = combo_monthly_cor, aes(x = well, y = (cor_vpd)))+
  geom_boxplot()
# ggplot(data = arrange(cor_ppt_avgs, month), aes(x = mean_ppt, y = cor_vpd, label = month))+
#   labs(x = "Mean precipitation (mm)", y = "EVI - max VPD correlation")+
#   geom_point(alpha = 0.5)+
#   theme_light(base_size = 20)+
#   geom_text(check_overlap = T, nudge_y = 0.02)+
#   geom_path(alpha = 0.3)+
#   facet_wrap(~well)+
#   geom_smooth(aes(group = well), method = "lm")

# aggregated to monsoon?

# monsoon <- weather_evi %>%
#   filter(szn == "JAS") %>% 
#   mutate(year = year(date)) %>% 
#   group_by(well, name, year) %>% 
#   summarise(evi = mean(evi, na.rm = T),
#             ppt = sum(ppt), vpd_max = max(vpdmax))

winter_data <- filter(we_prism, month %in% c(1, 2, 3))
spring_data <- filter(we_prism, month %in% c(4, 5, 6))
monsoon_data <- filter(we_prism, month %in% c(7, 8, 9))
fall_data <- filter(we_prism, month %in% c(10, 11, 12))

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
ggplot(filter(monsoon_data), aes(x = (ppt), y = evi, group = interaction(well)))+
  theme_light(base_size = 20)+
  labs(x = "30-day maximum VPD (kPa)", y = "EVI", 
        color = "Well location")+
  geom_point(alpha = 0.2, aes(shape = well))+
  scale_shape_manual(values = c(21, 22), guide = "none")+
  geom_smooth(method = "lm", aes(color = well), linewidth = 2)+
  scale_color_manual(values = well_colors)+
  guides(fill = guide_legend(override.aes=list(shape = 21, alpha = 1)))

# total monsoon season precipitation relationship looks good......
summary(lm(evi ~ vpd_max, filter(monsoon_data, well == "Riparian"))) # R2 still low lol


# after running 3_EVI.R and 2_Wrangle_Weather.R

well_colors <- c("slateblue1", "darkgoldenrod3")

        # create monthly cycles:
        monthly_cycles <- we_prism %>% 
          group_by(well, month) %>% 
          summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T),
                    mean_vpd = mean(vpd_max, na.rm = T), sd_vpd = sd(vpd_max, na.rm = T),
                    mean_t = mean(tmean, na.rm = T), sd_t = sd(tmean, na.rm = T))
        
        monthly_ppt <- we_prism %>% 
          filter(!is.na(ppt)) %>% 
          group_by(year, month, well, prism) %>% 
          ungroup() %>% 
          group_by(well, month) %>% 
          summarise(mean_ppt = mean(ppt),
                    sd_ppt = sd(ppt))
        
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
