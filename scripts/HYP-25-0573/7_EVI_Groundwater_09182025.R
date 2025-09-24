library(tidyverse)
library(trend)
library(viridis)
# library(spearmanCI)
library(DescTools)
options(scipen = 9999)

# 3_EVI.R:
evi <- read_csv("data/Processed/USP_EVI_Z_Seasonal_Landsat_09182025_Interpolated") %>% 
  mutate(date = date(date))

# # 2_Wrangle_Weather.R:
# weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
#   mutate(well = case_when(well == "alluvial" ~ "Riparian",
#                           well == "regional" ~ "Upland"),
#          date = date(date))

# gw <- read_csv("data/Processed/USP_GW_Zscores_11142024.csv") %>% 
#   mutate(date = date(date))

                    smk_mod <- function(x, ...) {
                      result <- smk.test(x, ...)
                    
                      tibble(
                        p.value = result$p.value,
                        statistic = result$statistic
                      )
                    }
                    
                    mk_mod <- function(x, ...) {
                      result <- mk.test(x, ...)
                      
                      tibble(
                        p.value = result$p.value,
                        statistic = result$statistic
                      )
                    }
                    
                    evi_smk <- evi %>%
                      arrange(date) %>% 
                      group_by(well, name) %>%
                      group_modify(~ smk_mod(ts(.x$evi, frequency = 365)))

# 5_Groundwater.R:
gw <- read_csv("data/Processed/USP_GW_Zscores_Seasonal_01032025.csv") %>%
  mutate(date = date(date)) %>% 
  select(-method)

              gw_mk <- gw %>%
                filter(year(date) >= 2000) %>% 
                arrange(date) %>% 
                group_by(well, name) %>%
                group_modify(~ mk_mod(ts(.x$level)))

              gw_mk %>% 
                filter(p.value < 0.05) %>% 
                group_by(well) %>% 
                summarise(count = n())
              
              gw_mk %>% 
                filter(p.value < 0.05, statistic > 0) %>% 
                group_by(well) %>% 
                summarise(count = n())
              
              gw_mk %>% 
                group_by(well) %>% 
                summarise(count = n())
              
              filter(evi, name == "D-22-20 26DDC [ANTELOPE 2]", year(date) >= 2000) %>% 
                ggplot(aes(x = date, y = evi, color = well))+
                geom_line()+
                geom_smooth(method = "lm")


source("scripts/HYP-25-0573/7a_Wells_In_Same_EVI_Pixel.R")
              
z_scores <- full_join(evi, gw) %>% 
  filter(!is.na(month)) %>% 
  mutate(level_m = level*0.3048) %>% # create a level column in meters
  full_join(clust) %>%
  group_by(well, cluster, date, szn, evi_mean) %>% 
  summarise(evi_z = mean(evi_z), dtg_z = mean(dtg_z))
  
z_bins <- z_scores %>% 
  filter(!is.na(dtg_z), !is.na(evi_z)) %>%
  mutate(z_bin = case_when(dtg_z < -1 ~ "< -1",
                           dtg_z >= -1 & dtg_z < 0 ~ "-1 to 0",
                           dtg_z >= 0 & dtg_z < 1 ~ "0 to 1",
                           dtg_z >=1 ~ "> 1",
                           .default = "none")) %>% 
  mutate(z_dir = case_when(dtg_z <= 0 ~ "Shallower",
                          dtg_z > 0 ~ "Deeper")) %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(4, 5, 6, 7, 8, 9))

z_bins$z_bin <- factor(z_bins$z_bin, levels = c("< -1", "-1 to 0",
                                                "0 to 1", "> 1"))
z_bins$z_dir <- factor(z_bins$z_dir, levels = c("Shallower", "Deeper"))
# z_bins$month <- factor(z_bins$month, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep"))

ggplot(filter(z_bins), aes(x = dtg_z, y = evi_z, group = factor(month),
                           linetype = factor(month), fill = factor(month)))+
  theme_light(base_size = 20)+
  labs(x = "DTG z-score", y = "EVI z-score", color = "Month", linetype = "Month", fill = "Month")+
  geom_point(aes(color = factor(month)), size = 0.3)+
  # geom_bin_2d(alpha = 0.4, bins = 70)+
  
  geom_smooth(data = filter(z_bins, month == 4 & well == "Upland"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 4 & well == "Upland"),
              geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 5 & well == "Upland"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 5 & well == "Upland"),
              geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 6 & well == "Upland"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 6 & well == "Upland"),
              geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 6 & well == "Riparian"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 6 & well == "Riparian"),
              geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 7 & well == "Riparian"),
    method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 7 & well == "Riparian"),
    geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 7 & well == "Upland"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 7 & well == "Upland"),
              geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 8 & well == "Riparian"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 8 & well == "Riparian"),
              geom="line", method = "lm", se = T)+
  
  geom_smooth(data = filter(z_bins, month == 9 & well == "Riparian"),
              method = "lm", alpha=0.3, linewidth=0)+
  stat_smooth(data = filter(z_bins, month == 9 & well == "Riparian"),
              geom="line", method = "lm", se = T)+
  # scale_color_viridis(discrete = T)+
  # scale_fill_viridis(discrete = T)+
  scale_color_brewer(type = "qual", palette = "Dark2")+
  scale_fill_brewer(type = "qual", palette = "Dark2")+
  facet_wrap(~well + szn, scale = "free")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  guides(linetype = guide_legend(override.aes = list(size = 10)))




######## Individual regression model summaries (each combo of month & well loc):
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 4 & well == "Riparian")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 4 & well == "Upland")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 5 & well == "Riparian")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 5 & well == "Upland")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 6 & well == "Riparian")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 6 & well == "Upland")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 7 & well == "Riparian")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 7 & well == "Upland")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 8 & well == "Riparian")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 8 & well == "Upland")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 9 & well == "Riparian")))
summary(lm(evi_z ~ dtg_z, filter(z_bins, month == 9 & well == "Upland")))


ggplot(filter(z_bins), aes(x = z_bin, y = evi_z))+
  geom_boxplot()

z_bins_m <- filter(z_bins, szn == "JAS")

# With just a divide at DTGz = 0:
well_colors <- c("purple", "tan")
ggplot(filter(z_bins_m), aes(x = z_dir, y = evi_z, fill = well))+
  geom_boxplot()+
  scale_fill_manual(values = well_colors)+
  labs(x = "DTG anomaly", y = 'EVI z-score', fill = "Well location")+
  theme_light(base_size = 26)

anom_col <- c("lightblue", "pink3")
ggplot(filter(z_bins, month %in% c(7, 8, 9)), aes(x = well, y = evi_z, fill = z_dir))+
  geom_boxplot()+
  scale_fill_manual(values = anom_col)+
  labs(x = "Well location", y = 'EVI z-score', fill = "DTG anomaly")+
  theme_light(base_size = 26)

tuktest <- aov(evi_z ~ (z_dir*well), data = filter(z_bins, month %in% c(7, 8, 9)))
TukeyHSD(tuktest)


###########################################################
evi_aov <- aov(evi_z ~ z_dir, data = z_bins)
evi_lm <- lm(evi_z ~ dtg_z, data = z_bins)
# evi2_lm <- lm(evi_z ~ dtg_z + I(dtg_z^2), data = z_bins)
anova(evi_lm, evi_aov) #lm fits the data
# F = amoung group MS/within group MS (typical means comparison)
# Regression lack of fit: 
# F = (change in residual SS/change in residual DF)/sep means model variance
# denominator: equals MSE of full model (residual mean square)

TukeyHSD(evi_aov)

# Normal scatterplot:
filter(z_scores, well == "Upland", month(date) %in% c(7)) %>% 
ggplot(aes(x = dtg_z, y = evi_z, color = year(date)))+
  geom_point()+
  geom_smooth(method = "lm")

filter(z_scores, well == "Upland", month(date) %in% c(7)) %>% 
  ggplot(aes(y = evi_z, x = (date)))+
  geom_point()+
  geom_smooth(method = "lm")


summary(lm(evi_z ~ dtg_z, filter(z_scores, well == "Riparian", month %in% c(7, 8, 9))))
# R2 24.3% (25.6%????)







# correlation analysis:
correlations_dtg <- z_scores %>% 
  group_by(well, month) %>% 
  summarise(n = n(),
    cor = (cor.test(evi_z, dtg_z))$estimate,
            ci_lo = (cor.test(evi_z, dtg_z))$conf.int[1],
            ci_up = (cor.test(evi_z, dtg_z))$conf.int[2],
            p = (cor.test(evi_z, dtg_z))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "≤ 0.05",
                         p > 0.05 ~ " > 0.05"))

# spearman_dtg <- z_scores %>% 
#   filter(!is.na(evi_z), !is.na(dtg_z)) %>% 
#   group_by(well, month) %>% 
#   summarise(n = n(),
#             cor = (cor.test(evi_z, dtg_z, method = "spearman", exact = F))$estimate,
#             p = (cor.test(evi_z, dtg_z, method = "spearman", exact = F))$p.value,
#             ) %>% 
#   mutate(sig = case_when(p <= 0.05 ~ "≤ 0.05",
#                          p > 0.05 ~ " > 0.05"))

spearman_dtg <- z_scores %>% 
  filter(!is.na(evi_z), !is.na(dtg_z)) %>% 
  group_by(well, month) %>% 
  summarise(n = n(),
            cor = (SpearmanRho(evi_z, dtg_z, use = "pairwise.complete.obs", conf.level = 0.95))[1],
            ci_lo = (SpearmanRho(evi_z, dtg_z, use = "pairwise.complete.obs", conf.level = 0.95))[2],
            ci_up = (SpearmanRho(evi_z, dtg_z, use = "pairwise.complete.obs", conf.level = 0.95))[3]
  )

# %>% 
#   mutate(sig = case_when(p <= 0.05 ~ "≤ 0.05",
#                          p > 0.05 ~ " > 0.05"))

ggplot(correlations_dtg, aes(x = as.factor(month), y = cor, shape = sig))+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  theme_light(base_size = 20)+
  geom_hline(yintercept = 0, color = "gray60")+
  labs(x = "Month", y = "Correlation coefficient", 
       color = "Well location", shape = "p(r ≠ 0)")
# ggsave("figures/Monthly_EVI_DTG_z_cors.jpg", last_plot(), units = "in", 
#        width = 24, height = 16, dpi = 600)

ggplot(data = filter(z_scores, month(date) == 7), aes(x = evi_z))+
  geom_histogram(aes(fill = as.factor(month)))+
  facet_wrap(~well)

well_colors <- c("purple", "tan")
ggplot(correlations_dtg, aes(x = as.factor(month), y = cor, shape = sig))+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  scale_color_manual(values = well_colors)+
  theme_light(base_size = 30)+
  geom_hline(yintercept = 0, color = "gray60")+
  labs(x = "Month", y = "Correlation coefficient", 
       color = "Well location", shape = "p(r ≠ 0)")
# ggsave("figures/adwr_Monthly_EVI_DTG_z_cors.jpg", last_plot(), units = "in",
#        width = 8, height = 6, dpi = 600)

# actual spearman rank corrs:
ggplot(spearman_dtg, aes(x = as.factor(month), y = cor))+
  #ylim(c(-0.8, 0.8))+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  scale_color_manual(values = well_colors)+
  theme_light(base_size = 30)+
  geom_hline(yintercept = 0, color = "gray60")+
  labs(x = "Month", y = "Spearman's correlation", 
       color = "Well location")
# after running script 6: 
# compare weather correlations with groundwater correlations temporally
# Correlations of Correlations

correlations_dtg$variable <- "dtg_z" # actually evi z - dtg z
correlations_vpd$variable <- "vpd"
correlations_vpd_0p$variable <- "vpd_0p"

cor_combo_wide <- full_join(correlations_dtg, correlations_vpd, by = join_by(well, month))
cor_combo_wide2 <- full_join(correlations_dtg, correlations_vpd_0p, by = join_by(well, month))

ggplot(cor_combo_wide, aes(x = cor.x, y = cor.y, label = as.character(month)))+
  geom_point(aes(color = well))+
  geom_errorbar(aes(ymin = ci_lo.y, ymax = ci_up.y, xmin = ci_lo.x, xmax = ci_up.x),
                width = 0)+
  geom_text(nudge_x = 0.03)+
  labs(color = "Well location", x = "EVI z-score and DTG z-score correlation",
       y = "EVI and VPD30 correlation")

ggplot(cor_combo_wide2, aes(x = cor.x, y = cor.y, label = as.character(month)))+
  geom_point(aes(color = well))+
  geom_errorbar(aes(ymin = ci_lo.y, ymax = ci_up.y),
                width = 0)+
  geom_errorbar(aes(xmin = ci_lo.x, xmax = ci_up.x),
                width = 0)+
  geom_text(nudge_x = 0.03)+
  labs(color = "Well location", x = "EVI z-score and DTG z-score correlation",
       y = "EVI and VPD30 (PPT30 = 0) correlation")


