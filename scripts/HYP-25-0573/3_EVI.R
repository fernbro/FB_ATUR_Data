library(tidyverse)
options(scipen = 99999)
# library(viridis)


# EVI Measurements

# alluvial wells:

wells_by_reach <- read_csv("data/SPRNCA/Wells_Reaches.csv")

# MODIS EVI:
# evi_alluv1 <- read_csv("data/EVI_alluvial_2013_2023.csv")
# evi_alluv2 <- read_csv("data/EVI_alluvwells_2000_2013.csv")

# MODIS from MOD09GA:

evi_mod_a <- read_csv("data/MOD09GA_EVI_alluvial.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 10)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y_%m_%d"),
            evi = evi, name = name, well = "alluvial")

# regional aquifer wells:

# MODIS EVI:
# evi_gen1 <- read_csv("data/EVI_2013_2023_general.csv")
# evi_gen2 <- read_csv("data/EVI_generalwells_2000_2013.csv")

# MODIS from MOD09GA:

evi_mod_r <- read_csv("data/MOD09GA_EVI_regional.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 10)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y_%m_%d"),
            evi = evi, name = name, well = "regional")

evi_mod09 <- rbind(evi_mod_a, evi_mod_r) %>% 
  mutate(szn = case_when(month(date) %in% c(1, 2, 3) ~ "JFM",
                         month(date) %in% c(4, 5, 6) ~ "AMJ",
                         month(date) %in% c(7, 8, 9) ~ "JAS",
                         month(date) %in% c(10, 11, 12) ~ "OND"))

# evi_ann_daily <- evi_mod09 %>% 
#   mutate(doy = yday(date)) %>% 
#   group_by(well, doy) %>% 
#   summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T)) %>% 
#   mutate(well = case_when(well == "alluvial" ~ "Riparian",
#                           well == "regional" ~ "Upland"))

# STATS and Z-SCORES

evi_stats <- evi_mod09 %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>%  # use only complete calendar years
  group_by(well, name, szn) %>% 
  summarise(evi_mean = mean(evi, na.rm = T),
            evi_sd = sd(evi, na.rm = T)) %>% 
  ungroup()

z_model <- full_join(evi_mod09, evi_stats) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         month = month(date))

z_model$method <- "obs relative to seasons - MOD09 daily EVI" # indicating how z scores were calculated
z_model$szn <- factor(z_model$szn, levels = c("JFM", "AMJ", "JAS", "OND")) # set factor levels for seasons
z_model$well <- ifelse(z_model$well == "alluvial", "Riparian", "Upland") # rename well locations

# write_csv(z_model, "data/Processed/USP_EVI_Z_11132024.csv")
# write_csv(z_model, "data/Processed/USP_EVI_Z_Seasonal_01032025.csv")
# write_csv(monthly_z, "data/Processed/USP_EVI_Z_Monthly_12132024.csv")

monthly_cycles <- z_model %>% 
  mutate(month = month(date)) %>% 
  group_by(well, month) %>% 
  summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T))

# Figure 2 for paper:
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


# with a line for each well:

indiv_monthly_cycles <- z_model %>% 
  mutate(month = month(date)) %>% 
  group_by(well, name, month) %>% 
  summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T))

ggplot()+
  geom_ribbon(data = monthly_cycles, alpha = 0.3,
              aes(fill = well, x = factor(month), 
                  y = mean_evi, group = well,
                  ymin = (mean_evi - sd_evi),
                  ymax = (mean_evi + sd_evi)))+
  geom_line(data = indiv_monthly_cycles, linetype = 1, # evi_ann_daily from 3_Wells_EVI.R
            linewidth = 0.5,
            aes(x = factor(month), y = mean_evi, group = name,
                color = well),
            alpha = 0.2)+
  scale_color_manual(values = well_colors)+
  scale_fill_manual(values = well_colors)+
  theme_light(base_size = 30)+
  #facet_wrap(~well)+
  labs(x = "Month", y = "Mean daily EVI", 
       fill = "Well location", color = "Well location")

# Other visuals

ggplot(z_model, aes(x = szn, y = evi_sd/evi_mean, color = well))+
  geom_boxplot()+
  theme_light()+
  labs(x = "Season", y = "EVI CV", color = "Well location")

ggplot(filter(z_model, well == "regional"), 
       aes(x = date, y = evi_z))+
  geom_point()+
  geom_smooth()

ggplot(filter(z_model, well == "Riparian", 
              year(date) >= 2000), 
       aes(x = (date), y = (evi_z)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~month)

