library(tidyverse)
# install.packages("zoo")
library(zoo)
library(lme4)
# install.packages("lmerTest")
library(lmerTest)
library(ggthemes)
library(broom)
options(scipen = 99999)
library(viridis)


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
      
      # spikes and cloud mask???

      
      # For MODIS:
              # evi_ts_alluvial <- rbind(evi_alluv1, 
              #                          evi_alluv2) # bind alluvial evi time series
              #   
              # evi <- evi_ts_alluvial %>%  # alluvial aquifer wells
              #   mutate(date = make_date(year = year, 
              #                           month = month, 
              #                           day = day),
              #          evi = EVI, 
              #          well = "alluvial") %>% 
              #   select(name, evi, date, well)
              # 
              # evi_ts_general <- rbind(evi_gen1, 
              #                         evi_gen2) # bind regional aqu time series
              # 
              # evi2 <- evi_ts_general %>% # regional aquifer wells
              #   mutate(date = make_date(year = year, 
              #                           month = month, 
              #                           day = day),
              #          evi = EVI, 
              #          well = "regional") %>% 
              #   select(name, evi, date, well)
              # 
              # evi_combo <- rbind(evi, evi2) # combine now that they're labeled
              
              evi_mod09 <- rbind(evi_mod_a, evi_mod_r)
              
              evi_ann_daily <- evi_mod09 %>% 
                mutate(doy = yday(date)) %>% 
                group_by(well, doy) %>% 
                summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T)) %>% 
                mutate(well = case_when(well == "alluvial" ~ "Riparian",
                                        well == "regional" ~ "Upland"))


#plot of evi, time series for each well (colored by its aquifer type)
# ggplot(filter(evi_combo),
#        aes(x = date,
#            y = evi,
#            group = well,
#            color = well))+
#   geom_smooth()

# let's do some smoothing
# WANT TO IMPROVE LATER (for data spikes)

# evi_smooth <- evi_combo %>% 
#   group_by(name) %>% 
#   arrange(name, date) %>% 
#   mutate(evi_30 = zoo::rollmean(evi, 31, c(NA, NA, NA))) %>% 
#   ungroup()

# according to Nagler et al. 2013, each 0.01 increase in EVI in a riparian area
# corresponds to an increased ET of 0.16 mm/day (58.4 mm/yr)
# (see NaglerET product)

# Groundwater Measurements from uppersanpedrowhip.org/map

      # Alluvial:

alluv_levels <- read_csv("data/Near-stream_alluvial-aquifer_water_levels.csv",
                         skip = 2, col_names = T)

alluvial <- alluv_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, 
                              tryFormats = c("%m/%d/%Y")),
            name = station_nm, 
            lat = y, 
            lon = x, 
            level = lev_va) %>%
  mutate(well = "alluvial")

# filter(alluvial) %>%
#   ggplot(aes(x = date, y = -level, group = name))+
#   geom_line()

      # Rest of the wells:

regional_levels <- read_csv("data/Groundwater_levels_below_land_surface.csv")

regional <- regional_levels %>% 
  transmute(date = as.POSIXct(lev_timestamp, tryFormats = c("%m/%d/%y")),
            name = station_nm, lat = y, lon = x, level = lev_va) %>% 
  mutate(well = "regional") %>% 
  filter(year(date) >= 2000, year(date) <= 2024)

      # combine groundwater measurements

water_combo <- rbind(regional, alluvial) %>% 
  select(date, name, well, level)

water_stats <- water_combo %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>% 
  filter(!is.na(level)) %>% 
  group_by(name) %>% 
  summarise(dtg_mean = mean(level, na.rm = T),
            dtg_sd = sd(level, na.rm = T),
            dtg_cv = dtg_sd / dtg_mean) %>% 
  ungroup()


# water_combo: date, name, well, level
# evi_combo (evi_mod09): name, evi, date, well

evi_stats <- evi_mod09 %>% 
  filter(year(date) <= 2024 & year(date) >= 2000) %>% 
  filter(!is.na(evi)) %>% 
  group_by(name) %>% 
  summarise(evi_mean = mean(evi, na.rm = T),
            evi_sd = sd(evi, na.rm = T),
            evi_cv = evi_sd / evi_mean) %>% 
  ungroup()
# !!!!!!!!!!!!!!!!!!!!!!!! :

# each day and site should be paired with weather and stream permanence info
# after calculating z scores (then can do more analyses)
# compute EVI and DTG values over the same time period from their own periods of record

combined_evi <- full_join(water_combo, evi_mod09, 
                          join_by(name, date, well)) %>% 
  filter(year(date) <= 2024 & year(date) >= 2000)

# compute average EVI and groundwater levels
stats <- full_join(evi_stats, water_stats)
  
  
  # combined_evi %>%
  # filter(!is.na(evi), !is.na(level)) %>%
  # group_by(name) %>% 
  # summarise(evi_mean = mean(evi, na.rm = T), 
  #           evi_sd = sd(evi, na.rm = T),
  #           dtg_mean = mean(level, na.rm = T), 
  #           dtg_sd = sd(level, na.rm = T),
  #           evi_cv = evi_sd / evi_mean,
  #           dtg_cv = dtg_sd / dtg_mean
  # ) %>% 
  # ungroup()

# stats_by_month <- combined_evi %>% 
#   filter(!is.na(evi), !is.na(level)) %>% 
#   mutate(month = month(date)) %>% 
#   group_by(name, month) %>% 
#   summarise(evi_mean = mean(evi, na.rm = T), 
#             evi_sd = sd(evi, na.rm = T),
#             dtg_mean = mean(level, na.rm = T), 
#             dtg_sd = sd(level, na.rm = T),
#             evi_cv = evi_sd / evi_mean,
#             dtg_cv = dtg_sd / dtg_mean
#   ) %>% 
#   ungroup()

z_scores <- full_join(combined_evi, stats) %>% 
  filter(!is.na(evi), !is.na(level)) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd,
         month = month(date)) %>% 
  full_join(wells_by_reach)

# z_scores_monthly <- combined_evi %>%  # Z-scores data frame with mean & sd for any given Month
#   mutate(month = month(date)) %>% 
#   full_join(stats_by_month) %>% 
#   filter(!is.na(evi), !is.na(level),
#          month %in% c(4, 5, 6, 7, 8, 9)) %>% 
#   group_by(name, month) %>% 
#   mutate(evi_z = (evi - evi_mean)/evi_sd,
#          dtg_z = (level - dtg_mean)/dtg_sd) %>% 
#   ungroup()

# let's get a bunch of explanatory variables going
z_model <- filter(z_scores) %>% 
  full_join(wells_by_reach) %>% 
  mutate(year = year(date)) %>%
  # mutate(water_year = case_when(month(date) >= 11 ~ year(date) + 1,
  #                               .default = year(date))) %>% 
  full_join(weather_wy) %>% 
  filter(!is.na(evi_z), !is.na(dtg_z))
  #full_join(alluv_pairings %>% rename(name = well_name), join_by(name)) %>% 
  # full_join(stream_perm_raw %>% 
  #             rename(perm_name = site) %>% 
  #             select(year, perm_name, percent_flowing), join_by(year, perm_name))


z_model$method <- "obs relative to whole year - MOD09 daily EVI"

#write_csv(z_model, "data/Processed/USP_GW_EVI_Z_FullYear_10312024.csv")

wells_by_reach <- read_csv("data/SPRNCA/Wells_Reaches.csv")
# make Z_SCORES plot

# Regression lines: colored by MONTHS (growing season months)
# colors <- c("firebrick3", "darkorange1", "goldenrod1",
#             "darkgreen", "blue", "purple")
# colors <- c("navyblue", "cadetblue", "forestgreen", 
#             "yellow4", "cornsilk4", "peachpuff4")
# cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
#           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# colors <- c("royalblue1", "mediumpurple1", "tomato")

# colors <- c("navyblue", "dodgerblue3", "seagreen",
#             "", "", "")

colors <- c("brown4", "sienna2", "goldenrod1",
            "darkgreen", "royalblue", "mediumorchid3")

names(colors) <- as.factor(seq(4, 9, by = 1))
ggplot(filter(z_model, !is.na(month)),
       aes(x = dtg_z, y = evi_z, color = as.factor(month))
       )+
  geom_point(aes())+
  geom_smooth(method = "lm", se = F)+
  scale_color_manual(values = colors,
                     labels = c("April", "May", "June",
                       "July", "August", "September"))+
  labs(x = "DTG z-score", y = "EVI z-score", color = "Month",
       title = "EVI Z vs. DTG Z for riparian sites")+
  ylim(c(-2, 5))+
  theme_light(base_size = 16)
  # theme(panel.background = element_rect(fill = "grey89",
  #                                 colour = "grey89",
  #                                 size = 0.5, linetype = "solid"),
  #       panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
  #                                       colour = "white"), 
  #       panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
  #                                       colour = "white"))
# ggsave("figures/AMJvsYear_evi_months.jpg", width = 8, height = 4, units = "in")
# now box plots for each month, evi_z and dtg_z?

monthly_regr <- z_model %>% 
  mutate(month = as.factor(month(date))) %>% 
  filter(!is.na(month)) %>% 
  group_by(month) %>% 
  reframe(tidy(lm(evi_z ~ dtg_z))) %>% 
  ungroup()

ggplot(filter(z_scores, reach != 0, well == "alluvial"), 
       aes(y = level, 
           #x = as.factor(month(date)),
           x = as.factor(year(date))))+
  geom_boxplot()+
  #geom_boxplot(aes(y = evi_z), alpha = 0.2)+
  theme_light()+
  # scale_fill_manual(values = colors,
  #                    labels = c("April", "May", "June",
  #                      "July", "August", "September"))+
  labs(fill = "Month")+
  xlab("Month")+
  ylab("Depth to groundwater (ft)")+
  facet_wrap(~reach, ncol = 3)


ggplot(filter(z_scores, well == "alluvial"), 
       aes(y = level, x = as.factor(month(date)),
           fill = as.factor(month(date))))+
  geom_boxplot()+
  #geom_boxplot(aes(y = evi_z), alpha = 0.2)+
  theme_light(base_size = 26)+
  scale_fill_manual(values = colors,
                    labels = c("April", "May", "June",
                               "July", "August", "September"))+
  labs(fill = "Month")+
  xlab("Month")+
  ylab("Depth to groundwater (ft)")+
  theme(legend.position = "none")
#ggsave("figures/upland_DTGz_box.jpg", width = 6, height = 4, units = "in")

ggplot(filter(z_scores, well == "alluvial"), 
       aes(y = evi, x = as.factor(month(date)),
           fill = as.factor(month(date))))+
  geom_boxplot()+
  theme_light(base_size = 26)+
  scale_fill_manual(values = colors,
                    labels = c("April", "May", "June",
                               "July", "August", "September"))+
  labs(fill = "Month")+
  xlab("Month")+
  ylab("EVI")+
  ylim(c(-0.6, 0.6))+
  theme(legend.position = "none")
#ggsave("figures/upland_EVIz_box.jpg", width = 6, height = 4, units = "in")

# import vegetation data: (from script #2)
# 
# vegetation <- rbind(alluvial_landfire, regional_landfire) # alluvial + reg
# 
# z_scores <- full_join(z_scores, vegetation) %>% 
#   mutate(year = year(date)) %>% 
#   full_join(usp_spi_monsoon, join_by(year)) %>% 
#   mutate(drought_year = case_when(drought >= 50 ~ "yes", .default = "no"))

# ggplot(filter(z_scores, well == "alluvial"),
#        aes(x = date, y = evi, group = name))+
#   geom_line()+
#   theme_light()

# grepl("Wash", lc2016) 
#| grepl("Riparian", lc2016)
#| grepl("Ruderal", lc2016),

colors <- c("royalblue1", "mediumpurple1", "tomato")
names(colors) <- c(as.factor(4), as.factor(5), as.factor(6))

ggplot(filter(z_model,
              month %in% c(4, 5, 6)), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = as.factor(month(date))))+
  scale_color_manual(values = colors,
                     labels = c(
                       "April", "May", "June"))+
  geom_smooth(method = "lm", se = F
              )+
  theme_light(base_size = 26)+
  #ylim(c(-5,5))+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  labs(color = "Month")+
  ggtitle("AMJ EVI vs. DTG z-scores")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
#ggsave("figures/JASvsYear_evi_months.jpg", width = 8, height = 4, units = "in")

summary(lm(evi_z ~ dtg_z, filter(z_scores, month(date) %in% c(4, 5, 6), well == 'alluvial')))


ggplot(filter(z_model, 
              month(date) %in% c(4, 5, 6),
              well == "alluvial"), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes())+
  # scale_color_manual(values = colors,
  #                    labels = c(
  #                      "April", "May", "June"))+
  geom_smooth(method = "lm", se = F)+
  theme_light(base_size = 16)+
  #ylim(c(-5,5))+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  #labs(color = "Percent flowing")+
  ggtitle("AMJ EVI vs. DTG z-scores")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
# summary(lm(evi_z ~ dtg_z, data = z_model))

regr_amj <- z_model %>% 
  filter(month(date) %in% c(4, 5, 6)) %>% 
  group_by(name) %>% 
  reframe(tidy(lm(evi_z ~ dtg_z))) %>% 
  ungroup() %>% 
  mutate(term = case_when(term == "(Intercept)" ~ "intercept",
                          term == "dtg_z" ~ "slope")) %>% 
  filter(p.value < 0.05, term == "slope")
# all significant slopes are positive!
# relate flow permanence to these different slopes

regr <- z_model %>%
  filter(month %in% c(7, 8, 9), well == "alluvial") %>% 
  group_by(name) %>%
  reframe(tidy(lm(evi_z ~ dtg_z))) %>%
  ungroup() %>%
  mutate(term = case_when(term == "(Intercept)" ~ "intercept",
                          term == "dtg_z" ~ "slope")) %>% 
  full_join(stats, relationship = "many-to-many") %>% 
  filter(term == "slope") %>% 
  filter(p.value <= 0.05)

ggplot(data = regr, aes(x = dtg_mean, y = estimate))+
  geom_point()+
  theme_light()+
  labs(x = "Site mean depth to groundwater (ft)", y = "Site EVI Z - DTG Z slope")

# # significant with alpha = 0.05:
# 
# regr_05 <- regr %>% 
#   filter(p.value < 0.05, term == "slope")

# site lm slopes:

# site_sig_slopes <- regr_05 %>% 
#   select(name, estimate)

# Spearman's rank correlations:

# Correlation in groups by reach:
spearman_reach <- z_model %>% 
  filter(month(date) %in% c(7, 8, 9)) %>% 
  filter(!is.na(wy_ppt), !is.na(evi_z), !is.na(dtg_z)) %>% 
  group_by(reach) %>% 
  summarise(corr = cor(evi_z, dtg_z, method = "spearman"),
            ppt = mean(wy_ppt), dtg_mean = mean(dtg_mean),
            sd_wp = sd(wy_ppt)) %>% 
  mutate(cv_ppt = sd_wp/ppt, wy_ppt = ppt)

# Correlation in groups by well/point (name):
spearman_name <- z_model %>% 
  filter(month(date) %in% c(4, 5, 6)) %>% 
  filter(!is.na(wy_ppt), !is.na(evi_z), !is.na(dtg_z)) %>% 
  group_by(name) %>% 
  summarise(corr = cor(evi_z, dtg_z, method = "spearman"),
            ppt = mean(wy_ppt), dtg_mean = mean(dtg_mean),
            sd_wp = sd(wy_ppt)) %>% 
  mutate(cv_ppt = sd_wp/ppt, wy_ppt = ppt) %>% 
  full_join(site_sig_slopes)

# Correlation across sites and time within a season, as grouped by year 
spearman_year <- z_model %>% 
  filter(month(date) %in% c(4, 5, 6)) %>% 
  filter(!is.na(wy_ppt), !is.na(evi_z), !is.na(dtg_z)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(corr = cor(evi_z, dtg_z, method = "spearman"),
            ppt = mean(wy_ppt), dtg_mean = mean(dtg_mean),
            sd_wp = sd(wy_ppt)) %>% 
  mutate(cv_ppt = sd_wp/ppt, wy_ppt = ppt)

# Distribution of correlation over time at a point: 
ggplot(spearman_name, aes(x = corr))+
  geom_histogram(color = "black", fill = "gray")+
  theme_hc(base_size = 26)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x = "Spearman's rank correlation", y = "Count", 
       title = "EVI Z - DTG Z correlation by site")+
  theme(legend.position = "left")

# How does correlation over time at a point relate to 
# average pre-monsoon precipitation?
ggplot(filter(spearman_name), aes(x = sd_wp, y = estimate))+
  geom_point()+
  theme_hc(base_size = 26)+
  labs(y = "EVI Z - DTG Z slope estimate", x = "SD of pre-monsoon precipitation (mm)")+
  theme(legend.position = "left")

# How does correlation over time at a point relate to 
# average groundwater depth at the site?
ggplot(filter(spearman_name), aes(x = dtg_mean, y = corr))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_hc(base_size = 26)+
  labs(y = "Spearman's rank correlation", x = "Mean DTG (ft)", 
       title = "EVI Z - DTG Z correlation by site")+
  theme(legend.position = "left")
summary(lm(corr ~ dtg_mean, spearman_name))

# Distribution of correlation across space/msmts within a year
ggplot(spearman_year, aes(x = corr))+
  geom_histogram(fill = "gray", color = "black")+
  theme_hc(base_size = 26)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x = "Spearman's rank correlation", y = "Count",
       title = "EVI Z - DTG Z correlation by year")+
  theme(legend.position = "left")

# Correlation across space/msmts within a year as determined by 
# pre monsoon precip averaged across sites
ggplot(spearman_year, aes(x = ppt, y = corr, color = year))+
  geom_point(size = 3)+
  theme_hc()+
  labs(y = "Spearman rank correlation", x = "Site averaged WY PPT", 
       title = "EVI Z - DTG Z correlation by year")+
  theme(legend.position = "left")
###### Linear Modeling


mixed_name <- (lmer(evi_z ~ dtg_z + (dtg_z | name), data = filter(z_model)))
mixed_reach <- lmer(evi_z ~ (dtg_z | reach), data = filter(z_model, year(date) <= 2013))
mixed_year <- (lmer(evi_z ~ dtg_z + (dtg_z | year), data = filter(z_model))) # higher temporal variability bc this model explains best?
linear <- lm(evi_z ~ dtg_z, data = z_model)
summary(mixed_name)
summary(mixed_year) # by-year model had a lower AIC
summary(linear)

AIC(mod, mod1, mod2)
anova(mixed_name, mixed_year, mixed_reach)
ranova(mixed_year)
ranef(mixed_year) # mixed effects model cofficients
ranova(mixed_name)
ranef(mixed_name)
  
plot(fitted(mixed_name), z_model$evi_z)
abline(a = 0, b = 1, col = "red")

ggplot(filter(z_model), aes(x = dtg_z, y = evi_z))+
  geom_point(aes(color = as.integer(year(date))))+
  geom_line(aes(y = predict(mixed_year),
                group = year(date),
                color = year(date)))+
  labs(x = "DTG z-score", y = "EVI z-score", title = "Year as random effect",
       color = "Year")+
  theme_light(base_size = 26)
# +
#   theme(legend.position = "none")

ggplot(filter(z_model), aes(x = dtg_z, y = evi_z))+
  geom_point(aes(color = name))+
  geom_line(aes(y = predict(mixed_name),
                group = name, 
                color = name))+
  labs(x = "DTG z-score", y = "EVI z-score", title = "Site as random effect")+
  theme_light(base_size = 26)+
  theme(legend.position = "none")

# TIME SERIES of z-scores
          
          ggplot(filter(z_scores, 
                        well == "alluvial"), 
                 aes(x = date,
                     y = dtg_z))+  
            geom_smooth(method = "gam")+
            geom_point(aes(color = name))+
            theme_light()+
            theme(legend.position = "none")
          
          ggplot(filter(z_scores, 
                        well == "alluvial",
                        year(date) == 2013), 
                 aes(x = date,
                     y = dtg_z))+  
            geom_smooth(method = "lm")+
            geom_point(aes(color = name))+
            theme_light()+
            theme(legend.position = "none")
          
          monthly_z <- z_scores %>% 
            mutate(month = month(date)) %>% 
            group_by(month, name) %>% 
            summarise(dtg_z = mean(dtg_z, na.rm = T))
          
          ggplot(monthly_z, 
                 aes(x = as.factor(month),
                     y = dtg_z))+
            geom_point(aes(color = name))+
            theme_light()+
            theme(legend.position = "none")

# HISTOGRAMS

# #AMJ:
# colors <- c("royalblue1", "mediumpurple1", "tomato")
# names(colors) <- c(as.factor(4), as.factor(5), as.factor(6))
# 
# #JAS:
# colors <- c("tomato", "mediumpurple1", "royalblue1")
# names(colors) <- c(as.factor(7), as.factor(8), as.factor(9))


ggplot(filter(z_scores), 
       aes(x = evi_z, fill = as.factor(month(date))))+
  # geom_histogram(binwidth = 0.2,
  #                color = "darkgrey",
  #                aes(fill = as.factor(month(date))))+
  geom_density(alpha = 0.3)+
  scale_fill_manual(values = colors,
                     labels = c(
                                "July", "August", "September"))+
  xlab("EVI z-score")+
  ylab("Count")+
  # geom_vline(xintercept = mean((filter(z_scores, 
  #                                      well == "alluvial")$dtg_z), 
  #                              na.rm = T))+
  theme_light()+
  labs(fill = "Month")+
  facet_wrap(~well)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
# ggsave("figures/amjjas_eviz_density.jpg", last_plot(), width = 10, height = 5, units = "in")

# summary(lm(evi_z ~ dtg_z, filter(z_scores_nca,
#                                  sprnca == "SUM",
#                                   well == "alluvial"))) # p < 0.001

######## EVI TIME SERIES:

evi_annual <- evi_mod09 %>% 
  mutate(year = year(date), month = month(date)) %>%
  filter(year < 2025, month %in% c(7, 8, 9)) %>% 
  group_by(well, year) %>% 
  summarise(evi = mean(evi, na.rm = T))

evi_ann_daily <- evi_mod09 %>% 
  mutate(doy = yday(date)) %>% 
  group_by(well, doy) %>% 
  summarise(n = n(), mean_evi = mean(evi, na.rm = T), sd_evi = sd(evi, na.rm = T)) %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

ggplot(evi_ann_daily, aes(x = doy, y = mean_evi, color = well))+
  geom_line()+
  theme_light()+
  labs(x = "DOY", y = "Mean EVI")

# ggplot(evi_annual, aes(x = evi, fill = well))+
#   geom_density(alpha = 0.3)+
#   theme_hc(base_size = 26)+
#   labs(x = "MODIS EVI", y = "Density", fill = "Well location")+
#   scale_fill_manual(values = c("green", "brown"), 
#                     labels = c("Riparian", "Upland"))

evi_means <- evi_mod09 %>% 
  mutate(year = year(date), month = month(date)) %>%
  filter(year < 2025, month %in% c(4, 5, 6)) %>% 
  group_by(well) %>% 
  summarise(evi = mean(evi, na.rm = T))

ggplot(filter(evi_annual), 
       aes(x = year, 
           y = evi))+
  xlab("Year")+
  ylab("Average EVI")+
  theme(legend.position = "none")+
 theme_light(base_size = 20)+
  facet_wrap(~well, ncol = 2)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
 # scale_colour_gradient(low = "deepskyblue", high="red")+
  geom_smooth(method = "lm", se = F, color = "lemonchiffon4")+
  geom_hline(data = evi_means, aes(yintercept = evi), linetype = 2, color = "cornsilk4")+
  geom_line()+
  geom_point(size = 3)+
  scale_x_continuous(breaks = seq(2000, 2024, by = 6))
ggsave("figures/JAS_EVI_avgs.jpg", width = 7, height = 3, units = "in")

summary(lm(evi ~ year, filter(evi_annual, well == "alluvial")))

########## GW TIME SERIES:

gw_monthly <- water_combo %>% 
  mutate(month = month(date)) %>% 
  group_by(well, name, month) %>% 
  summarise(dtg = mean(level, na.rm = T))
  
ggplot(gw_monthly, aes(x = as.factor(month), y = dtg, color = well))+
  geom_boxplot()+
  # geom_line(aes(group = name))+
  facet_wrap(~well, scales = "free")+
  xlab("Month")+
  ylab("Mean depth to groundwater")+
  theme_light()+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
#ggsave("figures/monthly_dtg_boxplots.jpg", last_plot(), width = 10, height = 5, units = "in")
  
gw_annual <- water_combo %>% 
  mutate(year = year(date)) %>% 
  group_by(well, year) %>% 
  summarise(dtg = mean(level, na.rm = T))

ggplot(filter(water_combo, year(date) %in% seq(2000, 2023)) %>% 
         mutate(year = year(date)), 
       aes(x = as.factor(year), y = level))+
  geom_boxplot()+
  facet_wrap(~well, scales = "free", ncol = 1)+
  xlab("Year")+
  ylab("Mean depth to groundwater")+
  theme_light()+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
#ggsave("figures/annual_dtg_boxplots.jpg", last_plot(), width = 12, height = 5, units = "in")


# Number of wells observing in each year:

wells_annual <- water_combo %>% 
  mutate(year = year(date)) %>% 
  select(year, name, well) %>% 
  distinct() %>% 
  group_by(year, well) %>% 
  summarise(number = n())

# histograms:

# ggplot(filter(combined_evi, well == "alluvial"), aes(x = level))+
#   geom_histogram(aes(fill = name), binwidth = 0.1)+
#   theme(legend.position = "none")
# 
# full_evi_veg <- full_join(combined_evi, vegetation)
# 
# anova1 <- aov(level ~ name, data = combined_evi)
# 
# ggplot(filter(z_scores, well == "alluvial"), aes(x = -level))+
#   coord_flip()+
#   geom_histogram(fill = "skyblue2", binwidth = 1)+
#   geom_vline(xintercept = -9.19, color = "darkorange", size = 1)+
#   geom_vline(xintercept = -4.92, color = "forestgreen", size = 1)+
#   geom_vline(xintercept = -11.81, color = "maroon", size = 1)+
#   xlab("Groundwater level (ft)")+
#   ylab("Frequency")+
#   theme_light()

# ggsave("figures/dtg_hist.jpg", width = 6, height = 3, units = "in")

# ggplot(filter(z_scores, well == "alluvial"), aes(x = dtg_z))+
#   geom_histogram(fill = "skyblue2", binwidth = 0.2)+
#   theme(legend.position = "none")+
#   xlab("Depth to groundwater z-score")+
#   ylab("Frequency")
# 
# ggplot(filter(z_scores, well == "alluvial"), aes(x = date, y = level))+
#   geom_line(aes(group = name))+
#   geom_point(aes(color = name))+
#   facet_wrap(~lc2016, scales = "free_x")+
#   theme(legend.position = "none")
