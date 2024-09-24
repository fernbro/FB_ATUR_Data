library(tidyverse)
# install.packages("zoo")
library(zoo)
library(lme4)


# EVI Measurements

  # alluvial wells:

      evi_alluv1 <- read_csv("data/EVI_alluvial_2013_2023.csv")
      evi_alluv2 <- read_csv("data/EVI_alluvwells_2000_2013.csv")
      
  # regional aquifer wells:
      evi_gen1 <- read_csv("data/EVI_2013_2023_general.csv")
      evi_gen2 <- read_csv("data/EVI_generalwells_2000_2013.csv")
      
      # spikes and cloud mask???

evi_ts_alluvial <- rbind(evi_alluv1, 
                         evi_alluv2) # bind alluvial evi time series
  
evi <- evi_ts_alluvial %>%  # alluvial aquifer wells
  mutate(date = make_date(year = year, 
                          month = month, 
                          day = day),
         evi = EVI, 
         well = "alluvial") %>% 
  select(name, evi, date, well)

evi_ts_general <- rbind(evi_gen1, 
                        evi_gen2) # bind regional aqu time series

evi2 <- evi_ts_general %>% # regional aquifer wells
  mutate(date = make_date(year = year, 
                          month = month, 
                          day = day),
         evi = EVI, 
         well = "regional") %>% 
  select(name, evi, date, well)

evi_combo <- rbind(evi, evi2) # combine now that they're labeled

#plot of evi, time series for each well (colored by its aquifer type)
# ggplot(filter(evi_combo),
#        aes(x = date,
#            y = evi,
#            group = well,
#            color = well))+
#   geom_smooth()

# let's do some smoothing
# WANT TO IMPROVE LATER (for data spikes)

evi_smooth <- evi_combo %>% 
  group_by(name) %>% 
  arrange(name, date) %>% 
  mutate(evi_30 = zoo::rollmean(evi, 31, c(NA, NA, NA))) %>% 
  ungroup()

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

      # plot gw status by wells

# ggplot(water_combo, aes(x = date, y = level, 
#                         group = name, color = well))+
#   geom_line()

      # alluvial is much deeper

      # attach EVI

# water_combo: date, name, well, level
# evi_combo: name, evi, date, well

combined_evi <- full_join(water_combo, evi_smooth, 
                          join_by(name, date, well))

# compute average EVI and groundwater levels
stats <- combined_evi %>%
  filter(!is.na(evi_30), !is.na(level)) %>%
  group_by(name) %>% 
  summarise(evi_mean = mean(evi, na.rm = T), 
            evi_sd = sd(evi, na.rm = T),
            dtg_mean = mean(level, na.rm = T), 
            dtg_sd = sd(level, na.rm = T),
            evi_cv = evi_sd / evi_mean,
            dtg_cv = dtg_sd / dtg_mean
  ) %>% 
  ungroup()

stats_by_month <- combined_evi %>% 
  filter(!is.na(evi_30), !is.na(level)) %>% 
  mutate(month = month(date)) %>% 
  group_by(name, month) %>% 
  summarise(evi_mean = mean(evi, na.rm = T), 
            evi_sd = sd(evi, na.rm = T),
            dtg_mean = mean(level, na.rm = T), 
            dtg_sd = sd(level, na.rm = T),
            evi_cv = evi_sd / evi_mean,
            dtg_cv = dtg_sd / dtg_mean
  ) %>% 
  ungroup()

z_scores <- full_join(combined_evi, stats) %>% 
  filter(!is.na(evi_30), !is.na(level),
         month(date) %in% c(4, 5, 6, 7, 8, 9)
         ) %>% 
  mutate(evi_z = (evi_30 - evi_mean)/evi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd,
         month = month(date))

z_scores_monthly <- combined_evi %>%  # Z-scores data frame with mean & sd for any given Month
  mutate(month = month(date)) %>% 
  full_join(stats_by_month) %>% 
  filter(!is.na(evi_30), !is.na(level),
         month %in% c(4, 5, 6, 7, 8, 9)) %>% 
  group_by(name, month) %>% 
  mutate(evi_z = (evi_30 - evi_mean)/evi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd) %>% 
  ungroup()


# Regression lines: colored by MONTHS (growing season months)
colors <- c("firebrick3", "darkorange1", "goldenrod1", 
            "darkgreen", "blue", "purple")
names(colors) <- as.factor(seq(4, 9, by = 1))
ggplot(filter(z_scores, well == "alluvial"),
       aes(x = dtg_z, y = evi_z, 
           color = as.factor(month(date)),
           group = month(date))
       )+
  geom_point()+
  geom_smooth(method = "lm", se = T)+
  ylim(c(-5, 5))+
  scale_color_manual(values = colors,
                     labels = c("April", "May", "June",
                                "July", "August", "September"))+
  theme_light()

# now box plots for each month, evi_z and dtg_z?

ggplot(filter(z_scores, dtg_z < 5, well == "regional"), 
       aes(y = dtg_z, x = as.factor(month(date)),
           fill = as.factor(month(date))))+
  geom_boxplot()+
  #geom_boxplot(aes(y = evi_z), alpha = 0.2)+
  theme_light()+
  scale_fill_manual(values = colors,
                     labels = c("April", "May", "June",
                                "July", "August", "September"))+
  labs(fill = "Month")+
  xlab("Month")+
  ylab("DTG z-score")
#ggsave("figures/upland_DTGz_box.jpg", width = 6, height = 4, units = "in")

ggplot(filter(z_scores, dtg_z < 5, well == "regional"), 
       aes(y = evi_z, x = as.factor(month(date)),
           fill = as.factor(month(date))))+
  geom_boxplot()+
  theme_light()+
  scale_fill_manual(values = colors,
                    labels = c("April", "May", "June",
                               "July", "August", "September"))+
  labs(fill = "Month")+
  xlab("Month")+
  ylab("EVI z-score")
#ggsave("figures/upland_EVIz_box.jpg", width = 6, height = 4, units = "in")

# import vegetation data: (from script #2)

vegetation <- rbind(alluvial_landfire, regional_landfire) # alluvial + reg

z_scores <- full_join(z_scores, vegetation) %>% 
  mutate(year = year(date)) %>% 
  full_join(usp_spi_monsoon, join_by(year)) %>% 
  mutate(drought_year = case_when(drought >= 50 ~ "yes", .default = "no"))

# ggplot(filter(z_scores, well == "alluvial"),
#        aes(x = date, y = evi, group = name))+
#   geom_line()+
#   theme_light()

# grepl("Wash", lc2016) 
#| grepl("Riparian", lc2016)
#| grepl("Ruderal", lc2016),

# colors <- c("royalblue1", "mediumpurple1", "tomato")
# names(colors) <- c(as.factor(9), as.factor(8), as.factor(7))

ggplot(filter(z_scores, 
              well == "alluvial"), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = as.factor(month(date))))+
  scale_color_manual(values = colors,
                     labels = c("April", "May", "June",
                       "July", "August", "September"))+
  geom_smooth(method = "lm", se = F)+
  theme_light()+
  ylim(c(-5,5))+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  labs(color = "Month")+
  ggtitle("AMJJAS EVI vs. DTG z-scores relative to each month")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
# ggsave("figures/AMJJASvsYear_evi30_months.jpg", width = 8, height = 4, units = "in")

summary(lm(evi_z ~ dtg_z, filter(z_scores, 
                                 well == "alluvial")))

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
                     labels = c("April", "May", "June",
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

z_scores$method <- "JAS obs relative to whole year - 30 d EVI"

#write_csv(z_scores, "data/Processed/USP_GW_EVI_Z.csv")

######## EVI TIME SERIES:
evi_annual <- evi_combo %>% 
  mutate(year = year(date), month = month(date)) %>%
  filter(year != 2000 & year != 2023, month %in% c(4, 5, 6)) %>% 
  group_by(well, year) %>% 
  summarise(evi = mean(evi, na.rm = T)) %>% 
  inner_join(usp_spi_annual, join_by(year))

evi_means <- evi_combo %>% 
  mutate(year = year(date), month = month(date)) %>%
  filter(year != 2000 & year != 2023, month %in% c(4, 5, 6)) %>% 
  group_by(well) %>% 
  summarise(evi = mean(evi, na.rm = T))

ggplot(filter(evi_annual), 
       aes(x = year, 
           y = evi))+
  xlab("Year")+
  ylab("Average EVI")+
  theme(legend.position = "none")+
  theme_light()+
  facet_wrap(~well)+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_colour_gradient(low = "deepskyblue", high="red")+
  geom_smooth(method = "lm", se = F, color = "lemonchiffon4")+
  geom_hline(data = evi_means, aes(yintercept = evi), linetype = 2, color = "cornsilk4")+
  geom_line()+
  geom_point(aes(color = drought), size = 3)
# ggsave("figures/JAS_EVI_Drought_avgs.jpg", width = 7, height = 3, units = "in")


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
