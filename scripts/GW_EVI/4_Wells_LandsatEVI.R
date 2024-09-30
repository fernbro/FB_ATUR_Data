library(tidyverse)
# install.packages("zoo")
library(zoo)
library(lme4)


# EVI Measurements:

landsat_alluv <- read_csv("data/LandsatEVI_Alluvial.csv")
ls_alluv <- landsat_alluv %>% 
  transmute(date = make_date(year = year, month = month,
                             day = day),
            evi = EVI, name = name, well = "alluvial")


landsat_reg <- read_csv("data/LandsatEVI_Regional.csv")
ls_reg <- landsat_reg %>% 
  transmute(date = make_date(year = year, month = month,
                             day = day),
            evi = EVI, name = name, well = "regional")

evi_combo <- rbind(ls_alluv, ls_reg)

# groundwater data:

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
  mutate(well = "regional")

# combine groundwater measurements

water_combo <- rbind(regional, alluvial) %>% 
  filter(year(date) <= 2024) %>% 
  select(date, name, well, level)

# %>% 
#   mutate(doy = yday(date), year = year(date))

# this is what DOESN'T work:
# combined_evi <- full_join(water_combo, evi_combo, 
#                           join_by(name, date, well))


# now to combine with these weird dates....

              
              expand_landsat_evi <- function(df) {
                df <- df %>%
                  arrange(date) %>%
                  mutate(interval = c(as.numeric(difftime(lead(date), date, units = "days")))) # interval = length of composite pd
                
                expanded_df <- df %>%
                  filter(!is.na(interval)) %>%
                  rowwise() %>%
                  do(data.frame(
                    date = seq(.$date, by = "day", length.out = .$interval), 
                    # the above code creates a sequence of days for each row starting with the 
                    # row's date value and ending after the interval # of days
                    evi = rep(.$evi, .$interval) 
                    # repeat the daily value as many times as days in the interval
                  ))
                
                # rowwise() and do() operate by binding the outputs of do() back into rows
                
                return(expanded_df)
              }
              
              evi_nested <- evi_combo %>% 
                group_by(well, name) %>% 
                nest()
              
              evi_expand <- evi_nested %>% 
                mutate(data = map(data, expand_landsat_evi)) %>% 
                unnest(data)
              
        # And what if i linearly interpolate?

# merge GW with "composite expanded" EVI

combined_evi <- full_join(water_combo, evi_expand,
                          join_by(name, date, well))

# yay!

stats <- combined_evi %>%
  filter(!is.na(evi), !is.na(level)) %>%
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
  filter(!is.na(evi), !is.na(level)) %>% 
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
  filter(!is.na(evi), !is.na(level),
         month(date) %in% c(4, 5, 6, 7, 8, 9)
  ) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
         dtg_z = (level - dtg_mean)/dtg_sd,
         month = month(date))

z_scores_monthly <- combined_evi %>%  # Z-scores data frame with mean & sd for any given Month
  mutate(month = month(date)) %>% 
  full_join(stats_by_month) %>% 
  filter(!is.na(evi), !is.na(level),
         month %in% c(4, 5, 6, 7, 8, 9)) %>% 
  group_by(name, month) %>% 
  mutate(evi_z = (evi - evi_mean)/evi_sd,
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
  theme_light()+
  facet_wrap(~month(date))

ggplot(filter(z_scores, well == "alluvial"), 
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

ggplot(filter(z_scores, dtg_z < 5, well == "alluvial"), 
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

ggplot(filter(z_scores, 
              well == "alluvial",
              month(date) %in% c(7, 8, 9)), 
       aes(x = dtg_z,
           y = evi_z))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = as.factor(month(date))))+
  geom_smooth(method = "lm", se = TRUE)+
  scale_color_manual(values = c("red", "purple", "blue"))+
  theme_light()+
  ylim(c(-5,5))+
  xlab("DTG z-score")+
  ylab("EVI z-score")+
  labs(color = "Month")+
  ggtitle("JAS EVI vs. DTG z-scores relative to each month")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

summary(lm(evi_z ~ dtg_z, data = filter(z_scores, 
                                        well == "alluvial",
                                        month(date) %in% c(7, 8, 9))))


