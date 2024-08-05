library(tidyverse)
library(zoo)
options(scipen = 99999)

# piezometer data
# datum for depths: NAVD88

# data: (list of files with a particular ending)
piez_files <- list.files(path = "data/USGS_2005-5163", pattern = "_DTG.csv",
                         full.names = T)
piez_data <- lapply(piez_files, read_csv)

pivot_piez_data <- function(x) {
  
  x <- x %>% 
    mutate(across(everything(), as.character))
  
  data <- pivot_longer(x,
                       cols = !c(year, month),
                       names_to = "id",
                       values_to = "dtg") %>% 
    mutate(year = as.numeric(year),
           month = as.numeric(month))
  
  return(data)
}

piez_data_lengthened <- lapply(piez_data, 
                               pivot_piez_data)

# these are monthly averages for the calendar months. should average EVI in same manner
usgs_dtg <- do.call(rbind, piez_data_lengthened) %>% 
  mutate(dtg = na_if(dtg, "---")) %>% 
  mutate(dtg = na_if(dtg, "dry"), name = id,
         date = make_date(year = year, month = month, 
                          day = days_in_month(month))) %>% 
  select(-id) %>% 
  mutate(dtg = as.numeric(dtg))


# EVI data (extracted in GEE)

evi_ts <- read_csv("data/USGS_2005-5163/EVI_USGS_study.csv") # 2000-2003

evi <- evi_ts %>%  # piezometers along floodplains/biohydro transects
  mutate(date = make_date(year = year, month = month, day = day),
         evi = EVI, well = "usgs piezometer")

# %>%
#   select(name, evi, date, well)

evi_monthly <- evi %>%
  group_by(name, year, month) %>%
  summarise(evi_month_mean = mean(evi, na.rm = T),
            evi_month_sd = sd(evi, na.rm = T),
            well = "usgs piezometer")

ggplot(evi, aes(x = date, y = evi, group = name))+
  geom_line()

# combine monthly avg EVI with monthly avg DTG

evi_dtg_usgs <- full_join(usgs_dtg, evi_monthly) %>% # auto-joins by year, month, and name
  filter(!is.na(dtg)) %>% 
  select(-date) %>% 
  mutate(dtg = as.numeric(dtg))

evi_dtg_time_series <- evi_dtg_usgs %>% 
  mutate(date = make_date(year = year, month = month, 
                          day = days_in_month(month)/2))

ggplot(evi_dtg_time_series, aes(x = date, y = dtg, group = name))+
  geom_line()+
  ylab("Monthly average depth to groundwater (m)")+
  xlab("Date")

# compute EVI and DTG statistics for sites
# not a great sample size but alas

evi_stats <- evi %>% 
  group_by(name) %>% 
  summarise(evi_mean = mean(evi, na.rm = T),
            evi_sd = sd(evi, na.rm = T),
            evi_cv = evi_sd / evi_mean) %>% 
  ungroup()

dtg_stats <- usgs_dtg %>% 
  filter(name != "Month/year") %>% 
  group_by(name) %>% 
  summarise(dtg_mean = mean(dtg, na.rm = T),
            dtg_sd = sd(dtg, na.rm = T),
            dtg_cv = dtg_sd / dtg_mean)

site_stats <- full_join(evi_stats, dtg_stats) %>%
  filter(!is.na(dtg_mean))

# z score binding & computation

z_scores <- left_join(evi_dtg_time_series, site_stats) %>% 
  mutate(evi_z = (evi_month_mean - evi_mean)/evi_sd,
         dtg_z = (dtg - dtg_mean)/dtg_sd
  )

ggplot(filter(z_scores, month == 5), aes(x = dtg_z, y = evi_z))+
  geom_point()+
  geom_smooth(method = "lm")+ # no pronounced effect
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1,1))
