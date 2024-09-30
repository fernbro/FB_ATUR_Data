# Code for processing MODIS composite ET from a CSV to get calendar month values
# by Fern Bromley

# Load in libraries:

library(tidyverse)
library(terra)
#install.packages("lme4")
library(lme4)
options(scipen=9999999)

# Function for cute ggplot labels:

custom_month_labels <- function(x) {
  months <- format(x, "%b")
  first_letter_months <- toupper(substr(months, 1, 1))
  return(first_letter_months)
}

# Read in MODIS data
modis_et_raw <- read_csv("../../../Data/Remote_ET/MODIS_ET.csv")
colnames(modis_et_raw) <- c("site", "date", "et")

# Format MODIS data
# scales ET (since depth units are in 0.1 mm) and formats dates

modis_et <- modis_et_raw %>% 
  mutate(et = et/10, date = as.POSIXct(date,
                                       tryFormats = c("%m/%d/%Y"))) %>% 
  group_by(site) %>% 
  arrange(date) %>% 
  filter(site %in% c("US-CMW", "US-LS1", "US-LS2", "US-Whs", "US-Wkg"))



# function for "expanding" MODIS dataframe by filling in all dates, 
# computing the length of composite period, and 
# the daily average ET over the composite period 

expand_modis_data <- function(df) {
  df <- df %>%
    arrange(date) %>%
    mutate(interval = c(as.numeric(difftime(lead(date), date, units = "days"))), # interval = length of composite pd
           daily_value = et / interval)
  
  expanded_df <- df %>%
    filter(!is.na(interval)) %>%
    rowwise() %>%
    do(data.frame(
      date = seq(.$date, by = "day", length.out = .$interval), 
      # the above code creates a sequence of days for each row starting with the 
      # row's date value and ending after the interval # of days
      daily_value = rep(.$daily_value, .$interval) 
      # repeat the daily value as many times as days in the interval
    ))
  
  # rowwise() and do() operate by binding the outputs of do() back into rows
  
  return(expanded_df)
}

# turns MODIS data into a collection of lists unique to each site
# this makes sure that the interpolation works site-wise 

modis_data_nested <- modis_et %>%
  group_by(site) %>%
  nest()

modis_data_expanded <- modis_data_nested %>%
  mutate(data = map(data, expand_modis_data)) %>%
  # map the expand_modis_data function to each site (called "data" by the nest fxn) in the collection we created above
  unnest(data) # now unnest to make one dataframe again with its site column


# compute values for each calendar month by summing daily values from each month, site, and year

modis_monthly_vals <- modis_data_expanded %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(et_monthly = sum(daily_value)) %>% 
  mutate(date = make_date(year = year, month = month,
                          day = days_in_month(month)),
         site = str_sub(site, 4, 6)) %>%
  ungroup()


# the rest of this code formats the MODIS data to be compared 
# with post-processed flux tower data

modis_monthly <- modis_monthly_vals %>% 
  mutate(value = "modis", ET = et_monthly) %>% 
  select(site, date, value, ET)

flux_monthly <- read_csv("../../../Data/Flux/Ameriflux_BASE_monthly_ET.csv")
flux_modis <- rbind(flux_monthly, modis_monthly)

et_monthly_avgs <- flux_modis %>% 
  mutate(month = month(date)) %>% 
  filter(site != "Fmf") %>%
  group_by(value, site, month) %>% 
  summarise(ET = mean(ET, na.rm = T)) %>% 
  mutate(date = make_date(year = 1900, month = month, day = 1))

filter(et_monthly_avgs) %>% 
  ggplot(aes(x = date, y = ET, group = interaction(site, value), 
             color = site, linetype = value))+
  geom_line(size = 2, show.legend = F)+
  xlab("Month")+
  ylab("ET (mm)")+
  scale_x_date(labels = function(x) custom_month_labels(x), date_breaks = "1 months", expand = c(0, 0))+
  theme_light(base_size = 40)+
  theme(text=element_text(family = "serif"),
        panel.background = element_rect(fill = "transparent"), # Panel background
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"))
