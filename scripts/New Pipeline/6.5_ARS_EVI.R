library(tidyverse)

evi_ars <- read_csv("data/MOD09GA_EVI_ARS.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 10)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y_%m_%d"),
            evi = evi, gage = as.character(instrument)) %>% 
  mutate(date = date(date))

rain_data_raw <- read_csv("data/SWRC/dap_02132025.csv")

names(rain_data_raw) <- c("year", "month", "day", 
                          "1", "2", "400", "402", "405", "406",
                          "411", "417", "418", "425", "427")

rain_data1 <- pivot_longer(rain_data_raw, cols = c(4:14),
                           names_to = "gage", values_to = "ppt")

rain_data <- rain_data1 %>% 
  filter(year != "Gage") %>% 
  mutate(date = make_date(year = year, month = month, day = day)) %>% # edit data for before some gages were turned on
  filter((gage == "1" & year >= 2000) | (gage == "2" & year >= 2000) 
         | (gage == "400" & year >= 2002) | (gage == "402" & year >= 2005)
         | (gage == "405" & year >= 2006) | (gage == "406" & year >= 2006) 
         | (gage == "411" & year >= 2006) | (gage == "417" & year >= 2007) 
         | (gage == "418" & year >= 2007) | (gage == "425" & year >= 2007)
         | (gage == "427" & year >= 2011))
# documentation of these dates: https://www.tucson.ars.ag.gov/dap/dap_docs/precipitation.html

interpolate_rg <- function(df) {
  df <- df %>%
    arrange(date) %>%
    mutate(interval = c(as.numeric(difftime(lead(date), date, units = "days")))) # interval = length of composite pd
  
  expanded_df <- df %>%
    filter(!is.na(interval)) %>%
    rowwise() %>%     # rowwise() and do() operate by binding the outputs of do() back into rows
    do(data.frame(
      date = seq(.$date, by = "day", length.out = .$interval)
      # the above code creates a sequence of days for each row starting with the 
    ))
  
  return(expanded_df)
}

rain_nested <- rain_data %>%
  group_by(gage) %>%
  nest()

rain_interp <- rain_nested %>%
  mutate(data = map(data, interpolate_rg)) %>% # map the interpolation function to each site (called "data" by the nest fxn) in the collection we created above
  unnest(data) %>%  # now unnest to make one dataframe again with its site column
  full_join(rain_data, join_by(gage, date)) %>% # now bring back data to put observations in where they exist...
  mutate(ppt = case_when(is.na(ppt) ~ 0,
                              !is.na(ppt) ~ ppt),
         date = date(date)) %>% 
  select(gage, date, ppt) %>% 
  ungroup()

evi_ars_ppt <- inner_join(rain_interp, evi_ars, by = join_by(date, gage))
monsoon <- evi_ars_ppt %>% 
  mutate(month = month(date)) %>% 
  # group_by(gage) %>% 
  # arrange(date) %>% 
  # mutate(evi_lead1 = lead(evi, n = 1),
  #        evi_lead3 = lead(evi, n = 3),
  #        evi_lead5 = lead(evi, n = 5),
  #        evi_lead10 = lead(evi, n = 10),
  #        evi_lead15 = lead(evi, n = 15)) %>%  # for comparing ppt with the next day's evi
  # ungroup() %>% 
  filter(month %in% c(7, 8, 9))

monthly <- evi_ars_ppt %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(gage, month, year) %>% 
  summarise(ppt = sum(ppt), evi = mean(evi, na.rm = T))


# how can i add in a lag of like.... 15 days or something?

summary(lm(evi ~ ppt, filter(monthly, month %in% c(7, 8, 9))))

cor.test(filter(monthly, month %in% c(7, 8, 9))$ppt, filter(monthly, month %in% c(7, 8, 9))$evi)
# at least it has a significant correlation...? 12%?

ggplot(filter(monthly, month %in% c(7, 8, 9)), aes(x = ppt, y = evi))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(monsoon, aes(x = ppt, y = evi_lead10))+
  geom_point(aes(x = ppt, y = evi_lead10), color = "red")+
  geom_point(aes(x = ppt, y = evi), color = "blue")

ggplot(monsoon, aes(x = ppt, y = evi))+
  geom_point()


