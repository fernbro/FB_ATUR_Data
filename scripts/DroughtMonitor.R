library(tidyverse)

# drought monitor results for US section of HUC15050202:
# from https://droughtmonitor.unl.edu/DmData/DataTables.aspx

# elise: desert laboratory director, gals

usp_dm_raw <- read_csv("data/Drought/USDM_HUC15050202.csv")

usp_dm_monthly <- usp_dm_raw %>% 
  mutate(date = as.POSIXct(Week),
         year = year(date), month = month(date)) %>%
  group_by(year, month) %>% 
  summarise(no_drought = mean(None), 
            drought = mean(D0 + D1 + D2 + D3 + D4)) 

# 
# add all drought categories and then take the mean

usp_dm_annual <- usp_dm_monthly %>% 
  group_by(year) %>% 
  summarise(no_drought = mean(no_drought),
            drought = mean(drought))


# SPI data:

usp_spi_raw <- read_csv("data/Drought/SPI_CochiseCo.csv")

usp_spi <- usp_spi_raw %>% 
  mutate(date = as.POSIXct(str_sub(DATE, 3, 10), 
                           tryFormats = "%Y%m%d")) 

usp_spi_monthly <- usp_spi %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month) %>% 
  summarise(drought = mean(D0), no_drought = 100 - drought)

usp_spi_monsoon <- usp_spi %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(month %in% c(7, 8, 9)) %>% 
  group_by(year) %>% 
  summarise(drought = mean(D0), no_drought = 100 - drought)

usp_spi_annual <- usp_spi %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year) %>% 
  summarise(drought = mean(D0), no_drought = 100 - drought)

ggplot(usp_spi_annual, aes(x = year, y = drought))+
  geom_point()

usp_droughtyears <- usp_spi_annual %>% 
  filter(drought >= 50)
