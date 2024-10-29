library(tidyverse)
library(sf)

swrc_inst <- st_read("data/SWRC/SWRC_DAP_Instrumentation.shp")
usp_alluv <- st_read("data/Alluvial_well_locations.shp")

swrc_usp <- subset(swrc_inst, Watershed == "USP")

# use st_nearest_feature()

# reproject both

usp_inst <- st_transform(swrc_usp, "epsg:32612")
ars_inst <- st_transform(swrc_inst, "epsg:32612") %>% 
  subset(siteTypeSt == "Raingage")
# need to process PRISM for these points...

alluv_wells <- st_transform(usp_alluv, "epsg:32612")

inst_to_wells <- (st_nearest_feature(alluv_wells, ars_inst))
# for each alluvial well, what ars instrument is closest?
# RG 600 is the Charleston mesquite flux site precipitation. These data come with the flux data.

ggplot()+
  geom_sf(data = ars_inst[inst_to_wells, 1:26], color = "red")+
  geom_sf(data = alluv_wells, color = "blue")+
  theme_light()

unique(swrc_inst$instrument[inst_to_wells])

rain_data_raw <- read_csv("data/SWRC/dap_aggregate_10282024.csv")

names(rain_data_raw) <- c("year", "month", "day", 
                          "rg1", "rg400", "rg417", "rg418")

rain_data1 <- pivot_longer(rain_data_raw, cols = c("rg1", "rg400", "rg417", "rg418"),
                          names_to = "gage", values_to = "ppt")

rain_data <- rain_data1 %>% 
  filter(year != "Gage") %>% 
  mutate(date = make_date(year = year, month = month, day = day)) %>% # edit data for before some gages were turned on
  filter((gage == "rg1" & year >= 2000) | (gage == "rg400" & year >= 2002) | 
           (gage == "rg417" & year >= 2007) | (gage == "rg418" & year >= 2007))
# documentation of these dates: https://www.tucson.ars.ag.gov/dap/dap_docs/precipitation.html



# need to inerpolate with zeroes to have same structure as PRISM/total comparison
# especially seeing where this dataset reads zero and prism doesn't!
  
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
  mutate(data = map(data, interpolate_rg)) %>%
  # map the interpolation function to each site (called "data" by the nest fxn) in the collection we created above
  unnest(data) %>%  # now unnest to make one dataframe again with its site column
  full_join(rain_data, join_by(gage, date)) %>% # now bring back data to put observations in where they exist...
  mutate(ppt = case_when(is.na(ppt) ~ 0,
                         !is.na(ppt) ~ ppt)) %>% 
  select(gage, date, ppt)

ggplot(filter(rain_interp, year(date) %in% c(2020, 2021)), aes(x = date, y = ppt))+
  geom_line(aes(group = gage, color = gage))+
  theme_light()+
  labs(x = "Date", y = "Daily precipitation (mm)")+
  facet_wrap(~gage + year(date), scales = "free_x")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

# let's aggregate to some time periods

ars_monthly <- rain_interp %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(gage, year, month) %>%
  summarise(ppt = sum(ppt))

ars_yearly <- rain_interp %>%
  mutate(year = year(date)) %>% 
  filter(year != 2024) %>% 
  group_by(gage, year) %>%
  summarise(ppt = sum(ppt))

ggplot(ars_yearly, aes(x = year, y = ppt))+
  geom_line()+
  geom_hline(yintercept = mean(ars_yearly$ppt), color = "red")+
  facet_wrap(~gage)+
  theme_light()+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))
# currently: extracting PRISM to compare explicitly with these gauges!

