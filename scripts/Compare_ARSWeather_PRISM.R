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

rain_data_int <- rain_data %>% 
  


ggplot(rain_data, aes(x = date, y = ppt))+
  geom_line(aes(group = gage, color = gage))+
  theme_light()+
  facet_wrap(~gage, scales = "free_x")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))


