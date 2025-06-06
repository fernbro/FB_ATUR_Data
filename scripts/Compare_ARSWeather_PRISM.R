library(tidyverse)
library(sf)

# PRISM record, 2000-2024, of these 4 ARS gages:

ars_ppt <- read_csv("data/ARS_Gages_PRISM_ppt.csv")
ars_tmean <- read_csv("data/ARS_Gages_PRISM_tmean.csv")
ars_vpdmax <- read_csv("data/ARS_Gages_PRISM_vpdmax.csv")

# only from four wells (extract rest):
ars_prism <- full_join(ars_ppt, ars_tmean) %>% 
  full_join(ars_vpdmax) %>% 
  mutate(gage = case_when(name == "wg_rg001" ~ "rg1",
                          name == "wg_rg400" ~ "rg400",
                          name == "wg_rg417" ~ "rg417",
                          name == "rg_rg418" ~ "rg418"),
         ppt_prism = ppt, date = date(date)) %>% 
  select(-name, -ppt)

# Spatial analysis: matching gages to wells

swrc_inst <- st_read("data/SWRC/SWRC_DAP_Instrumentation.shp")
usp_alluv <- st_read("data/Alluvial_well_locations.shp")
usp_reg <- st_read("data/General_USPWHIP_well_locations.shp")

swrc_usp <- subset(swrc_inst, Watershed == "USP") # only select instrumentation from the USP watershed

# use st_nearest_feature()

# reproject both

# usp_inst <- st_transform(swrc_usp, "epsg:32612")
ars_inst <- st_transform(swrc_usp, "epsg:32612") %>% 
  subset(siteTypeSt == "Raingage")
              # need to process PRISM for the rest of these points...

alluv_wells <- st_transform(usp_alluv, "epsg:32612")
reg_wells <- st_transform(usp_reg, "epsg:32612")
wells <- rbind(alluv_wells, reg_wells) # one shapefile of all project wells

inst_to_wells <- unique(st_nearest_feature(wells, ars_inst)) # pick the rain gage that's closest to each well
# for each alluvial well, what ars instrument is closest?
# RG 600 is the Charleston mesquite flux site precipitation. These data come with the flux data.

# then invert to select the well closest to each rain gage

subset_gages <- unique(ars_inst$instrument[inst_to_wells]) # extract gage ID numbers using index from the st_nearest_feature output
subset_gage_for_wells <- subset(ars_inst, instrument %in% subset_gages & instrument != 600)
st_write(subset_gage_for_wells, "data/SWRC/Gages_near_wells.shp")
# 11 gages to be used as only these 12 are closest to our wells. each gage got matched with several wells

wells_to_gages <- st_nearest_feature(subset_gage_for_wells, wells) # invert shortest distance search to ask which well is closest to each gage
ars_well_match <- unique(wells[wells_to_gages,]) # unique well names ID'd by previous line
# duplicate_well_distance <- st_nearest_feature(subset(wells, name == "D-22-20 26ABB1 [ANTELOPE 3]"), 
#                                        subset_gage_for_wells)
# ars_well_match <- ars_well_match[c(1,3:11)]
wells_to_gages2 <- st_nearest_feature(ars_well_match, subset_gage_for_wells)
matched_wells <- subset(wells, name %in% ars_well_match)
# Antelope 3 well is closest to gage [9].. as opposed to?

distances <- st_distance(matched_wells, subset_gage_for_wells, by_element = T)

ggplot()+
  geom_sf(data = ars_inst[inst_to_wells, 1:26], color = "red")+ # gages
  geom_sf(data = alluv_wells, color = "blue")+ # riparian wells
  geom_sf(data = reg_wells, color = "green")+ # upland wells
  theme_light()

ggplot()+
  geom_sf(data = ars_inst[inst_to_wells, 1:26], color = "red")+ # gages
  geom_sf(data = subset(alluv_wells, name %in% ars_well_match), color = "blue")+ # riparian wells
  geom_sf(data = subset(reg_wells, name %in% ars_well_match), color = "green")+ # upland wells
  theme_light()

rain_data_raw <- read_csv("data/SWRC/dap_02132025.csv")

names(rain_data_raw) <- c("year", "month", "day", 
                          "rg1", "rg2", "rg400", "rg402", "rg405", "rg406",
                          "rg411", "rg417", "rg418", "rg425", "rg427")

rain_data1 <- pivot_longer(rain_data_raw, cols = c(4:14),
                          names_to = "gage", values_to = "ppt")

rain_data <- rain_data1 %>% 
  filter(year != "Gage") %>% 
  mutate(date = make_date(year = year, month = month, day = day)) %>% # edit data for before some gages were turned on
  filter((gage == "rg1" & year >= 2000) | (gage == "rg2" & year >= 2000) 
         | (gage == "rg400" & year >= 2002) | (gage == "rg402" & year >= 2005)
         | (gage == "rg405" & year >= 2006) | (gage == "rg406" & year >= 2006) 
         | (gage == "rg411" & year >= 2006) | (gage == "rg417" & year >= 2007) 
         | (gage == "rg418" & year >= 2007) | (gage == "rg425" & year >= 2007)
         | (gage == "rg427" & year >= 2011))
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
  mutate(ppt_gage = case_when(is.na(ppt) ~ 0,
                         !is.na(ppt) ~ ppt),
         date = date(date)) %>% 
  select(gage, date, ppt_gage)

# combine interpolated (with zeroes) ARS gage to PRISM

ars_both <- full_join(ars_prism, rain_interp) %>%
  filter(!is.na(ppt_gage)) %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(gage, year, month) %>%
  summarise(ppt_prism = sum(ppt_prism),
            ppt_gage = sum(ppt_gage))

# rolling sums:
# library(RcppRoll)
# ars_both <- full_join(ars_prism, rain_interp) %>%
#   filter(!is.na(ppt_gage)) %>% 
#   group_by(gage) %>% 
#   arrange(date) %>% 
#   mutate(cum_prism = RcppRoll::roll_sum(ppt_prism, n = 30, fill = "NA",
#                                         align = "right"),
#          cum_gage = RcppRoll::roll_sum(ppt_gage, n = 30, fill = "NA",
#                                         align = "right")) %>% 
#   ungroup()
# there should be no NAs in the ARS data... once they're turned on

# after running Compare_FluxWeather_PRISM.R:
source("scripts/Compare_FluxWeather_PRISM.R")

ars_both <- ars_both %>% 
  mutate(gage = case_when(gage == "rg1" ~ "ARS 1",
                          gage == "rg400" ~ "ARS 400",
                          gage == "rg417" ~ "ARS 417",
                          gage == "rg418" ~ "ARS 418")) %>% 
  rename(ppt30_p = cum_prism, ppt30_f = cum_gage) %>% 
  mutate(site = 0) #dummy column

ars_flux_ppt_1 <- ars_both %>% 
  select(date, site, gage, ppt30_p, ppt30_f)

flux_ars_ppt_1 <- full_flux_w %>% 
  select(date, site, ppt30_p, ppt30_f) %>% 
  mutate(gage = case_when(site == "CMW" ~ "US-CMW",
                          site == "LS1" ~ "US-LS1",
                          site == "LS2" ~ "US-LS2",
                          site == "Wkg" ~ "US-Wkg",
                          site == "Whs" ~ "US-Whs"))

flux_ars_ppt <- rbind(ars_flux_ppt_1, flux_ars_ppt_1) %>% 
  filter(site %in% san_pedro | gage %in% c("ARS 1", "ARS 400",
                                           "ARS 417", "ARS 418")) %>% 
  mutate(site = case_when(site %in% san_pedro ~ gage))
  
ggplot(flux_ars_ppt, aes(x = ppt30_p, y = ppt30_f))+
  geom_point(pch = 1, aes(color = gage))+
  geom_smooth(method = "lm", linetype = 2, se = T)+
  geom_abline(a = 1, b = 0, color = "red")+
  theme_light(base_size = 20)+
  labs(x = "PRISM 30-day precipitation (mm)",
       y = "Gage 30-day precipitation (mm)",
       color = "Rain gage")
cor.test(flux_ars_ppt$ppt30_f, flux_ars_ppt$ppt30_p)

summary(lm(cum_prism ~ cum_gage, ars_both)) #R2 0.8423; slope = 0.8969
summary(lm(cum_gage ~ cum_prism, ars_both))
cor.test(ars_both$cum_prism, ars_both$cum_gage) # r = 0.9177598

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


