library(tidyverse)
library(lme4)

ppt1 <- read_csv("data/USP_AlluvialWells_PRISM_ppt.csv")
tmean1 <- read_csv("data/USP_AlluvialWells_PRISM_tmean.csv")
vpdmax1 <- read_csv("data/USP_AlluvialWells_PRISM_vpdmax.csv")

weather1 <- full_join(ppt1, tmean1, join_by(name, date)) %>% 
  full_join(vpdmax1, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, well = "alluvial",
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T)))

ppt2 <- read_csv("data/USP_RegionalWells_PRISM_ppt.csv")
tmean2 <- read_csv("data/USP_RegionalWells_PRISM_tmean.csv")
vpdmax2 <- read_csv("data/USP_RegionalWells_PRISM_vpdmax.csv")

weather2 <- full_join(ppt2, tmean2, join_by(name, date)) %>% 
  full_join(vpdmax2, join_by(name, date)) %>% 
  mutate(vpdmax = vpdmax / 10, well = "regional",
         date = as.POSIXct(gsub(date, pattern=" 07:00:00", replacement="", fixed=T)))

  
weather <- rbind(weather1, weather2) %>% 
  mutate(year = year(date))
# UNITS:
# ppt: mm per day
# tmean: celsius
# vpdmax: hPa (10^2 pascals); converted to kPa


monthly_weather <- weather %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(well, name, year, month) %>% 
  summarise(ppt = sum(ppt), tmean = mean(tmean, na.rm = T),
            vpdmax = mean(vpdmax, na.rm = T))

# we want to understand antecedent moisture conditions.
# prev. water year's precipitation; calculate precipitation 
# precipitation up to date in z score dataframe

# function(date){
#   
#   wy_ppt <- filter(weather, date %in% 
#                      seq(make_date(year = year(date)-1, 
#                                    month = 10, 
#                                    day = 1),
#                          date,
#                          by = 1)) %>% 
#     select(ppt) %>% 
#     sum()
#     
# }

'%!in%' <- function(x,y)!('%in%'(x,y))

# weather year precipitation PRIOR to monsoon:
weather_wy <- weather %>%
  mutate(water_year = case_when(month(date) >= 11 ~ year(date) + 1,
                                .default = year(date))) %>% 
  filter(water_year != 2000, water_year != 2024) %>% # we don't have data for 1999, and WY 2024 is not over
  group_by(name, water_year) %>% 
  filter(month(date) %!in% c(7, 8, 9)) %>% # exclude july, august, and september: summing total rain from oct-june
  summarise(wy_ppt = sum(ppt)) %>% 
  rename(year = water_year)

wy_avgs <- weather_wy %>%
  group_by(year) %>% 
  summarise(wy_ppt = mean(wy_ppt))

ggplot(wy_avgs, aes(x = year, y = wy_ppt))+
  geom_point()+
  geom_line()+
  theme_light(base_size = 26)+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Pre-monsoon precip (Nov - June) (mm)")+
  geom_hline(yintercept = mean(weather_wy$wy_ppt), linetype = 2)


wy_stats <- weather_wy %>% 
  group_by(name) %>% 
  summarise(wy_avg = mean(wy_ppt, na.rm = T),
            wy_sd = sd(wy_ppt, na.rm = T))

wy_z <- weather_wy %>% 
  full_join(wy_stats) %>% 
  mutate(wy_z = (wy_ppt - wy_avg)/wy_sd)


ggplot(filter(weather, year(date) == 2015), 
       aes(x = date,
           y = ppt))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_vline(xintercept = 0, color = "gray")+
  geom_point(aes(color = vpdmax))+
  theme_light()


ggplot(filter(weather), aes(x = tmean))+
  geom_histogram(binwidth = 0.5)+
  theme_light()


# Z-score data:
# (monsoonal drought)

z_scores <- read_csv("data/Processed/USP_GW_EVI_Z.csv") %>%
  mutate(date = as.POSIXct(gsub(date, pattern=" 07:00:00", 
                                replacement="", fixed=T)))

weather_z <- full_join(z_scores, weather, join_by(name, date, well, year)) %>% 
  filter(!is.na(evi_z)) %>% 
  inner_join(wy_z, join_by(name, year)) %>% 
  mutate(winter = case_when(wy_z < -0.5 ~ "dry",
                                wy_z > 0.5 ~ "wet",
                            .default = "avg"))
# !!~~~~~~~~!!
# best models:
model <- (lm(evi_z ~ dtg_z + vpdmax + wy_z, 
             filter(weather_z, well == "alluvial")))
summary(model)

plot(filter(weather_z, well == "alluvial")$evi_z, fitted(model))
abline(a = 0, b = 1, col = "red")

hist(residuals(model), breaks = 100)

ggplot(filter(weather_z, dtg_z < 5), aes(x = dtg_z, y = evi_z))+
  geom_point(aes(color = name))+
  geom_smooth(method = "lm", se = F)+
  theme_light()+
  facet_wrap(~year(date))+
  theme(legend.position = "none")

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monthly weather patterns:

monthly_normals <- monthly_weather %>% 
  group_by(well, name, month) %>% 
  summarise(ppt = mean(ppt),
            tmean = mean(tmean),
            vpdmax = mean(vpdmax))

ggplot(monthly_normals, aes(x = as.factor(month), y = ppt, 
                            color = well, group = name))+
  geom_line()+
  theme_light()+
  xlab("Month")+
  ylab("Average monthly precipitation (mm)")+
  theme(legend.position = "none")
# ggsave("figures/well_ppt_trends.jpg", last_plot(), 
       # width = 5, height = 5, units = "in")

ggplot(monthly_normals, aes(x = as.factor(month), y = vpdmax, 
                            color = well, group = name))+
  geom_line()+
  theme_light()+
  xlab("Month")+
  ylab("Average daily maximum VPD (kPa)")
# ggsave("figures/well_vpdmax_trends.jpg", last_plot(), 
       # width = 6, height = 5, units = "in")

ggplot(monthly_normals, aes(x = as.factor(month), y = tmean, 
                            color = well, group = name))+
  geom_line()+
  theme_light()+
  xlab("Month")+
  ylab("Average daily mean temperature (degC)")
# ggsave("figures/well_tmean_trends.jpg", last_plot(), 
#        width = 6, height = 5, units = "in")
