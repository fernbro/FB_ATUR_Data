library(tidyverse)
# install.packages("randomForest")
library(randomForest)
# install.packages("caret")
library(caret)

# use:
# EVI on that day
# DTG on that day
# VPD(max) accumulated over previous 30 days
# PPT accumulated over previous 30 days

# how relatively important are the depth to groundwater, 
# recent atmospheric water demand, and recent precipitation
# in determining vegetation greenness?


upland_evi <- read_csv("data/MOD09GA_EVI_regional.csv") %>% 
  mutate(date1 = str_sub(`system:index`, 1, 10)) %>% 
  transmute(date = as.POSIXct(date1, tryFormats = "%Y_%m_%d"),
            evi = evi, name = name, well = "regional")

up_lev <- read_csv("data/Groundwater_levels_below_land_surface.csv",
                                         col_names = T)
upland_dtg <- up_lev %>% 
  transmute(date = as.POSIXct(lev_timestamp, 
                              tryFormats = c("%m/%d/%y")),
            name = station_nm, 
            level = lev_va/3.281) %>% 
  filter(year(date) >= 2000 & year(date) <= 2024)
weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  mutate(date = date(date))

combo <- inner_join(upland_dtg, upland_evi) %>% 
  inner_join(weather) %>% 
  select(evi, level, cum_ppt_30d, cum_vpd_30d) %>% 
  filter(!is.na(cum_ppt_30d), !is.na(cum_vpd_30d))

ggplot(data = combo, aes(x = evi))+
  geom_histogram(bins = 50, fill = "lightblue", color = "darkgray")

ind <- sample(2, nrow(combo), replace = TRUE, prob = c(0.7, 0.3))
train <- combo[ind==1,]
test <- combo[ind==2,]

rf <- randomForest(evi~., data=train, proximity=TRUE)
p1 <- predict(rf, test)
importance(rf)
varImpPlot(rf, type = 2)
# put predictions and source EVI data into classes?




caret::confusionMatrix(p1, test$evi)

plot(p1, test$evi)
abline(a = 1, b = 0, col = "red")


