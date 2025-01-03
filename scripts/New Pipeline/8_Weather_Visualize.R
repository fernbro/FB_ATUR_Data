library(tidyverse)

weather <- read_csv("data/Processed/Weather_Cumulative.csv")

annuals <- weather %>%
  mutate(year = year(date)) %>% 
  filter(year > 2000 & year < 2024) %>% 
  group_by(year, name, well) %>% 
  arrange(date) %>% 
  summarise(ppt = sum(ppt),
            vpdmax = max(vpdmax))

ggplot(annuals, aes(x = year, y = vpdmax, group = name))+
  geom_line()+
  geom_smooth(aes(group = well), method = "lm")+
  facet_wrap(~well)
ggplot(annuals, aes(x = year, y = ppt, group = name))+
  geom_line()+
  geom_smooth(aes(group = well), method = "lm")+
  facet_wrap(~well)

ggplot(weather, aes(x = date, y = vpdmax, group = name))+
  geom_line()+
  geom_smooth(aes(group = well), method = "lm")+
  facet_wrap(~well)
