library(tidyverse)
library(trend)

weather <- read_csv("data/Processed/Weather_Cumulative.csv")

smk_mod <- function(x, ...) {
  result <- smk.test(x, ...)
  
  tibble(
    p.value = result$p.value,
    statistic = result$statistic
  )
}

ppt_smk <- weather %>%
  arrange(date) %>% 
  group_by(well, name) %>%
  group_modify(~ smk_mod(ts(.x$ppt, frequency = 365)))

vpdmax_smk <- weather %>%
  arrange(date) %>% 
  group_by(well, name) %>%
  group_modify(~ smk_mod(ts(.x$vpdmax, frequency = 365)))

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
