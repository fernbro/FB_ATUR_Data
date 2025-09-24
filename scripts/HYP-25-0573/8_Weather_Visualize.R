library(tidyverse)
library(trend)

source("scripts/HYP-25-0573/1a_PRISM_Grids.R")
weather <- read_csv("data/Processed/Weather_Cumulative.csv") %>% 
  full_join(fab)


smk_mod <- function(x, ...) {
  result <- smk.test(x, ...)
  tibble(
    p.value = result$p.value,
    statistic = result$statistic
  )
}

ppt_smk <- weather %>%
  select(date, prism, ppt) %>% 
  unique() %>% 
  arrange(date) %>% 
  group_by(prism) %>%
  group_modify(~ smk_mod(ts(.x$ppt, frequency = 365)))

vpdmax_smk <- weather %>%
  select(date, prism, vpdmax) %>% 
  unique() %>% 
  arrange(date) %>% 
  group_by(prism) %>%
  group_modify(~ smk_mod(ts(.x$vpdmax, frequency = 365)))

# annuals <- weather %>%
#   mutate(year = year(date)) %>% 
#   filter(year > 2000 & year < 2024) %>% 
#   group_by(year, name, well) %>% 
#   arrange(date) %>% 
#   summarise(ppt = sum(ppt),
#             vpdmax = max(vpdmax))
# 
# ggplot(annuals, aes(x = year, y = vpdmax, group = name))+
#   geom_line()+
#   geom_smooth(aes(group = well), method = "lm")+
#   facet_wrap(~well)
# ggplot(annuals, aes(x = year, y = ppt, group = name))+
#   geom_line()+
#   geom_smooth(aes(group = well), method = "lm")+
#   facet_wrap(~well)
# 
# ggplot(weather, aes(x = date, y = vpdmax, group = name))+
#   geom_line()+
#   geom_smooth(aes(group = well), method = "lm")+
#   facet_wrap(~well)


# portion of rain falling in monsoon:

monsoon_frac <- weather %>%
  select(prism, date, ppt) %>% 
  mutate(date = date(date),
         month = month(date),
         year = year(date),
         szn = case_when(month %in% c(7, 8, 9) ~ "monsoon",
                         .default = "non")) %>% 
  filter(year != 2000, year != 2024) %>% 
  group_by(prism, year, szn) %>% 
  summarise(sznl_ppt = sum(ppt)) %>% 
  ungroup()

monsoon_ppt <- filter(monsoon_frac, szn == "monsoon")

mk_mod <- function(x, ...) {
  result <- mk.test(x, ...)
  
  tibble(
    p.value = result$p.value,
    statistic = result$statistic
  )
}

mon_ppt_smk <- monsoon_ppt %>% 
  arrange(year) %>% 
  group_by(prism) %>% 
  group_modify(~ mk_mod(ts(.x$sznl_ppt)))

#########

ppt_ratio <- monsoon_frac %>% 
  pivot_wider(values_from = sznl_ppt, names_from = szn) %>% 
  group_by(prism, year) %>% 
  summarise(ratio = monsoon/non) %>% 
  ungroup()

ggplot(ppt_ratio, aes(x = year, y = ratio, group = prism))+
  geom_line()+
  geom_smooth(method = "lm", se = F, aes(group = prism))

summary(lm(sznl_ppt ~ year, data = filter(monsoon_frac, well == "regional" & szn == "monsoon")))
summary(lm(sznl_ppt ~ year, data = filter(monsoon_frac, well == "alluvial" & szn == "monsoon")))
  


