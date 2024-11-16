library(tidyverse)
library(RcppRoll)
library(correlation)

total_z <- read_csv("data/Processed/GW_EVI_Weather.csv")

# vpd_test <- total_z %>% 
#   select(well, evi, vpdmax, vpd_5d:vpd_60d) %>% 
#   group_by(well) %>% 
#   correlation() %>% 
#   filter(Parameter1 == "evi")

ppt_test <- total_z %>% 
  select(well, evi, ppt, ppt_5d:ppt_60d) %>% 
  group_by(well) %>% 
  correlation(method = "pearson") %>% 
  filter(Parameter1 == "evi")

plot(ppt_30d ~ cum_ppt_30d, total_z)

cor.test(filter(total_z, well == "Riparian")$evi, filter(total_z, well == "Riparian")$cum_ppt_30d)

vpd_test_m <- total_z %>% 
  select(well, month, evi, vpdmax, vpd_5d:vpd_60d) %>% 
  group_by(well, month) %>% 
  correlation() %>% 
  separate(Group, into = c("well", "month"), sep = " - ") %>% 
  filter(Parameter1 == "evi") %>% 
  mutate(month = as.numeric(month),
         days = case_when(Parameter2 == "vpdmax" ~ 1,
                          Parameter2 == "vpd_5d" ~ 5,
                          Parameter2 == "vpd_10d" ~ 10,
                          Parameter2 == "vpd_20d" ~ 20,
                          Parameter2 == "vpd_30d" ~ 30,
                          Parameter2 == "vpd_45d" ~ 45,
                          Parameter2 == "vpd_60d" ~ 60
                          ))

ggplot(vpd_test_m, aes(x = days, y = r, 
                       group = well))+
  geom_hline(yintercept = 0, color = "gray")+
  geom_point(aes(color = well))+
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.3)+
  facet_wrap(~month)+
  geom_line(linetype = 2)+
  theme_light()



vpd_cors <- total_z %>% 
  group_by(well) %>% 
  summarise(cor1 = (cor.test(evi, vpdmax))$estimate,
            ci_lo1 = (cor.test(evi, vpdmax))$conf.int[1],
            ci_up1 = (cor.test(evi, vpdmax))$conf.int[2],
            
            cor5 = (cor.test(evi, vpd_5d))$estimate,
            ci_lo5 = (cor.test(evi, vpd_5d))$conf.int[1],
            ci_up5 = (cor.test(evi, vpd_5d))$conf.int[2],
            
            cor10 = (cor.test(evi, vpd_10d))$estimate,
            ci_lo10 = (cor.test(evi, vpd_10d))$conf.int[1],
            ci_up10 = (cor.test(evi, vpd_10d))$conf.int[2],
            
            cor20 = (cor.test(evi, vpd_20d))$estimate,
            ci_lo20 = (cor.test(evi, vpd_20d))$conf.int[1],
            ci_up20 = (cor.test(evi, vpd_20d))$conf.int[2],
            
            cor30 = (cor.test(evi, vpd_30d))$estimate,
            ci_lo30 = (cor.test(evi, vpd_30d))$conf.int[1],
            ci_up30 = (cor.test(evi, vpd_30d))$conf.int[2],
            
            cor45 = (cor.test(evi, vpd_45d))$estimate,
            ci_lo45 = (cor.test(evi, vpd_45d))$conf.int[1],
            ci_up45 = (cor.test(evi, vpd_45d))$conf.int[2],
            
            cor60 = (cor.test(evi, vpd_60d))$estimate,
            ci_lo60 = (cor.test(evi, vpd_60d))$conf.int[1],
            ci_up60 = (cor.test(evi, vpd_60d))$conf.int[2]
            )


vpd_cor <- vpd_cors %>% 
  select(well, starts_with("cor")) %>% 
  pivot_longer(cols = c("cor1":"cor60"),
    values_to = "cor") %>% 
  mutate(days = gsub(x = name, pattern = "cor", replacement = "", fixed = T)) %>% 
  select(well, cor, days)
vpd_cilo <-vpd_cors %>% 
  select(well, starts_with("ci_lo")) %>% 
  pivot_longer(cols = c("ci_lo1":"ci_lo60"),
               values_to = "ci_lo") %>% 
  mutate(days = gsub(x = name, pattern = "ci_lo", replacement = "", fixed = T)) %>% 
  select(well, ci_lo, days)
vpd_ciup <-vpd_cors %>% 
  select(well, starts_with("ci_up")) %>% 
  pivot_longer(cols = c("ci_up1":"ci_up60"),
               values_to = "ci_up") %>% 
  mutate(days = gsub(x = name, pattern = "ci_up", replacement = "", fixed = T)) %>% 
  select(well, ci_up, days)

vpd_corr <- full_join(vpd_cor, vpd_cilo) %>% 
  full_join(vpd_ciup) %>% 
  mutate(days = as.numeric(days))

ggplot(vpd_corr,
       aes(x = days, y = cor))+
  geom_line(aes(group = well), linetype = 2, color = "gray60")+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), width = 1)+
  geom_point(aes(color = well), size = 2)+
  theme_light(base_size = 26)+
  labs(color = "Well", x = "Rolling sum window length (days)",
       y = "Correlation coefficient")+
  ggtitle("EVI - Daily max cumulative VPD correlation")

vpd_cors0 <- total_z %>% 
  filter(cum_ppt_30d == 0) %>% 
  group_by(well) %>% 
  summarise(cor1 = (cor.test(evi, vpdmax))$estimate,
            ci_lo1 = (cor.test(evi, vpdmax))$conf.int[1],
            ci_up1 = (cor.test(evi, vpdmax))$conf.int[2],
            
            cor5 = (cor.test(evi, vpd_5d))$estimate,
            ci_lo5 = (cor.test(evi, vpd_5d))$conf.int[1],
            ci_up5 = (cor.test(evi, vpd_5d))$conf.int[2],
            
            cor10 = (cor.test(evi, vpd_10d))$estimate,
            ci_lo10 = (cor.test(evi, vpd_10d))$conf.int[1],
            ci_up10 = (cor.test(evi, vpd_10d))$conf.int[2],
            
            cor20 = (cor.test(evi, vpd_20d))$estimate,
            ci_lo20 = (cor.test(evi, vpd_20d))$conf.int[1],
            ci_up20 = (cor.test(evi, vpd_20d))$conf.int[2],
            
            cor30 = (cor.test(evi, vpd_30d))$estimate,
            ci_lo30 = (cor.test(evi, vpd_30d))$conf.int[1],
            ci_up30 = (cor.test(evi, vpd_30d))$conf.int[2],
            
            cor45 = (cor.test(evi, vpd_45d))$estimate,
            ci_lo45 = (cor.test(evi, vpd_45d))$conf.int[1],
            ci_up45 = (cor.test(evi, vpd_45d))$conf.int[2],
            
            cor60 = (cor.test(evi, vpd_60d))$estimate,
            ci_lo60 = (cor.test(evi, vpd_60d))$conf.int[1],
            ci_up60 = (cor.test(evi, vpd_60d))$conf.int[2]
  )


vpd_cor0 <- vpd_cors0 %>% 
  select(well, starts_with("cor")) %>% 
  pivot_longer(cols = c("cor1":"cor60"),
               values_to = "cor") %>% 
  mutate(days = gsub(x = name, pattern = "cor", replacement = "", fixed = T)) %>% 
  select(well, cor, days)
vpd_cilo0 <-vpd_cors0 %>% 
  select(well, starts_with("ci_lo")) %>% 
  pivot_longer(cols = c("ci_lo1":"ci_lo60"),
               values_to = "ci_lo") %>% 
  mutate(days = gsub(x = name, pattern = "ci_lo", replacement = "", fixed = T)) %>% 
  select(well, ci_lo, days)
vpd_ciup0 <-vpd_cors0 %>% 
  select(well, starts_with("ci_up")) %>% 
  pivot_longer(cols = c("ci_up1":"ci_up60"),
               values_to = "ci_up") %>% 
  mutate(days = gsub(x = name, pattern = "ci_up", replacement = "", fixed = T)) %>% 
  select(well, ci_up, days)

vpd_corr0 <- full_join(vpd_cor0, vpd_cilo0) %>% 
  full_join(vpd_ciup0) %>% 
  mutate(days = as.numeric(days))

ggplot(vpd_corr0,
       aes(x = days, y = cor))+
  geom_line(aes(group = well), linetype = 2, color = "gray60")+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), width = 1)+
  geom_point(aes(color = well), size = 2)+
  theme_light(base_size = 25)+
  labs(color = "Well", x = "Rolling sum window length (days)",
       y = "Correlation coefficient")+
  ggtitle("EVI - Daily max cumulative VPD: 30-day PPT = 0")

vpd_corr0$method <- "ppt30 = 0"
vpd_corr$method <- "all days"

vpd_corr_combo <- rbind(vpd_corr, vpd_corr0)

ggplot(vpd_corr_combo,
       aes(x = days, y = cor))+
  geom_line(aes(group = well), linetype = 2, color = "gray60")+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), width = 1)+
  geom_point(aes(color = well), size = 2)+
  theme_light(base_size = 25)+
  labs(color = "Well", x = "Rolling sum window length (days)",
       y = "Correlation coefficient")+
  facet_wrap(~method)+
  ggtitle("EVI - cumulative daily max VPD")+
  theme(strip.background = element_rect(color = "black", fill = "white"))+
  theme(strip.text = element_text(colour = 'black'))

# now make month-wise correlations:
vpd_cors_m <- total_z %>% 
  group_by(well, month) %>% 
  mutate(upland_8 = case_when(well == "Upland" & month == 8 ~ "yes",
                              .default = "no")) %>% 
  summarise(cor1 = (cor.test(evi, vpdmax))$estimate,
            ci_lo1 = (cor.test(evi, vpdmax))$conf.int[1],
            ci_up1 = (cor.test(evi, vpdmax))$conf.int[2],
            
            cor5 = (cor.test(evi, vpd_5d))$estimate,
            ci_lo5 = (cor.test(evi, vpd_5d))$conf.int[1],
            ci_up5 = (cor.test(evi, vpd_5d))$conf.int[2], # how to just return NA if error?
            
            cor10 = (cor.test(evi, vpd_10d))$estimate,
            ci_lo10 = (cor.test(evi, vpd_10d))$conf.int[1],
            ci_up10 = (cor.test(evi, vpd_10d))$conf.int[2],
            
            cor20 = (cor.test(evi, vpd_20d))$estimate,
            ci_lo20 = (cor.test(evi, vpd_20d))$conf.int[1],
            ci_up20 = (cor.test(evi, vpd_20d))$conf.int[2],
            
            cor30 = (cor.test(evi, vpd_30d))$estimate,
            ci_lo30 = (cor.test(evi, vpd_30d))$conf.int[1],
            ci_up30 = (cor.test(evi, vpd_30d))$conf.int[2],
            
            cor45 = (cor.test(evi, vpd_45d))$estimate,
            ci_lo45 = (cor.test(evi, vpd_45d))$conf.int[1],
            ci_up45 = (cor.test(evi, vpd_45d))$conf.int[2],
            
            cor60 = (cor.test(evi, vpd_60d))$estimate,
            ci_lo60 = (cor.test(evi, vpd_60d))$conf.int[1],
            ci_up60 = (cor.test(evi, vpd_60d))$conf.int[2]
  )


