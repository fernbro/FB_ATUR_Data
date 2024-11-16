library(tidyverse)

evi <- read_csv("data/Processed/USP_EVI_Z_11132024.csv") %>% 
  mutate(well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"),
         date = date(date))

gw <- read_csv("data/Processed/USP_GW_Zscores_11142024.csv") %>% 
  mutate(date = date(date))

z_scores <- full_join(evi, gw) %>% 
  filter(!is.na(month))

ggplot(z_scores, aes(x = dtg_z, y = evi_z))+
  geom_point()+
  facet_wrap(~well, scales = "free")

filter(z_scores, well == "Riparian", month %in% c(7, 8, 9)) %>% 
ggplot(aes(x = dtg_z, y = evi_z))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~name)




# correlation analysis:
correlations_dtg <- z_scores %>% 
  group_by(well, month) %>% 
  summarise(cor = (cor.test(evi_z, dtg_z))$estimate,
            ci_lo = (cor.test(evi_z, dtg_z))$conf.int[1],
            ci_up = (cor.test(evi_z, dtg_z))$conf.int[2],
            p = (cor.test(evi_z, dtg_z))$p.value) %>% 
  mutate(sig = case_when(p <= 0.05 ~ "≤ 0.05",
                         p > 0.05 ~ " > 0.05"))

ggplot(correlations_dtg, aes(x = as.factor(month), y = cor, shape = sig))+
  geom_line(aes(group = well), linetype = 2)+
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_up), 
                alpha = 0.5, width = 0.2)+
  geom_point(aes(color = well), size = 6)+
  theme_light(base_size = 20)+
  geom_hline(yintercept = 0, color = "gray60")+
  labs(x = "Month", y = "Correlation coefficient", 
       color = "Well location", shape = "p(r ≠ 0)")
# ggsave("figures/Monthly_EVI_DTG_z_cors.jpg", last_plot(), units = "in", 
#        width = 24, height = 16, dpi = 600)

# after running script 6: 
# compare weather correlations with groundwater correlations temporally

correlations_dtg$variable <- "dtg_z" # actually evi z - dtg z
correlations_vpd$variable <- "vpd"
correlations_vpd_0p$variable <- "vpd_0p"

cor_combo_wide <- full_join(correlations_dtg, correlations_vpd, by = join_by(well, month))
cor_combo_wide2 <- full_join(correlations_dtg, correlations_vpd_0p, by = join_by(well, month))

ggplot(cor_combo_wide, aes(x = cor.x, y = cor.y, label = as.character(month)))+
  geom_point(aes(color = well))+
  geom_errorbar(aes(ymin = ci_lo.y, ymax = ci_up.y, xmin = ci_lo.x, xmax = ci_up.x),
                width = 0)+
  geom_text(nudge_x = 0.03)+
  labs(color = "Well location", x = "EVI z-score and DTG z-score correlation",
       y = "EVI and VPD30 correlation")

ggplot(cor_combo_wide2, aes(x = cor.x, y = cor.y, label = as.character(month)))+
  geom_point(aes(color = well))+
  geom_errorbar(aes(ymin = ci_lo.y, ymax = ci_up.y),
                width = 0)+
  geom_errorbar(aes(xmin = ci_lo.x, xmax = ci_up.x),
                width = 0)+
  geom_text(nudge_x = 0.03)+
  labs(color = "Well location", x = "EVI z-score and DTG z-score correlation",
       y = "EVI and VPD30 correlation")


