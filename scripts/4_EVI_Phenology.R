library(tidyverse)
#install.packages("phenofit")
library(phenofit)

# library(remotes)
# 
# install_github("eco-hydro/phenofit")
# install_github("rpkgs/Ipaper")
# install_github("rpkgs/sf.extra")
# install_github("rpkgs/lattice.layers")

# EVI Measurements

# alluvial wells:

evi_alluv1 <- read_csv("data/EVI_alluvial_2013_2023.csv")
evi_alluv2 <- read_csv("data/EVI_alluvwells_2000_2013.csv")

# regional aquifer wells:
evi_gen1 <- read_csv("data/EVI_2013_2023_general.csv")
evi_gen2 <- read_csv("data/EVI_generalwells_2000_2013.csv")

# spikes and cloud mask???

evi_ts_alluvial <- rbind(evi_alluv1, 
                         evi_alluv2) # bind alluvial evi time series

evi <- evi_ts_alluvial %>%  # alluvial aquifer wells
  mutate(date = make_date(year = year, 
                          month = month, 
                          day = day),
         evi = EVI, 
         well = "alluvial") %>% 
  select(name, evi, date, well)

evi_ts_general <- rbind(evi_gen1, 
                        evi_gen2) # bind regional aqu time series

evi2 <- evi_ts_general %>% # regional aquifer wells
  mutate(date = make_date(year = year, 
                          month = month, 
                          day = day),
         evi = EVI, 
         well = "regional") %>% 
  select(name, evi, date, well)

evi_combo <- rbind(evi, evi2) %>% # combine now that they're labeled
  mutate(leap_day = ifelse(month(date) == 2 & day(date) == 29, "yes", "no"))

#plot of evi, time series for each well (colored by its aquifer type)
# ggplot(filter(evi_combo),
#        aes(x = date,
#            y = evi,
#            group = well,
#            color = well))+
#   geom_smooth()

# let's do some smoothing
# WANT TO IMPROVE LATER (for data spikes)

evi_smooth <- evi_combo %>% 
  group_by(name) %>% 
  arrange(name, date) %>% 
  mutate(evi_30 = zoo::rollmean(evi, 31, c(NA, NA, NA))) %>% 
  ungroup()

evi_averages <- evi_smooth %>% 
  group_by(date, well) %>% 
  filter(!is.na(evi_30)) %>% 
  summarise(avg_evi = mean(evi, na.rm = T), avg_30 = mean(evi_30, na.rm = T))

ggplot(filter(evi_averages, year(date) == 2012), aes(x = date, y = avg_30))+
  geom_line(aes(color = well))+
  theme_light()

evi_annual_cycle <- evi_smooth %>% 
  mutate(doy = yday(date)) %>% 
  filter(!is.na(evi_30)) %>% 
  group_by(doy, well) %>% 
  summarise(avg_evi = mean(evi, na.rm = T), 
            avg_30 = mean(evi_30, na.rm = T),
            evi_sd = sd(evi, na.rm = T),
            size = n()) %>% 
  mutate(se = evi_sd/sqrt(size),
         well = case_when(well == "alluvial" ~ "Riparian",
                          well == "regional" ~ "Upland"))

ggplot(filter(evi_annual_cycle), aes(x = doy, y = avg_evi))+
  geom_line(aes(color = well))+
  geom_ribbon(aes(fill = well, ymin = avg_evi - se, ymax = avg_evi + se),
              alpha = 1)+
  theme_light()+
  scale_fill_manual(values = c("yellow4", "tan"))+
  scale_color_manual(values = c("yellow4", "tan"))








# evi_combo <- evi_combo %>% 
#   group_by(name)

# evi_pheno <- phenofit::check_input(t = evi_combo$date, y = evi_combo$evi,
#                                    nptperyear = ifelse(leap_year(evi_combo$date) == T, 366, 365),
#                                    maxgap = 90, wmin = 0.2)

# Pheno steps:

# check_input
# season_mov
# curvefits
# get_pheno

evi_for_pheno <- evi_smooth %>% 
  group_by(name) %>% 
  arrange(date) %>%
  ungroup() %>% 
  filter(year(date) %in% seq(2001, 2022, 1),
         leap_day == "no")

evi_pheno <- list()
for(i in 1:length(unique(evi_for_pheno$name))){
  
  evi_combo_f <- filter(evi_for_pheno, name == unique(evi_for_pheno$name)[i])
  result <- phenofit::check_input(t = evi_combo_f$date, y = evi_combo_f$evi,
                                  nptperyear = 365, south = F,
                                  maxgap = 90, wmin = 0.2, wsnow = 0.8,
                                  alpha = 0.02, mask_spike = T,
                                  na.rm = F)
  evi_pheno[[i]] <- result
  
}

# moving growing season division: (rough fitting)

evi_szns <- lapply(evi_pheno, phenofit::season_mov)

# see evi_szns_using_daily

phenofit::plot_season(evi_pheno[[1]], evi_szns[[1]])

# fit the curves to each growing season(?): (smooth fitting)
evi_fit <- list()
for(i in 1:length(evi_szns)){
  result <- phenofit::curvefits(evi_pheno[[i]], evi_szns[[i]])
  evi_fit[[i]] <- result
} # curvefit vs. curvefits?
# i hope the evi fitting worked because it took >1 hour
# extract phenological dates
pheno_dates <- lapply(X = evi_fit, FUN = get_pheno, 
                      method = "Zhang", IsPlot = T)
# names(pheno_dates) <- unique(evi_combo$name)

# each df in pheno_dates has $date and $doy forms to see season values

pheno_dates <- lapply(pheno_dates, FUN = as.data.frame)
# failed for some seasons
# can rudimentarily check when EVI is above a base value?
                      