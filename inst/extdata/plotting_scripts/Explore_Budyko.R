library(formattable)
library(readr)
library(dplyr)
library(ggthemes)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(RColorBrewer)
library(scico)
library(cowplot)


data_path <- "../data"

# df_etr <- read_csv(paste0(data_path, "/Climate_Data/gridmet/etr_mm_gridmet_gaged_1979_2021_mean.csv")) %>%
#   pivot_longer(cols =  -c('Date'), values_to = 'value', names_to= 'site') %>%
#   mutate(Date = as_date(Date)) %>%
#   mutate(cy = ifelse(month(Date)>=4,year(Date)+1,year(Date))) %>%
#   filter(Date >= "1950-04-01") %>%
#   filter(Date < "2020-04-01" ) %>%
#   group_by(site) %>%
#   summarise(mean_etr = mean(value, na.rm = TRUE))
# 
# 
# df_pet <- read_csv(paste0(data_path, "/Climate_Data/gridmet/pet_mm_gridmet_gaged_1979_2021_mean.csv")) %>%
#   pivot_longer(cols =  -c('Date'), values_to = 'value', names_to= 'site') %>%
#   mutate(Date = as_date(Date)) %>%
#   mutate(cy = ifelse(month(Date)>=4,year(Date)+1,year(Date))) %>%
#   filter(Date >= "1950-04-01") %>%
#   filter(Date < "2020-04-01" ) %>%
#   group_by(site) %>%
#   summarise(mean_pet = mean(value, na.rm = TRUE))
# 
df_p_gm <- read_csv(paste0(data_path, "/Climate_Data/gridmet/pr_mm_gridmet_conus_gaged_1980_2020_mean.csv")) %>%
  pivot_longer(cols =  -c('Date'), values_to = 'value', names_to= 'site') %>%
  mutate(Date = as_date(Date)) %>%
  # mutate(cy = ifelse(month(Date)>=4,year(Date)+1,year(Date))) %>%
  filter(Date >= "2000-04-01") %>%
  filter(Date < "2019-04-01" ) %>%
  group_by(site) %>%
  summarise(mean_p_gm = mean(value, na.rm = TRUE) * 365)

df_pet_gm <- read_csv(paste0(data_path, "/Climate_Data/gridmet/pet_mm_gridmet_conus_gaged_1979_2020_mean.csv")) %>%
  pivot_longer(cols =  -c('Date'), values_to = 'value', names_to= 'site') %>%
  mutate(Date = as_date(Date)) %>%
  # mutate(cy = ifelse(month(Date)>=4,year(Date)+1,year(Date))) %>%
  filter(Date >= "2000-04-01") %>%
  filter(Date < "2019-04-01" ) %>%
  group_by(site) %>%
  summarise(mean_pet_gm = mean(value, na.rm = TRUE) * 365)

climate_data_path_p <- paste0(data_path, "/Climate_Data/MWBM_PRCP_mm_all_CONUS_gages2_climgrid.csv")
df_p <- read_csv(climate_data_path_p) %>%
  dplyr::select(-`...1`) %>%
  pivot_longer(cols =  -c('date'), values_to = 'value', names_to= 'site') %>%
  # filter(site %in% !!sites) %>%
  # mutate(cy = ifelse(month(date)>=4,year(date)+1,year(date))) %>%
  filter(date >= "2000-04-01") %>%
  filter(date < "2019-04-01" ) %>%
  group_by(site) %>%
  summarise(mean_p = mean(value, na.rm = TRUE) * 12)

climate_data_path_pet <- paste0(data_path, "/Climate_Data/MWBM_PET_mm_all_CONUS_gages2_climgrid.csv")
# Read in PET data
df_pet <- read_csv(climate_data_path_pet) %>%
  dplyr::select(-`...1`) %>%
  pivot_longer(cols =  -c('date'), values_to='value', names_to= 'site') %>%
  # filter(site %in% !!sites) %>%
  # mutate(cy = ifelse(month(date)>=4,year(date)+1,year(date))) %>%
  filter(date >= "2000-04-01") %>%
  filter(date < "2019-04-01" ) %>%
  group_by(site) %>%
  summarise(mean_pet = mean(value, na.rm = TRUE) * 12)

df_aet <- read_csv(paste0(data_path, "/Climate_Data/gridmet/aet_modis_ssebop_mm_conus_gaged_2000_2020_mean.csv")) %>%
  pivot_longer(cols =  -c('Date'), values_to='value', names_to= 'site') %>%
  # filter(site %in% !!sites) %>%
  mutate(Date = as_date(Date)) %>%
  # mutate(cy = ifelse(month(Date)>=4,year(Date)+1,year(Date))) %>%
  filter(Date >= "2000-04-01") %>%
  filter(Date < "2019-04-01" ) %>%
  group_by(site) %>%
  summarise(mean_aet = mean(value, na.rm = TRUE) * 365/1000)

df_clim <- inner_join(df_p, df_aet, by = c('site')) %>%
  inner_join(df_pet, by = c('site')) %>%
  inner_join(df_p_gm, by = c('site')) %>%
  inner_join(df_pet_gm, by = c('site')) %>%
  mutate(aet_fraction = mean_aet/mean_p) %>%
  mutate(pet_fraction = mean_pet/mean_p) %>%
  mutate(aet_fraction_gm = mean_aet/mean_p_gm) %>%
  mutate(pet_fraction_gm = mean_pet_gm/mean_p_gm) %>%
  mutate(p_vs_p_gm = mean_p/mean_p_gm)

# Read in kappa values
df_kappa <- read_csv(paste0(data_path, "/single_site/kappa_long.csv"))
df_sbd <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv")) %>%
  mutate(pct_type = 'fixed')

df_metric = bind_rows(df_kappa, df_sbd) %>%
  filter(metric == "kappa") %>%
  filter(threshold == 10) %>%
  filter(pct_type == "fixed") %>%
  filter(source == "nwm")

df_both <- inner_join(df_clim, df_metric, by = 'site')


ggplot(df_both, aes(x=pet_fraction, y = aet_fraction, color = value)) + 
  geom_point() +
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 3)) +
  # scale_x_log10() + scale_y_log10() +
  scale_color_scico(palette = 'batlow',
                    direction = -1,
                    limits = c(0, 1),
                    oob = scales::squish) +
  xlab("PET/P") +
  ylab("AET/P")


hist(df_clim$aet_fraction)



