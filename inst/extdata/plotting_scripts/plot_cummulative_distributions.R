# Plot CDFs

library(formattable)
library(readr)
library(dplyr)
library(forcats)
library(ggthemes)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(RColorBrewer)
library(scico)
library(cowplot)

# Inspired by tutorial here: 
# https://haozhu233.github.io/kableExtra/awesome_table_in_html.html

# Setup data path
data_path <- "../data"

# Read in metadata
meta_df <- read_csv(paste0(data_path, "/data_from_sydney/streamflow_gages_v1_n5390.csv"))

# Read in data
df_sbd <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv"))

# Read in metric data
df_sig <- read_csv(paste0(data_path, "/single_site/ann_eval_long.csv")) %>%
  filter(metric == 'nmae') %>%
  # mutate(metric = paste0(metric, "_", measure)) %>%
  filter(measure %in% c('total_duration_below_threshold', 'total_flow_volume_below_threshold_cms', 'maximum_drought_intensity')) %>%
  # mutate(measure = ifelse(measure == 'total_duration_below_threshold', "Drought Duration", measure)) %>%
  # mutate(measure = ifelse(measure == 'total_flow_volume_below_threshold_cms', "Drought Severity", measure)) %>%
  # mutate(measure = ifelse(measure == 'maximum_drought_intensity', "Drought Intensity", measure)) %>%
  mutate(metric = measure) %>%
  arrange(metric) %>%
  ungroup() %>%
  select(-measure) %>%
  select(-mean_obs) %>%
  rename(pct_type = type)

# Read in kappa values
df_kappa <- read_csv(paste0(data_path, "/single_site/kappa_long.csv")) %>%
  filter(metric == "kappa") 

# Pulling in other metadata
df_gages_hydro <- read_csv(paste0(data_path, "/Metadata/Gages-II/gagesII_sept30_2011_conterm_hydro.csv")) %>%
  rename(site = STAID) %>%
  select(c(site, BFI_AVE))

df_gages_BasinID <- read_csv(paste0(data_path, "/Metadata/Gages-II/gagesII_sept30_2011_conterm_BasinID.csv")) %>%
  rename(site = STAID) %>%
  select(c(site, `HCDN-2009`)) %>%
  rename(HCDN = `HCDN-2009`) %>%
  mutate(HCDN= ifelse(is.na(HCDN), 'no', HCDN))

df_gages_Bas_Classif <- read_csv(paste0(data_path, "/Metadata/Gages-II/gagesII_sept30_2011_conterm_Bas_Classif.csv")) %>%
  rename(site = STAID) %>%
  select(c(site, CLASS))

climate_data_path_p <- paste0(data_path, "/Lowflow_Seasonality/Climate_Data/MWBM_PRCP_mm_all_CONUS_gages2_climgrid.csv")
climate_data_path_p <- paste0(data_path, "/Climate_Data/MWBM_PRCP_mm_all_CONUS_gages2_climgrid.csv")
df_p <- read_csv(climate_data_path_p) %>%
  dplyr::select(-`...1`) %>%
  pivot_longer(cols =  -c('date'), values_to = 'value', names_to= 'site') %>%
  # filter(site %in% !!sites) %>%
  mutate(cy = ifelse(month(date)>=4,year(date)+1,year(date))) %>%
  filter(date >= "1950-04-01") %>%
  filter(date < "2020-04-01" ) %>%
  group_by(site, cy) %>%
  summarise(mean_year_p = mean(value, na.rm = TRUE))

climate_data_path_pet <- paste0(data_path, "/Climate_Data/MWBM_PET_mm_all_CONUS_gages2_climgrid.csv")
# Read in PET data
df_pet <- read_csv(climate_data_path_pet) %>%
  dplyr::select(-`...1`) %>%
  pivot_longer(cols =  -c('date'), values_to='value', names_to= 'site') %>%
  # filter(site %in% !!sites) %>%
  mutate(cy = ifelse(month(date)>=4,year(date)+1,year(date))) %>%
  filter(date >= "1950-04-01") %>%
  filter(date < "2020-04-01" ) %>%
  group_by(site, cy) %>%
  summarise(mean_year_pet = mean(value, na.rm = TRUE))

df_clim <- left_join(df_p, df_pet, by = c("site", "cy")) %>%
  mutate(mean_year = mean_year_p/mean_year_pet)

df_aridity <- df_clim %>%
  # filter(site %in% unique(df_all$site)) %>%
  group_by(site) %>%
  summarise(aridity = mean(mean_year, na.rm = TRUE))

df_aridity <- df_aridity %>%
  mutate(site = str_pad(site, width = 8, side = 'left', pad = "0")) 

df_aridity %>%
  filter(site %in% meta_df$site) %>%
  write_csv("../data/Metadata/aridity.csv")

df_all <- bind_rows(df_kappa, df_sig, df_sbd) %>%
  mutate(threshold = as.factor(threshold)) %>%
  mutate(name = paste(source, threshold)) %>%
  mutate(name = factor(name, levels = c('nhm 5', 'nhm 10', 'nhm 20', "nhm 30", "nwm 5", "nwm 10", "nwm 20", 'nwm 30'))) %>%
  mutate(value = round(value, 2)) %>%
  left_join(df_aridity) %>%
  left_join(meta_df) %>%
  mutate(Aridity_Group=cut(aridity, breaks= c(0,0.5, 1, 2, 10))) %>%
  mutate(Aridity_Group_q=cut(percent_rank(aridity), breaks= seq(0, 1, by = 0.1))) %>%
  mutate(Aridity_Group_q2=cut(percent_rank(aridity), breaks= seq(0, 1, by = 0.2))) %>%
  left_join(df_gages_hydro) %>%
  left_join(df_gages_BasinID) %>%
  left_join(df_gages_Bas_Classif) %>%
  mutate(BFI_Group = cut(BFI_AVE, breaks = c(0, 30, 60, 100))) %>%
  mutate(BFI_Group_q=cut(percent_rank(BFI_AVE), breaks= seq(0, 1, by = 0.1))) %>%
  mutate(BFI_Group_q2=cut(percent_rank(BFI_AVE), breaks= seq(0, 1, by = 0.2))) %>%
  mutate(DA_Group_q=cut(percent_rank(drain_sqkm), breaks= seq(0, 1, by = 0.1))) %>%
  mutate(DA_Group_q2=fct_rev(cut(percent_rank(drain_sqkm), breaks= seq(0, 1, by = 0.2))))



# Split by source/pct_type
p1 <- ggplot(df_all %>% filter(metric == "kappa" & threshold == 20), aes(x = value, color = source, linetype = pct_type)) + 
  stat_ecdf(geom = "line", size = 1) +
  theme_classic() +
  scale_colour_manual(name = "Model", values = scico(2, palette = 'batlow', direction = -1)) +
  ggtitle("Kappa for different threshold types and models") +
  xlim(-0.34, 0.89) +
  xlab("Cohen's Kappa") + 
  ylab('Fraction of Sites with that score or below')

# Split by source/threshold
p2 <- ggplot(df_all %>% filter(metric == "kappa" & pct_type == "fixed"), aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa for different thresholds and models") +
  xlab("Cohen's Kappa") + 
  xlim(-0.34, 0.89) +
  ylab('Fraction of Sites with that score or below')

p3 <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(CLASS) & pct_type == 'fixed'), aes(x = value, color = CLASS, linetype = HCDN)) +
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Reference Gage", values = scico(2, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa for Reference/Non Reference Gages") +
  xlab("Cohen's Kappa") +
  xlim(-0.34, 0.89) +
  ylab('Fraction of Sites with that score or below')


p4 <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(Aridity_Group_q) & pct_type == 'fixed'), aes(x = value, color = Aridity_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Aridity Group", values = scico(5, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa for different aridity groups") +
  xlab("Cohen's Kappa") + 
  xlim(-0.34, 0.89) +
  ylab('Fraction of Sites with that score or below')

p5 <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(BFI_Group_q) & pct_type == 'fixed'), aes(x = value, color = BFI_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "BFI Group", values = scico(5, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa for different base flow index groups") +
  xlab("Cohen's Kappa") + 
  xlim(-0.34, 0.89) +
  ylab('Fraction of Sites with that score or below')

p6 <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(DA_Group_q) & pct_type == 'fixed'), aes(x = value, color = DA_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "DA Group", values = scico(5, palette = 'batlow', direction = 1)) +
  theme_classic() +
  xlim(-0.34, 0.89) +
  ggtitle("Kappa for different drainage area groups") +
  xlab("Cohen's Kappa") + 
  ylab('Fraction of Sites with that score or below')

ggdraw() +
  draw_plot(p1, x=0, y = .66, width = .5, height = .33 )  +
  draw_plot(p2, x=0, y = .33, width = .5, height = .33) + 
  draw_plot(p3, x=0, y = 0, width = 0.5, height = 0.33) + 
  draw_plot(p4, x=.5, y = .66, width = .5, height = .33) + 
  draw_plot(p5, x=.5, y = .33, width = .5, height = .33) + 
  draw_plot(p6, x=.5, y = .0, width = .5, height = .33 )  +    
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.985) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.65) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.32) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.985) +
  draw_label("e)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.65) +
  draw_label("f)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.32) 
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/drought_cdf_main.png"), bg = 'transparent', height = 10, width = 10)


type_name <- 'fixed'
df_all_plot <- filter(df_all, pct_type == type_name)

p7 <- ggplot(df_all_plot %>% filter(metric == 'rho_obs'), aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Spearman's R") +
  xlab("Spearman's R") + 
  ylab('Fraction of Sites with that score or worse')

df_all_bias <- df_all_plot %>%
  filter(metric == 'pct_bias') %>%
  mutate(value = abs(value))
  # mutate(value = ifelse(value >= 300, 300, value))

p8 <- ggplot(df_all_bias, aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  scale_x_reverse() +
  coord_cartesian(xlim=c(300, 0)) +
  
  ggtitle("Absolute Percent Bias") +
  xlab("Absolute Percent Bias") + 
  ylab('Fraction of Sites with that score or worse')

df_all_sd <- df_all_plot %>%
  filter(metric == 'sd_ratio') %>%
  mutate(value = abs(value  -1 ))
  # mutate(value = ifelse(value >= 2, 2, value))

p9 <- ggplot(df_all_sd, aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  scale_x_reverse() +
  ggtitle("SD Ratio") +
  coord_cartesian(xlim=c(2, 0)) +
  # xlim(2,0) +
  xlab("SD Ratio difference from 1") + 
  ylab('Fraction of Sites with that score or worse')


p10 <- ggplot(df_all_plot %>% filter(metric == "total_duration_below_threshold" ), aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  coord_cartesian(xlim=c(3, 0)) +
  scale_x_reverse() +
  ggtitle("Drought Duration NMAE") +
  xlab("Drought Duration") + 
  ylab('Fraction of Sites with that score or worse')

p11 <- ggplot(df_all_plot %>% filter(metric == "total_flow_volume_below_threshold_cms" ), aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  scale_x_reverse() +
  coord_cartesian(xlim=c(3, 0)) +
  ggtitle("Drought Severity NMAE") +
  xlab("Drought Severity") + 
  ylab('Fraction of Sites with that score or worse')

p12 <- ggplot(df_all_plot %>% filter(metric == 'maximum_drought_intensity' ), aes(x = value, color = threshold, linetype = source)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Threshold", values = scico(4, palette = 'batlow', direction = -1)) +
  theme_classic() +
  scale_x_reverse() +
  coord_cartesian(xlim=c(3, 0)) +
  ggtitle("Drought Intensity NMAE") +
  xlab("Drought Severity") + 
  ylab('Fraction of Sites with that score or worse')


ggdraw() +
  draw_plot(p7 + theme(legend.position = ""), x=0, y = .69, width = .5, height = .31 )  +
  draw_plot(p8 + theme(legend.position = ""), x=0, y = .38, width = .5, height = .31) + 
  draw_plot(p9 + theme(legend.position = "bottom"), x=0, y = 0.015, width = 0.5, height = 0.365) + 
  draw_plot(p10 + theme(legend.position = ""), x=.5, y = .69, width = .5, height = .31) + 
  draw_plot(p12 + theme(legend.position = ""), x=.5, y = .38, width = .5, height = .31) + 
  draw_plot(p11 + theme(legend.position = ""), x=.5, y = .07, width = .5, height = .31)  +    
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.04, y = 0.985) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.04, y = 0.68) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.04, y = 0.37) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.985) +
  draw_label("e)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.68) +
  draw_label("f)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.37) 
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/drought_cdf_other_metrics_", type_name, ".png"), bg = 'transparent', height = 11, width = 10)





# Plot all kinds of aridity. 
p4a <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(Aridity_Group_q) & pct_type == 'fixed' & source == "nwm"), aes(x = value, color = Aridity_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Aridity Group", values = scico(5, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa Fixed NWM") +
  xlab("Cohen's Kappa") + 
  ylab('Fraction of Sites with that score or below')

p4b <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(Aridity_Group_q) & pct_type == 'fixed' & source == "nhm"), aes(x = value, color = Aridity_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Aridity Group", values = scico(5, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa Fixed NHM") +
  xlab("Cohen's Kappa") + 
  ylab('Fraction of Sites with that score or below')

p4c <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(Aridity_Group_q) & pct_type == 'variable' & source == "nwm"), aes(x = value, color = Aridity_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Aridity Group", values = scico(5, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa Variable NWM") +
  xlab("Cohen's Kappa") + 
  ylab('Fraction of Sites with that score or below')

p4d <- ggplot(df_all %>% filter(metric == 'kappa' & threshold == 20 & !is.na(Aridity_Group_q) & pct_type == 'variable' & source == "nhm"), aes(x = value, color = Aridity_Group_q2)) + 
  stat_ecdf(geom = "line", size = 1) +
  scale_colour_manual(name = "Aridity Group", values = scico(5, palette = 'batlow', direction = -1)) +
  theme_classic() +
  ggtitle("Kappa Variable NHM") +
  xlab("Cohen's Kappa") + 
  ylab('Fraction of Sites with that score or below')

ggdraw() +
  draw_plot(p4a, x=0, y = .5, width = .5, height = .5) + 
  draw_plot(p4b, x=0, y = 0, width = 0.5, height = 0.5) + 
  draw_plot(p4c, x=.5, y = .5, width = .5, height = .5) + 
  draw_plot(p4d, x=.5, y = .0, width = .5, height = .5 )  +    
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.985) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.485) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.985) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.485)
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/drought_cdf_aridity.png"), bg = 'transparent', height = 6.66, width = 10)
