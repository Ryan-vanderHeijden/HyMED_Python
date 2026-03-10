# Plot Scorecard

# Import packages
library(kableExtra)
library(formattable)
library(readr)
library(dplyr)
library(prettyprint)
library(forcats)
library(webshot)
library(ggthemes)
library(tidyr)
library(ggplot2)
library(scico)
library(cowplot)

# Set data path
data_path <- "../data"

# Inspired by tutorial here: 
# https://haozhu233.github.io/kableExtra/awesome_table_in_html.html

# Setup output table
metric <- c("pct_bias", "rho_obs", "sd_ratio", "nse", 'bias')
best_value <- c(0, 1, 0, 1, 0)
worst_value <- c(100, 0, 1, -1, 1)

# Setup table
df_met <- as.data.frame(metric) %>%
  mutate(best = best_value, 
         worst = worst_value)

# Setup Spearman's, Bias, Dist dataframe. 
df <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv")) %>%
  mutate(value = replace(value, is.infinite(value), NA)) %>%
  mutate(value = ifelse(metric %in% c("bias", "pct_bias"), abs(value), value)) %>%
  mutate(value = ifelse(metric == 'sd_ratio', abs(value - 1), value)) %>%
  group_by(source, threshold, metric, pct_type) %>%
  summarise(median_value = as.numeric(round(median(value, na.rm = TRUE), digits = 3))) %>%
  arrange(source, metric) %>%
  filter(metric %in% c("rho_obs", "pct_bias", "sd_ratio")) %>% 
  ungroup() %>%
  select(source, metric, threshold, pct_type, median_value) %>%
  left_join(df_met, by = 'metric') %>%
  mutate(scale_val = (best-median_value)/(best-worst)) %>%
  # mutate(scale_val = ifelse(scale_val < 0, 0, scale_val)) %>%
  select(-c(best, worst)) %>%
  pivot_wider(names_from = source, values_from = c(median_value, scale_val)) %>%
  rename(nhm = median_value_nhm,
         nwm = median_value_nwm)

df_spear <- df %>% filter(metric == 'rho_obs')
df_bias <- df %>% filter(metric == 'pct_bias') %>%
  mutate(nhm = round(nhm, 1)) %>%
  mutate(nwm = round(nwm, 1))

df_dist <- df %>% filter(metric == 'sd_ratio')

df_sbd <- bind_rows(df_spear, df_bias, df_dist) %>%
  mutate(scale_val_nhm = 1- scale_val_nhm,
         scale_val_nwm = 1- scale_val_nwm)


# Setup dataframe for kappa. 
df_kappa <- read_csv(paste0(data_path, "/single_site/kappa_long.csv")) %>%
  filter(metric == "kappa") %>%
  group_by(source, threshold, metric, pct_type) %>%
  summarise(median_value = as.numeric(median(value, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_wider(names_from = source, values_from = median_value) %>%
  arrange(metric) %>%
  mutate(scale_val_nhm = nhm, 
         scale_val_nwm = nwm) 
  

# Setup annual attributes table. 
df_ann_eval <- read_csv(paste0(data_path, "/single_site/ann_eval_long.csv")) %>%
  filter(metric == 'nmae') %>%
  mutate(metric = paste0(metric, "_", measure)) %>%
  group_by(metric, source, threshold, measure, type) %>%
  summarise(median_value = as.numeric(median(value, na.rm = TRUE))) %>%
  mutate(color_val = ifelse(median_value < 0, 0, median_value)) %>%
  pivot_wider(names_from = source, values_from = c(median_value, color_val)) %>%
  rename(nhm = median_value_nhm,
         nwm = median_value_nwm) %>%
  filter(measure %in% c('total_duration_below_threshold', 'total_flow_volume_below_threshold_cms', 'maximum_drought_intensity')) %>%
  mutate(measure = ifelse(measure == 'total_duration_below_threshold', "Drought Duration", measure)) %>%
  mutate(measure = ifelse(measure == 'total_flow_volume_below_threshold_cms', "Drought Deficit", measure)) %>%
  mutate(measure = ifelse(measure == 'maximum_drought_intensity', "Drought Intensity", measure)) %>%
  arrange(factor(measure, levels = c("Drought Duration", "Drought Deficit", "Drought Intensity"))) %>%
  arrange(metric) %>%
  ungroup() %>%
  mutate(scale_val_nhm = 1- color_val_nhm,
         scale_val_nwm = 1- color_val_nwm) %>%
mutate(scale_val_nhm = ifelse(scale_val_nhm < 0, 0, scale_val_nhm),
       scale_val_nwm = ifelse(scale_val_nwm < 0, 0, scale_val_nwm)) %>%
  select(-c(measure, color_val_nhm, color_val_nwm)) %>%
  rename(pct_type = type)


df_all <- bind_rows(df_sbd, df_kappa, df_ann_eval) %>%
  mutate(threshold = as.factor(threshold)) %>%
  pivot_longer(cols = c(nhm, nwm), names_to = 'model', values_to = 'value') %>%
  mutate(scale_val = ifelse(model == 'nhm', scale_val_nhm, scale_val_nwm)) %>%
  select(-c(scale_val_nhm, scale_val_nwm)) %>%
  mutate(name = paste(model, threshold)) %>%
  mutate(metric = factor(metric, levels = rev(c("kappa", "rho_obs", "pct_bias", "sd_ratio", 
                                                "nmae_total_duration_below_threshold",
                                                "nmae_maximum_drought_intensity", 
                                                "nmae_total_flow_volume_below_threshold_cms")))) %>%
  mutate(name = toupper(name)) %>%
  mutate(name = factor(name, levels = c('NHM 5', 'NHM 10', 'NHM 20', "NHM 30", "NWM 5", "NWM 10", "NWM 20", 'NWM 30'))) %>%
  mutate(value = round(value, 2)) %>%
  mutate(pct_type = ifelse(pct_type == 'fixed', 'Fixed Drought', "Variable Drought"))

  # arrange(model, threshold)

myLoc <- 
  (which(levels(df_all$name) == "NHM 30") +
     which(levels(df_all$name) == "NWM 5")) / 
  2

p <- ggplot(df_all, aes(x = name, y = metric, fill = scale_val, group = model)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), limits = c(0,1)) +
  # scale_fill_scico(palette = 'batlow', direction = -1) +
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(angle=45, vjust=0, hjust=0)) +
  scale_x_discrete(position = "top") +
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE)) +
  xlab("") + 
  ylab("") +
  scale_y_discrete(labels=c("kappa" = "Cohen's Kappa", 
                            "rho_obs" = "Spearman's R", 
                            "pct_bias" = "Absolute Percent Bias", 
                            "sd_ratio" = "Ratio of Standard Deviations\nDifference from 1",  
                            "nmae_total_duration_below_threshold" = "Drought Duration NMAE", 
                            "nmae_maximum_drought_intensity" = "Drought Intensity NMAE", 
                            "nmae_total_flow_volume_below_threshold_cms" = "Drought Severity NMAE"  
                            )) +

  facet_grid(rows = vars(pct_type)) +
  geom_vline(xintercept=myLoc, linewidth = 1) +
  labs(fill='') 

# Make combined figure. 
ggdraw() +
  draw_plot(p, x=0, y = 0, width = 1, height = 1 )  +                   
  draw_label("Better\nPerformance", colour = "black", size = 8, angle = 0, x = 0.95, y = 0.56) +
  draw_label("Worse\nPerformance", colour = "black", size = 8, angle = 0, x = 0.95, y = 0.26)

ggsave(paste0(data_path, "/Figures/Scorecard_all.png"), bg = 'transparent', height = 5, width = 7)

