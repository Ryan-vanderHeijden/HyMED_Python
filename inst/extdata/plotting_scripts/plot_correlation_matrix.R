# Plot Correlation Matrix

library(dplyr)
library(tidyr)
library(readr)
library(corrplot)
require(ggplot2) #required packages
require(cowplot)
require(magick)


data_path <- "../data"


plot_correlation_matrix <- function(data_path, thresh, source_name, type, sig_metric){

  df_sbd <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv"))  %>%
    filter(threshold == !!thresh) %>%
    filter(source == !!source_name) %>%
    select(-threshold) %>%
    select(-source) %>%
    pivot_wider(names_from = c(metric)) %>%
    select(-c(nse, bias))
  
  df_kappa <- read_csv(paste0(data_path, "/single_site/kappa_long.csv")) %>%
    filter(threshold == !!thresh) %>%
    filter(source == !!source_name) %>%
    filter(pct_type == !!type) %>%
    filter(metric == 'kappa') %>%
    select(-threshold) %>%
    select(-source) %>%
    select(-pct_type) %>%
    pivot_wider(names_from = c(metric))
  
  df_at <- read_csv(paste0(data_path, "/single_site/ann_eval_long.csv")) %>%
    filter(threshold == !!thresh) %>%
    filter(type == !!type) %>%
    filter(source == !!source_name) %>%
    filter(metric == !!sig_metric) %>%
    filter(measure %in% c("total_duration_below_threshold", "total_flow_volume_below_threshold_cms", "maximum_drought_intensity")) %>%
    select(-threshold) %>%
    select(-source) %>%
    select(-type) %>%
    select(-mean_obs) %>%
    select(-metric) %>% 
    pivot_wider(names_from = c(measure)) %>%
    rename(drought_deficit = total_flow_volume_below_threshold_cms) %>%
    rename(drought_duration = total_duration_below_threshold) %>%
    rename(drought_intensity = maximum_drought_intensity)
  
  df <- full_join(df_kappa, df_sbd, by = 'site') %>%
    full_join(df_at) %>%
    select(-site) %>%
    rename(Kappa = kappa,
           `Percent Bias` = pct_bias,
           `Ratio of SD` = sd_ratio,
           `Spearmans R` = rho_obs,
           `Drought Intensity` = drought_intensity,
           `Drought Duration` = drought_duration,
           `Drought Deficit` = drought_deficit) %>% 
    mutate(across(where(is.numeric), \(x) ifelse(is.nan(x), NA, x)))
  df[sapply(df, is.infinite)] <- NA
  
  res <- cor(df, use = "pairwise.complete.obs")
  
  # corrplot(res, order = "hclust", 
  #          tl.col = "black", tl.srt = 45)
  corrplot(res, order = "original", 
           tl.col = "black", tl.srt = 45, type = 'upper')
  p <- recordPlot()
  
  png(paste0(data_path, "/Figures/Correlation/Corr_Matrix_", source_name, "_", thresh, "_", type, "_", sig_metric, ".png")) 
  corrplot(res, order = "original", 
           tl.col = "black", tl.srt = 45, type = 'upper')
  dev.off() 
  
  
  png(paste0(data_path, "/Figures/Correlation/Corr_Matrix_W_Values_", source_name, "_", thresh, "_", type, "_", sig_metric, ".png")) 
  corrplot(res, order = "original", addCoef.col="grey",
           tl.col = "black", tl.srt = 45, type = 'upper')
  dev.off() 
  
  return(p)
}

this_stat <- 'nmae'
p_1 <- plot_correlation_matrix(data_path, 20, "nhm", 'variable', this_stat)
p_2 <- plot_correlation_matrix(data_path, 20, "nwm", 'variable', this_stat)

p_3 <- plot_correlation_matrix(data_path, 20, "nhm", 'fixed', this_stat)
p_4 <- plot_correlation_matrix(data_path, 20, "nwm", 'fixed', this_stat)

p_5 <- plot_correlation_matrix(data_path, 5, "nhm", 'variable', this_stat)
p_6 <- plot_correlation_matrix(data_path, 5, "nwm", 'variable', this_stat)

ggdraw() +   
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nhm_20_variable_nmae.png"),
             y = 0.0, x = -0.25, scale = 1.15) +
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nwm_20_variable_nmae.png"),
             y = 0.0, x = 0.25, scale = 1.15) +
  draw_label("a)", colour = "black", size = 14, angle = 0, x = 0.03, y = 0.98) +
  draw_label("b)", colour = "black", size = 14, angle = 0, x = 0.53, y = 0.98) 
ggsave(paste0(data_path, "/Figures/Correlation/Corr_Matrix_Figure.png"), width = 8, height = 4)

ggdraw() +   
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nhm_20_variable_nmae.png"),
             y = 0.33, x = -0.25, scale = .5) +
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nwm_20_variable_nmae.png"),
             y = 0.33, x = 0.25, scale = .5) +
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nhm_5_variable_nmae.png"),
             y = 0.0, x = -0.25, scale = .5) +
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nwm_5_variable_nmae.png"),
             y = 0.0, x = 0.25, scale = .5) +
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nhm_20_fixed_nmae.png"),
             y = -0.33, x = -0.25, scale = .5) +
  draw_image(paste0(data_path, "/Figures/Correlation/Corr_Matrix_nwm_20_fixed_nmae.png"),
             y = -0.33, x = 0.25, scale = .5) +
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.98) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.53, y = 0.98) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.65) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.53, y = 0.65) +
  draw_label("e)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.32) +
  draw_label("f)", colour = "black", size = 12, angle = 0, x = 0.53, y = 0.32) 
ggsave(paste0(data_path, "/Figures/Correlation/Corr_Matrix_Figure_SI.png"), width = 6, height = 8)



source_name <- 'nwm'
thresh <- 20

df_sbd <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv"))  %>%
  filter(threshold == !!thresh) %>%
  filter(source == !!source_name) %>%
  select(-threshold) %>%
  select(-source) %>%
  pivot_wider(names_from = c(metric)) %>%
  select(-c(nse, bias))

ggplot(df_sbd, aes(x=pct_bias, y = sd_ratio)) +
  geom_point(alpha = 0.25) +
  xlim(-100, 500) +
  ylim(0, 5)
