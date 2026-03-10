# Plot all scatter plots

# Import packages
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(RColorBrewer)
library(usmap)
library(cowplot)
library(scico)

# Setup data path
data_path <- "../data"

# Read in metadata
meta_df <- read_csv(paste0(data_path, "/data_from_sydney/streamflow_gages_v1_n5390.csv"))

# Read in data
df_sbd <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv"))  %>%
  left_join(meta_df)

df_sum_sbd <- df_sbd %>%
  filter(!is.infinite(value)) %>%
  group_by(threshold, source, metric, pct_type) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

# Read in metric data
df_sig <- read_csv(paste0(data_path, "/single_site/ann_eval_long.csv")) %>%
  left_join(meta_df)

# Read in kappa values
df_kp <- read_csv(paste0(data_path, "/single_site/kappa_long.csv")) %>%
  left_join(meta_df,by = 'site')

# Setup function
plot_sbd_scatter <- function(df,  thresh, metric_name, x_lims, pct_type_name){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(metric == !!metric_name) %>%
    filter(threshold == !!thresh) %>%
    filter(pct_type == pct_type_name) %>%
    # arrange(flow_cluster) %>%
    filter(value <= max(x_lims)) %>%
    pivot_wider(names_from = source, values_from = value) 
  
  df_plot <- df_plot[!is.na(df_plot$nhm),]
  df_plot <- df_plot[!is.na(df_plot$nwm),]
  df_plot <- df_plot[!is.infinite(df_plot$nhm),]
  df_plot <- df_plot[!is.infinite(df_plot$nwm),]
  
  # R2 <- cor(df_plot$nwm, df_plot$nhm) ^ 2
  spear <- cor.test(df_plot$nwm, df_plot$nhm, method = 'spearman', exact = FALSE)
  
  p <- ggplot(df_plot, aes(x=nwm, y=nhm)) + 
    geom_point(alpha = 0.25) +
    theme_classic() +
    xlim(x_lims) +
    ylim(x_lims) +
    geom_abline(slope = 1, intercept = 0) +
    geom_text(x=max(x_lims)/3-.2, y=max(x_lims) * .85, label=paste0("Spearman's R=", round(spear$estimate, 2)),
              size=5, fontface="italic")
  p
  
  return(p)
}


# Setup function
plot_sig_scatter <- function(df,  measure_name, thresh, type, x_lims){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(metric == 'nmae') %>%
    filter(threshold == !!thresh) %>%
    # arrange(flow_cluster) %>%
    filter(value <= max(x_lims)) %>%
    filter(value >= min(x_lims)) %>%
    filter(measure == !!measure_name) %>%
    filter(type == !!type) %>%
    pivot_wider(names_from = source, values_from = value) 
  
  
  df_plot <- df_plot[!is.na(df_plot$nhm),]
  df_plot <- df_plot[!is.na(df_plot$nwm),]
  df_plot <- df_plot[!is.infinite(df_plot$nhm),]
  df_plot <- df_plot[!is.infinite(df_plot$nwm),]
  
  # R2 <- cor(df_plot$nwm, df_plot$nhm) ^ 2
  spear <- cor.test(df_plot$nwm, df_plot$nhm, method = 'spearman', exact = FALSE)
  
  p <- ggplot(df_plot, aes(x=nwm, y=nhm)) + 
    geom_point(alpha = 0.1) +
    theme_classic() +
    xlim(x_lims) +
    ylim(x_lims) +
    geom_abline(slope = 1, intercept = 0) +
    geom_text(x=max(x_lims)/3, y=max(x_lims) * .85, label=paste0("Spearman's R=", round(spear$estimate, 2)),
              size=5, fontface="italic")
  p
  
  return(p)
}



# Setup function
plot_kappa_scatter <- function(df,  thresh, type){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(pct_type == !!type) %>%
    filter(threshold == !!thresh) %>%
    filter(metric == 'kappa') %>%
    # arrange(flow_cluster) %>%
    pivot_wider(names_from = source, values_from = value)
  
  # R2 <- cor(df_plot$nwm, df_plot$nhm) ^ 2
  spear <- cor.test(df_plot$nwm, df_plot$nhm, method = 'spearman', exact = FALSE)
  
  p <- ggplot(df_plot, aes(x=nwm, y=nhm)) + 
    geom_point(alpha = 0.25) +
    theme_classic() +
    xlim(-.3, 1) +
    ylim(-.3, 1) +
    geom_abline(slope = 1, intercept = 0) +
    geom_text(x=0.1, y=0.85, label=paste0("Spearman's R=", round(spear$estimate, 2)),
              size=5, fontface="italic")
  p
  
  return(p)
}

p1_kp <- plot_kappa_scatter(df_kp, 20,  "variable") 
p2_kp <- plot_kappa_scatter(df_kp, 20, "fixed") 


# Setup plots for combined figure
p1_sig <- plot_sig_scatter(df_sig,"total_duration_below_threshold", 20, "variable", c(0, 4)) 
p2_sig <- plot_sig_scatter(df_sig,"maximum_drought_intensity", 20, "variable", c(0, 4)) 
p3_sig<- plot_sig_scatter(df_sig,"total_flow_volume_below_threshold_cms", 20, "variable", c(0, 4)) 


p4_sig <- plot_sig_scatter(df_sig,"total_duration_below_threshold", 20, "fixed", c(0, 4)) 
p5_sig <- plot_sig_scatter(df_sig,"maximum_drought_intensity", 20, "fixed", c(0, 4)) 
p6_sig<- plot_sig_scatter(df_sig,"total_flow_volume_below_threshold_cms", 20, "fixed", c(0, 4)) 



p1_sbd <- plot_sbd_scatter(df_sbd, 20,  "rho_obs", c(-.5,1), 'fixed') 
p2_sbd <- plot_sbd_scatter(df_sbd, 20,  "pct_bias", c(-100,500), 'fixed') 
p3_sbd <- plot_sbd_scatter(df_sbd, 20,  "sd_ratio", c(0,5), 'fixed') 

p4_sbd <- plot_sbd_scatter(df_sbd, 20,  "rho_obs", c(-.5,1), 'variable') 
p5_sbd <- plot_sbd_scatter(df_sbd, 20,  "pct_bias", c(-100,500), 'variable') 
p6_sbd <- plot_sbd_scatter(df_sbd, 20,  "sd_ratio", c(0,5), 'variable') 


# Make combined figure. 
ggdraw() +
  draw_plot(p1_kp, x=0, y = .75, width = .25, height = .25)  +
  draw_plot(p2_kp, x=0.25, y = .75, width = .25, height = .25) + 
  
  draw_plot(p1_sbd, x=.25, y = .5, width = .25, height = .25) + 
  draw_plot(p2_sbd, x=.25, y = .25, width = .25, height = .25) + 
  draw_plot(p3_sbd, x=.26, y = .0, width = .25, height = .25 )  +  
  
  draw_plot(p4_sbd, x=.0, y = .5, width = .25, height = .25) + 
  draw_plot(p5_sbd, x=.0, y = .25, width = .25, height = .25) + 
  draw_plot(p6_sbd, x=.01, y = .0, width = .25, height = .25 )  +  

  draw_plot(p1_sig, x=.5, y = 0.5, width = .25, height = .25) + 
  draw_plot(p2_sig, x=.5, y = 0.25, width = .25, height = .25)  +                            
  draw_plot(p3_sig, x=.5, y = 0, width = .25, height = .25) + 
  
  draw_plot(p4_sig, x=.75, y = .5, width = .25, height = .25) + 
  draw_plot(p5_sig, x=.75, y = .25, width = .25, height = .25)  +                            
  draw_plot(p6_sig, x=.75, y = 0, width = .25, height = .25) +

  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.05, y = 0.99) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.05, y = 0.74) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.05, y = 0.49) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.05, y = 0.25) +
  
  draw_label("e)", colour = "black", size = 12, angle = 0, x = 0.3, y = 0.99) +
  draw_label("f)", colour = "black", size = 12, angle = 0, x = 0.3, y = 0.74) +
  draw_label("g)", colour = "black", size = 12, angle = 0, x = 0.3, y = 0.49) +
  draw_label("h)", colour = "black", size = 12, angle = 0, x = 0.3, y = 0.25) +
  
  # draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.05, y = 0.99) +
  draw_label("i)", colour = "black", size = 12, angle = 0, x = 0.55, y = 0.74) +
  draw_label("j)", colour = "black", size = 12, angle = 0, x = 0.55, y = 0.49) +
  draw_label("k)", colour = "black", size = 12, angle = 0, x = 0.55, y = 0.25) +
  
  # draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.05, y = 0.99) +
  draw_label("l)", colour = "black", size = 12, angle = 0, x = 0.8, y = 0.74) +
  draw_label("m)", colour = "black", size = 12, angle = 0, x = 0.8, y = 0.49) +
  draw_label("n)", colour = "black", size = 12, angle = 0, x = 0.8, y = 0.25) +

  draw_label("Cohen's Kappa Variable", colour = "black", size = 14, x = 0.16, y = 0.99) +
  draw_label("Cohen's Kappa Fixed", colour = "black", size = 14, x = 0.4, y = 0.99) +
  draw_label("Error Components Fixed", colour = "black", size = 14, x = 0.40, y = 0.74) +
  draw_label("Error Components Variable", colour = "black", size = 14, x = 0.16, y = 0.74) +
  draw_label("Drought Signatures Variable", colour = "black", size = 14, x = 0.67, y = 0.74) +
  draw_label("Drought Signatures Fixed", colour = "black", size = 14, x = 0.9, y = 0.74)
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/drought_scatter.png"), bg = 'transparent', height = 13, width = 14)
