# Plot maps of Spearman's, bias and distributional errors. 

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
meta_df <- read_csv(paste0(data_path, "/Metadata/Metadata_w_clusters.csv")) 

df <- read_csv(paste0(data_path, "/single_site/spear_bias_dist_long.csv"))  %>%
  left_join(meta_df,by = 'site') %>%
  filter(!is.na(flow_cluster))

df_sum <- df %>%
  filter(!is.infinite(value)) %>%
  group_by(threshold, source, metric, pct_type) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

# Setup map. 
us <- map_data("state")
us <- fortify(us, region="region")
my.palette <- brewer.pal(n=10, name = 'RdYlBu')

# Define function to plot maps
plot_map <- function(df, metric_name, source_name, thresh, pct_type_name){
  
  # Set different limits for different metric names. 
  if (metric_name == "rho_obs"){min_val = 0; max_val = 1; p_dir = -1; ramp = 'batlow'}
  # if (metric_name == "bias"){min_val = -2; max_val = 1; p_dir = 1; ramp = 'bam'}
  if (metric_name == "pct_bias"){min_val = -200; max_val = 200; p_dir = 1; ramp = 'roma'}
  # if (metric_name == "nse"){min_val = -2; max_val = 2; p_dir = -1; ramp = 'imola'}
  if (metric_name == "sd_ratio"){min_val = 0; max_val = 2; p_dir = 1; ramp = 'roma'}
  
  # subset data for plot
  df_plot <- filter(df, threshold == thresh) %>%
    filter(metric == metric_name) %>%
    filter(source == source_name) %>%
    filter(pct_type == pct_type_name)
  df_plot <- mutate(df_plot, value = ifelse(is.infinite(value), NA, value)) %>%
    filter(!is.na(value))
  
  # Generate plot
  p1 <- ggplot() + 
    geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region, group=group),
             fill="#ffffff", color="#7f7f7f", size=0.5) +
    # plot_usmap(include = c('CO', 'UT')) +
    geom_point(data=df_plot, aes(x=long, y=lat, color=value),
               size=1) + 
    coord_map("albers", lat0=39, lat1=45)+ 
    theme_map() +
    # scale_colour_gradientn(colours = my.palette,
    #                        limits = c(min_val, max_val), 
    #                        oob = scales::squish) +
    scale_color_scico(palette = ramp,
                      direction = p_dir,
                      limits = c(min_val, max_val), 
                      oob = scales::squish) +
    theme(legend.position = c(0.02, 0.55), plot.margin=unit(c(-25,-20,-25,0),"mm"), legend.title = element_text(size=12)) + 
    labs(color = paste0(metric_name, " ", source_name, ' ', thresh)) 
    # theme(legend.position = c(0.05, 0.3), plot.margin=unit(c(0,0,0,0),"mm")) + 
    # labs(color = '', title = paste0("   ", metric_name, " ", source_name, ":", thresh),
    #      subtitle = paste0("    Median:", round(median(df_plot$value, na.rm = TRUE), 1)))
  
  
  df_plot <- df_plot %>%
    mutate(value = ifelse(value < min_val, min_val, value)) %>%
    mutate(value = ifelse(value > max_val *2, max_val * 2, value))
  
  p2 <- ggplot(data = df_plot) +
    geom_histogram(aes(x = value), bins = 30) +
    ylab("") +
    xlab("") +
    # xlim(min_val, max_val) +
    theme_classic() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  Main_Plot <- ggdraw() +
    draw_plot(p2, x= .0, y = -.05, width = 0.4, height = .3 ) +
    draw_plot(p1, x= .0, y = .0, width = 1, height = 1 )
  
  # Save plot
  ggsave(paste0(data_path, "/Figures/spear_bias_dist/All_Maps/drought_", metric_name, "_", source_name, "_", thresh, ".png"), plot=Main_Plot, bg = 'transparent', height = 8, width = 10)

  Main_Plot <- ggdraw() +
    draw_plot(p2, x= .0, y = -.05, width = 0.4, height = .3 ) +
    draw_plot(p1  + theme(legend.title =element_blank()), x= .0, y = .0, width = 1, height = 1 )
  
  return(p1)
}


# Setup function
plot_sbd_box <- function(df,  thresh, metric_name, x_lims, run_abs, goal, pct_type_name){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(metric== !!metric_name) %>%
    filter(threshold == !!thresh) %>%
    filter(pct_type == pct_type_name) %>%
    # filter(source == source_name) %>%
    arrange(flow_cluster) %>%
    mutate(source = ifelse(source == "nhm", 'NHM', 'NWM'))
  
  df_plot$cluster_name <- factor(df_plot$cluster_name, levels=rev(c('1-Northeast', '2-Northern Mid-Atlantic', '3-Southern Mid-Atlantic',
                                                                "4-Southeast", '5-Central plains and forests', 
                                                                '6-Northern central', '7-Central plains', '8-Rocky Mountains',
                                                                '9-Southwest', '10-South central', '11-California and interior west',
                                                                '12-Northwest')))
  
  if (run_abs == 'yes'){
    if (metric_name == 'sd_ratio'){
      df_plot$value <- abs(df_plot$value - 1)
    } else {
      df_plot$value <- abs(df_plot$value)
    }
  }
  
  df_plot <- df_plot %>%
    mutate(value = ifelse(value < min(x_lims), min(x_lims), value)) %>%
    mutate(value = ifelse(value > max(x_lims), max(x_lims), value))
  
  median_value <- median(df_plot$value, na.rm = TRUE)

  p <- ggplot(df_plot, aes(x=cluster_name, y=value, color = source)) + 
    geom_boxplot() +
    geom_hline(yintercept = median_value, linetype = 'dashed') +
    # geom_hline(yintercept = goal, linetype = 'dashed', color = 'green') +
    coord_flip() +
    theme_classic() +
    ylim(x_lims) + 
    theme (legend.position = "bottom")
  
  p
  
  return(p)
}


# Make plots for combined figure
p1 <- plot_map(df, "rho_obs", "nhm", 20, 'fixed')
p2 <- plot_map(df, "pct_bias", "nhm", 20, 'fixed')
p3 <- plot_map(df, "sd_ratio", "nhm", 20, 'fixed')
p4 <- plot_map(df, "rho_obs", "nwm", 20, 'fixed')
p5 <- plot_map(df, "pct_bias", "nwm", 20, 'fixed')
p6 <- plot_map(df, "sd_ratio", "nwm", 20, 'fixed')


# Setup plots for combined figure
p1_box <- plot_sbd_box(df, 20, "rho_obs", c(-.5, 1), run_abs = 'no', 1, 'fixed') 
p2_box <- plot_sbd_box(df, 20, "pct_bias", c(-100, 500), run_abs = 'no', 0, 'fixed') 
# p3_box <- plot_sbd_box(df, 20, "pct_bias", c(0, 500), run_abs = 'yes', 0, 'fixed') 
p4_box <- plot_sbd_box(df, 20, "sd_ratio", c(0, 5), run_abs = 'no', 1, 'fixed') 
# p5_box <- plot_sbd_box(df, 20, "sd_ratio", c(0, 5), run_abs = 'yes', 0, 'fixed') # This needs to be addressed as optimal value is 0


# Generate combined figure
ggdraw() +
  draw_plot(p1_box + theme(title=element_blank(), legend.position = 'top', axis.text=element_text(size=10), 
                           axis.text.y = element_text(angle=-30, vjust=1, hjust=1)), 
            x=.675, y = .62, width = .32, height = 0.37)  +                            
  draw_plot(p2_box + theme(title=element_blank(), legend.position = 'none', axis.text=element_text(size=10), 
                           axis.text.y = element_text(angle=-30, vjust=1, hjust=1)), 
            x=.675, y = .31, width = .32, height = 0.31) +
  draw_plot(p4_box  + theme(title=element_blank(), legend.position = 'none', axis.text=element_text(size=10), 
                            axis.text.y = element_text(angle=-30, vjust=1, hjust=1)), 
            x=0.675, y = .0, width = .32, height = 0.31) +
  draw_plot(p1 + theme(legend.title =element_blank(), legend.position = c(0.16, 0.14)), x=-0.04, y = .67, width = .43, height = 0.32 )  +                            
  draw_plot(p2 + theme(legend.title =element_blank(), legend.position = c(0.16, 0.14)), x=-0.04, y = .35, width = .43, height = 0.32) +
  draw_plot(p3 + theme(legend.title =element_blank(), legend.position = c(0.16, 0.14)), x=-0.04, y = .03, width = .43, height = 0.32) +
  draw_plot(p4+ theme(legend.position = 'none')    , x=.3, y = .67, width = .43, height = 0.32 )  +                            
  draw_plot(p5 + theme(legend.position = 'none'), x=.3, y = .35, width = .43, height = 0.32) +
  draw_plot(p6 + theme(legend.position = 'none'), x=0.3, y = .03, width = .43, height = 0.32) +
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.98) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.65) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.32) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.37, y = 0.98) +
  draw_label("e)", colour = "black", size = 12, angle = 0, x = 0.37, y = 0.65) +
  draw_label("f)", colour = "black", size = 12, angle = 0, x = 0.37, y = 0.32) +
  draw_label("g)", colour = "black", size = 12, angle = 0, x = 0.67, y = 0.98) +
  draw_label("h)", colour = "black", size = 12, angle = 0, x = 0.67, y = 0.65) +
  draw_label("i)", colour = "black", size = 12, angle = 0, x = 0.67, y = 0.32) +
  draw_label("NHM", colour = "black", size = 14, x = 0.18, y = 0.96) +
  draw_label("NWM", colour = "black", size = 14, x = 0.51, y = 0.96) +
  draw_label("Spearman's R", colour = "black", size = 14, x = 0.01, y = 0.81, angle = 90) +
  draw_label("Percent Bias", colour = "black", size = 14, x = 0.01, y = 0.49, angle = 90) +
  draw_label("SD Ratio", colour = "black", size = 14, x = 0.01, y = 0.17, angle = 90)
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/spear_bias_dist/drought_combined_figure.png"), bg = 'transparent', height = 10.5, width = 14)



make_combined_plot <- function(metric_name, thresh, pct_type){
  nhm_plot <- plot_map(df, metric_name, "nhm", thresh, pct_type)
  nwm_plot <- plot_map(df, metric_name, "nwm", thresh, pct_type)
  
  if (metric_name == "rho_obs"){
    box_plot <- plot_sbd_box(df, thresh, metric_name, c(-.5, 1), run_abs = 'no', 1, pct_type)}
  if (metric_name == "pct_bias"){
    box_plot <- plot_sbd_box(df, thresh, metric_name, c(-100, 500), run_abs = 'no', 0, pct_type)}
  if (metric_name == "sd_ratio"){
    box_plot <- plot_sbd_box(df, thresh, metric_name, c(0, 5), run_abs = 'no', 1, pct_type)}

  ggdraw() +
    draw_plot(box_plot + theme(title=element_blank(), legend.position = 'top', axis.text=element_text(size=10), 
                             axis.text.y = element_text(angle=-30, vjust=1, hjust=1)), 
              x=.5, y = .0, width = .5, height = 1)  +                            
    draw_plot(nhm_plot + theme(legend.title =element_blank(), legend.position = c(0.16, 0.16)), x=-0.06, y = .5, width = .65, height = 0.5)  +                            
    draw_plot(nwm_plot + theme(legend.position = 'none') , x=-0.06, y = 0, width = .65, height = 0.5) +
    # draw_label(legend_name, colour = "black", size = 14, x = 0.5, y = 0.96) +
    draw_label("NHM", colour = "black", size = 14, x = 0.01, y = 0.75, angle = 90) +
    draw_label("NWM", colour = "black", size = 14, x = 0.01, y = 0.25, angle = 90) +
    draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.98) +
    draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.48) +
    draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.55, y = 0.98) 
  ggsave(paste0(data_path, "/Figures/spear_bias_dist/simpler_figure_",metric_name, "_", pct_type, "_", thresh, ".png"), bg = 'transparent', height = 7, width = 10)
}

make_combined_plot("rho_obs", 20, 'fixed')
make_combined_plot("sd_ratio", 20, 'fixed')
make_combined_plot("pct_bias", 20, 'fixed')

make_combined_plot("rho_obs", 20, 'variable')
make_combined_plot("sd_ratio", 20, 'variable')
make_combined_plot("pct_bias", 20, 'variable')





# Setup function
plot_sbd_scatter <- function(df,  thresh, metric_name, x_lims){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(metric == !!metric_name) %>%
    filter(threshold == !!thresh) %>%
    arrange(flow_cluster) %>%
    filter(value <= max(x_lims)) %>%
    pivot_wider(names_from = source, values_from = value) 
  
  df_plot <- df_plot[!is.na(df_plot$nhm),]
  df_plot <- df_plot[!is.na(df_plot$nwm),]
  df_plot <- df_plot[!is.infinite(df_plot$nhm),]
  df_plot <- df_plot[!is.infinite(df_plot$nwm),]
  
  df_plot$cluster_name <- factor(df_plot$cluster_name, levels=c('1-Northeast', '2-Northern Mid-Atlantic', '3-Southern Mid-Atlantic',
                                                                "4-Southeast", '5-Central plains and forests', 
                                                                '6-Northern central', '7-Central plains', '8-Rocky Mountains',
                                                                '9-Southwest', '10-South central', '11-California and interior west',
                                                                '12-Northwest'))

  R2 <- cor(df_plot$nwm, df_plot$nhm) ^ 2
  
  
  p <- ggplot(df_plot, aes(x=nwm, y=nhm)) + 
    geom_point(alpha = 0.25) +
    theme_classic() +
    xlim(x_lims) +
    ylim(x_lims) +
    geom_abline(slope = 1, intercept = 0) +
    geom_text(x=max(x_lims)/2, y=max(x_lims) * .85, label=paste0("R-squared=", round(R2, 2)),
              size=5, fontface="italic")
  p
  
  return(p)
}

p1 <- plot_sbd_scatter(df, 20,  "rho_obs", c(-.5,1)) 
p2 <- plot_sbd_scatter(df, 20,  "pct_bias", c(-100,500)) 
p3 <- plot_sbd_scatter(df, 20,  "sd_ratio", c(0,5)) 




# Make combined figure. 
ggdraw() +
  draw_plot(p1, x=0, y = .66, width = 1, height = .33 )  +                            
  draw_plot(p2, x=0, y = .33, width = 1, height = .33) + 
  draw_plot(p3, x=0, y = .0, width = 1, height = .33) + 
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.985) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.66) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.33)  
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/spear_bias_dist/drought_sbd_figure_scatter.png"), bg = 'transparent', height = 9, width = 3)


df_sum <- df %>%
  filter(!is.infinite(value)) %>%
  # mutate(value = ifelse(metric == 'sd_ratio', abs(value - 1), value)) %>%
  group_by(threshold, source, metric, pct_type) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

filter(df_sum, metric == 'sd_ratio') %>%
  filter(threshold == 20)

df_sd_rat <- filter(df, metric == 'sd_ratio')

library(ggridges)
ggplot(df_sd_rat, aes(x = value, color = source)) +
  geom_boxplot(quantile_lines = TRUE, alpha = 0.75) +
  xlim(0, 5) + 
  theme_classic() + 
  facet_wrap(vars(pct_type, threshold))

            