# Plot annual drought attributes maps.

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

# Set import path
data_path <- "../data"

# Read in metadata
meta_df <- read_csv(paste0(data_path, "/Metadata/Metadata_w_clusters.csv")) 

df <- read_csv(paste0(data_path, "/single_site/ann_eval_long.csv")) %>%
  left_join(meta_df,by = 'site') %>%
  filter(!is.na(flow_cluster))



# Setup function
plot_attribute_map <- function(df, plot_measure, thresh, source_name, type_name, plot_metric){
  #' Function plots desired map. 
  
  # Subset data
  df_plot <- filter(df, measure == plot_measure) %>%
    filter(threshold == thresh) %>%
    filter(metric == plot_metric) %>%
    filter(source == source_name) %>%
    filter(type == type_name)
  
  # Setup map
  us <- map_data("state")
  us <- fortify(us, region="region")
  # my.palette <- brewer.pal(n=10, name = 'RdYlBu')
  
  df_plot <- df_plot %>%
    mutate(value = ifelse(value < min(0), min(0), value)) %>%
    mutate(value = ifelse(value > max(3), max(3), value))
  
  # Plot map
  p1 <- ggplot() + 
    geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region, group=group),
             fill="#ffffff", color="#7f7f7f", size=0.5) +
    # plot_usmap(include = c('CO', 'UT')) +
    geom_point(data=df_plot, aes(x=long, y=lat, color= value),
               size=1) + 
    coord_map("albers", lat0=39, lat1=45)+ 
    theme_map() +
    scale_color_scico(palette = 'batlow',
                      direction = 1,
                      limits = c(0, 1),
                      oob = scales::squish) +
    # xlim(-80, -65) +
    # ylim(40, 50) +
    theme(legend.position = c(0.02, 0.55), plot.margin=unit(c(-25,-20,-25,0),"mm"), legend.title = element_text(size=12)) + 
    labs(color = paste0(plot_measure, "\n", source_name, " ", plot_metric, ' ', thresh)) 
    # theme(legend.position = c(0.05, 0.3), plot.margin=unit(c(0,0,0,00),"mm")) + 
    # labs(color = '', title = paste0("   ", plot_metric, " ", source_name, " ", plot_measure, ' ', thresh))
  
  p2 <- ggplot(data = df_plot) +
    geom_histogram(aes(x = value), bins = 30) +
    ylab("") +
    xlab("") +
    xlim(0,3) +
    theme_classic() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  Main_Plot <- ggdraw() +
    draw_plot(p2, x= .0, y = -.05, width = 0.4, height = .3 ) +
    draw_plot(p1, x= .0, y = .0, width = 1, height = 1 )
  
  # Save map
  ggsave(paste0(data_path, "/Figures/Properties/All_Maps/drought_properties_", plot_metric, "_", type_name, "_",source_name,"_", plot_measure, "_", thresh, ".png"), plot=Main_Plot, bg = 'transparent', height = 8, width = 10)
  
  pplot <- p1 + theme(legend.title =element_blank())
  ggsave(paste0(data_path, "/Figures/Properties/All_Maps/drought_properties_simple_", plot_metric, "_", type_name, "_",source_name,"_", plot_measure, "_", thresh, ".png"), plot=pplot, bg = 'transparent', height = 8, width = 10)
  
  
  Main_Plot <- ggdraw() +
    draw_plot(p2, x= .0, y = -.05, width = 0.4, height = .3 ) +
    draw_plot(p1  + theme(legend.title =element_blank()), x= .0, y = .0, width = 1, height = 1 )
  
  return(p1)
}





# Setup function
plot_sig_box <- function(df,  measure_name, thresh, type, x_lims){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(type == !!type) %>%
    filter(threshold == !!thresh) %>%
    # filter(source == source_name) %>%
    filter(metric == 'nmae') %>%
    filter(measure == !!measure_name) %>%
    arrange(flow_cluster)  %>%
    mutate(source = ifelse(source == "nhm", 'NHM', 'NWM'))
  
  df_plot$cluster_name <- factor(df_plot$cluster_name, levels=rev(c('1-Northeast', '2-Northern Mid-Atlantic', '3-Southern Mid-Atlantic',
                                                                "4-Southeast", '5-Central plains and forests', 
                                                                '6-Northern central', '7-Central plains', '8-Rocky Mountains',
                                                                '9-Southwest', '10-South central', '11-California and interior west',
                                                                '12-Northwest')))
  
  median_value <- median(df_plot$value, na.rm = TRUE)
  
  df_plot <- df_plot %>%
    mutate(value = ifelse(value < min(x_lims), min(x_lims), value)) %>%
    mutate(value = ifelse(value > max(x_lims), max(x_lims), value))
  
  p <- ggplot(df_plot, aes(x=cluster_name, y=value, color = source)) + 
    geom_boxplot() +
    geom_hline(yintercept = median_value, linetype = 'dashed') +
    coord_flip() +
    theme_classic() +
    ylim(c(x_lims)) + 
    theme (legend.position = "bottom")
  
  p
  
  return(p)
}

# Setup plots for combined figure
stat_type <- 'nmae'

p1 <- plot_attribute_map(df, "total_duration_below_threshold", 20, "nhm", "variable", stat_type) 
p2 <- plot_attribute_map(df, "total_duration_below_threshold", 20, "nwm", "variable", stat_type) 

p3 <- plot_attribute_map(df, "total_flow_volume_below_threshold_cms", 20, "nhm", "variable", stat_type) 
p4 <- plot_attribute_map(df, "total_flow_volume_below_threshold_cms", 20, "nwm", "variable", stat_type) 

p5 <- plot_attribute_map(df, "maximum_drought_intensity", 20, "nhm", "variable", stat_type) 
p6 <- plot_attribute_map(df, "maximum_drought_intensity", 20, "nwm", "variable", stat_type) 

# Setup plots for combined figure
p1_box <- plot_sig_box(df,"total_duration_below_threshold", 20, "variable", c(0, 2)) 
p2_box <- plot_sig_box(df,"total_flow_volume_below_threshold_cms", 20, "variable", c(0, 2)) 
p3_box <- plot_sig_box(df,"maximum_drought_intensity", 20, "variable", c(0, 2)) 

ggdraw() +
  draw_plot(p1_box + theme(title=element_blank(), legend.position = 'top', axis.text=element_text(size=10), 
                           axis.text.y = element_text(angle=-30, vjust=1, hjust=1))
            , x=.675, y = .62, width = .32, height = 0.37 )  +                            
  draw_plot(p2_box + theme(title=element_blank(), legend.position = 'none', axis.text=element_text(size=10), 
                           axis.text.y = element_text(angle=-30, vjust=1, hjust=1)), 
            x=.675, y = .31, width = .32, height = 0.31) +
  draw_plot(p3_box + theme(title=element_blank(), legend.position = 'none', axis.text=element_text(size=10), 
                           axis.text.y = element_text(angle=-30, vjust=1, hjust=1)), 
            x=0.675, y = .0, width = .32, height = 0.31) +
  draw_plot(p1 + theme(legend.title =element_blank(), legend.position = c(0.16, 0.14)), x=-0.04, y = .67, width = .43, height = 0.32 )  +                            
  draw_plot(p3 + theme(legend.position = 'none'), x=-0.04, y = .35, width = .43, height = 0.32) +
  draw_plot(p5 +theme(legend.position = 'none'), x=-0.04, y = .03, width = .43, height = 0.32) +
  draw_plot(p2+ theme(legend.position = 'none'), x=.3, y = .67, width = .43, height = 0.32 )  +                            
  draw_plot(p4 + theme(legend.position = 'none'), x=.3, y = .35, width = .43, height = 0.32) +
  draw_plot(p6 + theme(legend.position = 'none'), x=0.3, y = .03, width = .43, height = 0.32) +
  draw_label("a)", colour = "black", size = 16, angle = 0, x = 0.03, y = 0.98) +
  draw_label("b)", colour = "black", size = 16, angle = 0, x = 0.03, y = 0.65) +
  draw_label("c)", colour = "black", size = 16, angle = 0, x = 0.03, y = 0.32) +
  draw_label("d)", colour = "black", size = 16, angle = 0, x = 0.37, y = 0.98) +
  draw_label("e)", colour = "black", size = 16, angle = 0, x = 0.37, y = 0.65) +
  draw_label("f)", colour = "black", size = 16, angle = 0, x = 0.37, y = 0.32) +
  draw_label("g)", colour = "black", size = 16, angle = 0, x = 0.67, y = 0.98) +
  draw_label("h)", colour = "black", size = 16, angle = 0, x = 0.67, y = 0.65) +
  draw_label("i)", colour = "black", size = 16, angle = 0, x = 0.67, y = 0.32) +
  draw_label("NHM", colour = "black", size = 14, x = 0.18, y = 0.96) +
  draw_label("NWM", colour = "black", size = 14, x = 0.51, y = 0.96) +
  draw_label("Drought Duration", colour = "black", size = 14, x = 0.01, y = 0.81, angle = 90) +
  draw_label("Drought Deficit", colour = "black", size = 14, x = 0.01, y = 0.49, angle = 90) +
  draw_label("Drought Intensity", colour = "black", size = 14, x = 0.01, y = 0.17, angle = 90)
ggsave(paste0(data_path, "/Figures/Properties/drought_combined_figure.png"), bg = 'transparent', height = 10.5, width = 14)

make_combined_plot <- function(metric_name, thresh, pct_type){
  nhm_plot <- plot_attribute_map(df, metric_name, thresh, "nhm", pct_type, "nmae")
  nwm_plot <- plot_attribute_map(df, metric_name, thresh, "nwm", pct_type, "nmae")
  box_plot <- plot_sig_box(df, metric_name, thresh, pct_type, c(0, 2)) 
  
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
  ggsave(paste0(data_path, "/Figures/Properties/simpler_figure_",metric_name, "_", pct_type, "_", thresh, ".png"), bg = 'transparent', height = 7, width = 10)
}

make_combined_plot("total_duration_below_threshold", 20, 'variable')
make_combined_plot("total_flow_volume_below_threshold_cms", 20, 'variable')
make_combined_plot("maximum_drought_intensity", 20, 'variable')

make_combined_plot("total_duration_below_threshold", 20, 'fixed')
make_combined_plot("total_flow_volume_below_threshold_cms", 20, 'fixed')
make_combined_plot("maximum_drought_intensity", 20, 'fixed')



# Setup function
plot_sig_scatter <- function(df,  measure_name, thresh, type, x_lims){
  #' Function plots desired map. 
  
  df_plot <- df %>%
    filter(metric == 'nmae') %>%
    filter(threshold == !!thresh) %>%
    arrange(flow_cluster) %>%
    filter(value <= max(x_lims)) %>%
    filter(value >= min(x_lims)) %>%
    filter(measure == !!measure_name) %>%
    filter(type == !!type) %>%
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
    geom_point(alpha = 0.1) +
    theme_classic() +
    xlim(x_lims) +
    ylim(x_lims) +
    geom_abline(slope = 1, intercept = 0) +
    geom_text(x=max(x_lims)/2, y=max(x_lims) * .85, label=paste0("R-squared=", round(R2, 2)),
              size=5, fontface="italic")
  p
  
  return(p)
}


# Setup plots for combined figure
p1 <- plot_sig_scatter(df,"total_duration_below_threshold", 20, "variable", c(0, 4)) 
p2 <- plot_sig_scatter(df,"total_flow_volume_below_threshold_cms", 20, "variable", c(0, 4)) 
p3 <- plot_sig_scatter(df,"maximum_drought_intensity", 20, "variable", c(0, 4)) 



# Make combined figure. 
ggdraw() +
  draw_plot(p1, x=0, y = .66, width = 1, height = .33 )  +                            
  draw_plot(p2, x=0, y = .33, width = 1, height = .33) + 
  draw_plot(p3, x=0, y = .0, width = 1, height = .33) + 
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.985) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.66) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.33)  
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/Properties/drought_sig_figure_scatter.png"), bg = 'transparent', height = 9, width = 3)
