# Plot Cohen's Kappa Metric Maps

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
library(scico)

# Set data path
data_path <- "../data"

# Read in metadata
meta_df <- read_csv(paste0(data_path, "/Metadata/Metadata_w_clusters.csv"))

df <- read_csv(paste0(data_path, "/single_site/kappa_long.csv")) %>%
  left_join(meta_df,by = 'site') %>%
  filter(!is.na(flow_cluster))


# Setup function
plot_kappa_map <- function(df,  thresh, source_name, type){
  #' Function plots desired map.

  df_plot <- df %>%
    filter(pct_type == !!type) %>%
    filter(threshold == !!thresh) %>%
    filter(source == source_name) %>%
    filter(metric == 'kappa')

  # Setup map
  us <- map_data("state")
  us <- fortify(us, region="region")

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
                      direction = -1,
                      limits = c(0, 1),
                      oob = scales::squish) +
    # xlim(-80, -65) +
    # ylim(40, 50) +
    theme(legend.position = c(0.02, 0.55), plot.margin=unit(c(-25,-20,-25,0),"mm"), legend.title = element_text(size=12)) +
    labs(color = paste0(source_name, " ", type, ' ', thresh))
  # theme(legend.position = c(0.05, 0.3), plot.margin=unit(c(0,0,0,00),"mm")) +
  # labs(color = '', title = paste0("   ", plot_metric, " ", source_name, " ", plot_measure, ' ', thresh))

  p2 <- ggplot(data = df_plot) +
    geom_histogram(aes(x = value), bins = 30) +
    ylab("") +
    xlab("") +
    xlim(0,1) +
    theme_classic() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

  Main_Plot <- ggdraw() +
    draw_plot(p2, x= .0, y = -.05, width = 0.4, height = .3 ) +
    draw_plot(p1, x= .0, y = .0, width = 1, height = 1 )

  # Save map
  # ggsave(paste0(data_path, "/Figures/Kappa/Drought_", "kappa", "_", source_name, "_", type, "_", thresh, ".png"), plot=Main_Plot, bg = 'transparent', height = 8, width = 10)

  Main_Plot <- ggdraw() +
    draw_plot(p2, x= .0, y = -.05, width = 0.4, height = .3 ) +
    draw_plot(p1  + theme(legend.title =element_blank()), x= .0, y = .0, width = 1, height = 1 )

  return(p1)
}


# Setup function
plot_kappa_box <- function(df,  thresh, type){
  #' Function plots desired map.

  df_plot <- df %>%
    filter(pct_type == !!type) %>%
    filter(threshold == !!thresh) %>%
    # filter(source == source_name) %>%
    filter(metric == 'kappa') %>%
    arrange(flow_cluster)

  df_plot$cluster_name <- factor(df_plot$cluster_name, levels=rev(c('1-Northeast', '2-Northern Mid-Atlantic', '3-Southern Mid-Atlantic',
                                                                "4-Southeast", '5-Central plains and forests',
                                                                '6-Northern central', '7-Central plains', '8-Rocky Mountains',
                                                                '9-Southwest', '10-South central', '11-California and interior west',
                                                                '12-Northwest')))

  median_value <- median(df_plot$value, na.rm = TRUE)

  p <- ggplot(df_plot, aes(x=cluster_name, y=value, color = source)) +
    geom_boxplot() +
    geom_hline(yintercept = median_value, linetype = 'dashed') +
    coord_flip() +
    theme_classic() +
    ylim(-0.3, 1) +
    theme (legend.position = "bottom")

  p

  return(p)
}



# Setup plots for combined figure
p1 <- plot_kappa_map(df, 20, "nhm", "variable")
p2 <- plot_kappa_map(df, 20, "nhm", "fixed")
p3 <- plot_kappa_map(df, 20, "nwm", "variable")
p4 <- plot_kappa_map(df, 20, "nwm", "fixed")

# Setup plots for combined figure
p1_box <- plot_kappa_box(df, 20, "variable")
p2_box <- plot_kappa_box(df, 20, "fixed")


# Generate combined figure
ggdraw() +
  draw_plot(p1_box+ theme(title=element_blank(), legend.position = 'top',
                          axis.text=element_text(size=10),
                          axis.text.y = element_text(angle=-30, vjust=1, hjust=1)),
            x=.675, y = .475, width = .32, height = 0.525)  +
  draw_plot(p2_box + theme(title=element_blank(), legend.position = 'none',
                           axis.text=element_text(size=10),
                           axis.text.y = element_text(angle=-30, vjust=1, hjust=1)),
            x=.675, y = .0, width = .32, height = 0.475) +
  draw_plot(p1+ theme(legend.title =element_blank(), legend.position = c(0.16, 0.14)), x=-.04, y = .5, width = .43, height = 0.5 )  +
  draw_plot(p2 + theme(legend.position = 'none'), x=-0.04, y = .0, width = .43, height = 0.5) +
  draw_plot(p3 + theme(legend.position = 'none'), x=0.3, y = .5, width = .43, height = 0.5) +
  draw_plot(p4+  theme(legend.position = 'none'), x=.3, y = .0, width = .43, height = 0.5 )  +
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.98) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.03, y = 0.48) +
  draw_label("c)", colour = "black", size = 12, angle = 0, x = 0.37, y = 0.98) +
  draw_label("d)", colour = "black", size = 12, angle = 0, x = 0.37, y = 0.48) +
  draw_label("e)", colour = "black", size = 12, angle = 0, x = 0.67, y = 0.98) +
  draw_label("f)", colour = "black", size = 12, angle = 0, x = 0.67, y = 0.48) +
  draw_label("NHM", colour = "black", size = 12, x = 0.18, y = 0.95) +
  draw_label("NWM", colour = "black", size = 12, x = 0.51, y = 0.95) +
  draw_label("Variable", colour = "black", size = 12, x = 0.01, y = 0.78, angle = 90) +
  draw_label("Fixed", colour = "black", size = 12, x = 0.01, y = 0.3, angle = 90)
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/kappa/drought_combined_figure.png"), bg = 'transparent', height = 7, width = 14)


make_combined_plot <- function(thresh, pct_type){
  nhm_plot <- plot_kappa_map(df, thresh, "nhm", pct_type)
  nwm_plot <- plot_kappa_map(df, thresh, "nwm", pct_type)
  box_plot <- plot_kappa_box(df, thresh, pct_type)

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
  ggsave(paste0(data_path, "/Figures/Kappa/simpler_figure_kappa_", "_", pct_type, "_", thresh, ".png"), bg = 'transparent', height = 7, width = 10)
}

make_combined_plot(20, 'fixed')
make_combined_plot(20, 'variable')




# Setup function
plot_kappa_scatter <- function(df,  thresh, type){
  #' Function plots desired map.

  df_plot <- df %>%
    filter(pct_type == !!type) %>%
    filter(threshold == !!thresh) %>%
    filter(metric == 'kappa') %>%
    arrange(flow_cluster) %>%
    pivot_wider(names_from = source, values_from = value)

  df_plot$cluster_name <- factor(df_plot$cluster_name, levels=c('1-Northeast', '2-Northern Mid-Atlantic', '3-Southern Mid-Atlantic',
                                                                "4-Southeast", '5-Central plains and forests',
                                                                '6-Northern central', '7-Central plains', '8-Rocky Mountains',
                                                                '9-Southwest', '10-South central', '11-California and interior west',
                                                                '12-Northwest'))

  R2 <- cor(df_plot$nwm, df_plot$nhm) ^ 2

  p <- ggplot(df_plot, aes(x=nwm, y=nhm)) +
    geom_point(alpha = 0.25) +
    theme_classic() +
    xlim(-.3, 1) +
    ylim(-.3, 1) +
    geom_abline(slope = 1, intercept = 0) +
    geom_text(x=0, y=0.85, label=paste0("R-squared=", round(R2, 2)),
              size=5, fontface="italic")
  p

  return(p)
}

p1 <- plot_kappa_scatter(df, 20,  "variable")
p2 <- plot_kappa_scatter(df, 20, "fixed")



# Make combined figure.
ggdraw() +
  draw_plot(p1, x=0.0, y = .0, width = .5, height = 1 )  +
  draw_plot(p2, x=0.5, y = .0, width = .5, height = 1) +
  draw_label("a)", colour = "black", size = 12, angle = 0, x = 0.02, y = 0.985) +
  draw_label("b)", colour = "black", size = 12, angle = 0, x = 0.5, y = 0.985)  +
  draw_label("Variable", colour = "black", size = 12, x = 0.25, y = 0.95) +
  draw_label("Fixed", colour = "black", size = 12, x = 0.75, y = 0.95)
# draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
ggsave(paste0(data_path, "/Figures/Kappa/drought_kappa_figure_scatter.png"), bg = 'transparent', height = 4, width = 8)
