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

library(sf)
library(nngeo)

# Set data path
data_path <- "../data"

# Read in kappa values to pull in sites that passed auditing
df <- read_csv(paste0(data_path, "/single_site/kappa_long.csv"))

# Read in metadata
meta_df <- read_csv(paste0(data_path, "/data_from_sydney/streamflow_gages_v1_n5390.csv")) %>%
  filter(site %in% df$site)

hcdn_clusters <- read_csv(paste0(data_path, "/Metadata/clus_hcdn.csv")) %>%
  rename(long = lon)

hcdn_clusters_new <- read.table(paste0(data_path, "/Metadata/final.12.clusters.ll"), header = FALSE) %>%
  as_tibble() %>%
  rename(lat = V1, long = V2, clus_new = V3)

hcdn_both <- full_join(hcdn_clusters, hcdn_clusters_new)

# We want new clusters
hcdn_clusters <- hcdn_both %>%
  select(-clus) %>%
  rename(clus = clus_new)

# Setup spatial data.
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
meta_df_spatial <- st_as_sf(x = meta_df, coords = c('long', 'lat'), crs = projcrs)
hcdn_df <- st_as_sf(x = hcdn_clusters, coords = c('long', 'lat'), crs = projcrs)

# # Plot map
# p1 <- ggplot() +
#   geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region, group=group),
#            fill="#ffffff", color="#7f7f7f", size=0.5) +
#   # plot_usmap(include = c('CO', 'UT')) +
#   geom_point(data=clus_diff, aes(x=long, y=lat, color = factor(clus_new)),
#              size=2) +
#   coord_map("albers", lat0=39, lat1=45)+
#   scale_color_manual(name = "Flow Cluster",
#                      values = c("1" = "#a6cee3", "2" = "#1f78b4", '3' = "#b2df8a",
#                                 "4" = '#33a02c', '5' = '#fb9a99', '6' = '#e31a1c',
#                                 "7" = '#fdbf6f', '8' = '#ff7f00', '9' = '#cab2d6',
#                                 "10" = '#6a3d9a', '11' = "yellow", '12' = '#b15928'
#                      ),
#                      # values = c("1" = "#8dd3c7", "2" = "#ffffb3", '3' = "#bebada",
#                      #            "4" = '#fb8072', '5' = '#80b1d3', '6' = '#fdb462',
#                      #            "7" = '#b3de69', '8' = '#fccde5', '9' = '#d9d9d9',
#                      #            "10" = '#bc90bd', '11' = "#ccebc5", '12' = '#ffed6f'
#                      # ),
#                      labels = c('1-Northeast', '2-Northern Mid-Atlantic', '3-Southern Mid-Atlantic',
#                                 "4-Southeast", '5-Central plains and forests',
#                                 '6-Northern central', '7-Central plains', '8-Rocky Mountains',
#                                 '9-Southwest', '10-South central', '11-California and interior west',
#                                 '12-Northwest')) +
#   theme_map() +
#   theme(legend.position = c(0, 0.15), plot.margin=unit(c(-25,-20,-25,0),"mm"), legend.title = element_text(size=12),
#         legend.text = element_text(size=10)) +
#   guides(colour = guide_legend(override.aes = list(size=5))) +
#   labs(color = "Flow Cluster")
# p1

closest_site <- c()
closest_clus <- c()
for(i in seq_len(nrow(meta_df_spatial))){
  print(i)
  nearest <- hcdn_df[which.min(st_distance(hcdn_df, meta_df_spatial[i,])),]
  closest_clus[i] <- nearest$clus
  closest_site[i] <- nearest$site
}

meta_df$flow_cluster <- as.factor(closest_clus)
meta_df$close_hcdn <- closest_site



# Name flow clusters
meta_df$cluster_name <- recode(meta_df$flow_cluster, 
                               '1' = '1-Northeast', 
                               "2" = '2-Northern Mid-Atlantic',
                               '3' = '3-Southern Mid-Atlantic',
                               '4' = "4-Southeast", 
                               '5' = '5-Central plains and forests', 
                               '6' = '6-Northern central', 
                               '7' = '7-Central plains', 
                               '8' = '8-Rocky Mountains',
                               '9' = '9-Southwest', 
                               '10' = '10-South central', 
                               '11' = '11-California and interior west',
                               '12' = '12-Northwest')

meta_sum <- meta_df %>%
  group_by(cluster_name) %>%
  summarise(sites = length(cluster_name))

# Setup map
us <- map_data("state")
us <- fortify(us, region="region")

# Plot map
p1 <- ggplot() + 
  geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region, group=group),
           fill="#ffffff", color="#7f7f7f", size=0.5) +
  # plot_usmap(include = c('CO', 'UT')) +
  geom_point(data=meta_df, aes(x=long, y=lat, color = flow_cluster),
             size=1) + 
  coord_map("albers", lat0=39, lat1=45)+ 
  scale_color_manual(name = "Regional \nStreamflow \nCluster", 
                     values = c("1" = "#a6cee3", "2" = "#1f78b4", '3' = "#b2df8a",
                                "4" = '#33a02c', '5' = '#fb9a99', '6' = '#e31a1c',
                                "7" = '#fdbf6f', '8' = '#ff7f00', '9' = '#cab2d6',
                                "10" = '#6a3d9a', '11' = "yellow", '12' = '#b15928'
                                ),
                     labels = c('1-Northeast (146)', 
                                '2-Northern Mid-Atlantic (652)', 
                                '3-Southern Mid-Atlantic (537)',
                                "4-Southeast (446)", 
                                '5-Central plains and forests (478)', 
                                '6-Northern central (328)', 
                                '7-Central plains (362)', 
                                '8-Rocky Mountains (412)',
                                '9-Southwest (180)', 
                                '10-South central (363)', 
                                '11-California and interior west (427)',
                                '12-Northwest (331)')) +
  theme_map() +
  theme(legend.position = c(0, 0.25), plot.margin=unit(c(-25,-20,-25,0),"mm"), legend.title = element_text(size=12),
        legend.text = element_text(size=10)) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(color = "Flow Cluster") 
p1
ggsave(paste0(data_path, "/Figures/Flow_Cluster_Map.png"), plot = p1, bg = 'transparent', height = 6, width = 10)

# write_csv(meta_df, paste0(data_path, "/Metadata/Metadata_w_clusters.csv"))

