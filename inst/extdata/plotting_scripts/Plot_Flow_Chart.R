library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(stringr)
library(readr)
library(data.table)
library(tidyr)
library(cowplot)


data_path <- "../data/single_site"
site_name <- "01011000"
source("Scripts/calculate_percentiles_single.R")
source("Scripts/calculate_properties_single.R")

df <- read_csv(paste0(data_path, "/input_data/", site_name, ".csv"), guess_max = 11000, col_types = cols()) %>%
  mutate(site = as.character(site))


# Percentiles function
df_pct <- calculate_site_percentiles(df, site_name)

# Function to calculate drought events and their properties.
df_both <- calculate_site_properties(df_pct, site_name)
df_properties <- df_both[[1]] %>% ungroup()
df_annual_stats <- df_both[[2]] %>% ungroup()


events_df <- df_properties %>%
  filter(threshold == 20) %>%
  filter(pct_type == 'weibull_site_obs') %>%
  arrange(desc(duration))
plot_start <- as.Date(events_df$start[[4]] - 60)
plot_end <- as.Date(events_df$end[[4]] + 60)

df_pct <- df_pct %>%
  mutate(obs_site_below = ifelse(weibull_site_obs_cdpm <= 20, 1, 0),
         nwm_site_below = ifelse(weibull_site_nwm_cdpm <= 20, 1, 0),
         matches_threshold = obs_site_below == nwm_site_below)


# Plot streamflow with the flow threshold
#-------------------------------------------------------------------------------

df_pct <- df_pct %>%
  ungroup() %>%
  mutate(thresh_20_site = quantile(q_cms_obs, 0.2)) %>%
  group_by(jd) %>%
  mutate(thresh_20_jd = quantile(q_cms_obs, 0.2)) %>%
  ungroup()

p_thresh_1 <- ggplot(df_pct) +
  geom_line(size = 1, aes(dt, thresh_20_site), color = 'red', alpha = 0.75) +
  geom_line(size = 1, aes(dt, q_cms_obs)) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") + ylab("Fixed Streamflow") +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  ylim(0, 300)

p_thresh_2 <- ggplot(df_pct) +
  geom_line(size = 1, aes(dt, thresh_20_jd), color = 'red', alpha = 0.75) +
  geom_line(size = 1, aes(dt, q_cms_obs)) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") + ylab("Variable Streamflow") +
  theme_classic() +
  ylim(0, 300)

p_thresh_3 <- ggplot(df_pct) +
  geom_line(size = 1, aes(dt, thresh_20_jd), color = 'red', alpha = 0.75) +
  geom_line(size = 1, aes(dt, q_cms_nwm)) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") + ylab("Variable Streamflow") +
  theme_classic() +
  ylim(0, 300)

ggdraw() +
  draw_plot(p_thresh_1, x=0.0, y = .52, width = 1, height = 0.48)  +
  draw_plot(p_thresh_2, x=0.0, y = .0, width = 1, height = 0.52)

ggsave(file = paste0(data_path, '/../Figures/Flow_Chart/NWM_drought_thresholds_', site_name, ".png"))
ggsave(file = paste0(data_path, '/../Figures/Presentations/NWM_drought_thresholds_', site_name, ".png"), width = 4, height = 4)

p_thresh_1p <- ggplot(df_pct) +
  geom_line(size = 1, aes(dt, thresh_20_site), color = 'red', alpha = 0.75) +
  geom_line(size = 1, aes(dt, q_cms_obs)) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") + ylab("Streamflow") +
  theme_classic() +
  # theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  ylim(0, 300)

p_thresh_2p <- ggplot(df_pct) +
  geom_line(size = 1, aes(dt, thresh_20_jd), color = 'red', alpha = 0.75) +
  geom_line(size = 1, aes(dt, q_cms_obs)) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") + ylab("Variable Streamflow") +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  ylim(0, 300)

plot_1 <- ggdraw() +
  draw_plot(p_thresh_2p + theme(axis.text.x = element_text (angle=45, vjust=.5, hjust=.5)), x=0.52, y = .0, width = 0.47, height = 1) +
  draw_plot(p_thresh_1p + theme(axis.text.x = element_text (angle=45, vjust=.5, hjust=.5)), x=0.0, y = .0, width = 0.51, height = 1)

plot_1


# Plot flow values vs percentiles
#-------------------------------------------------------------------------------
p_pct <- ggplot(df_pct, aes(log(q_cms_obs), weibull_site_obs_cdpm)) +
  geom_line(size = 1) +
  xlab("Log Streamflow (cms)") + ylab("Resulting Percentile") +
  theme_classic()
ggsave(file = paste0(data_path, '/../Figures/Flow_Chart/NWM_drought_transform_', site_name, ".png"), p_pct, height = 2, width = 2)


# ==============================================================================
# Plot percentiles
df_pct$plot_value <- df_pct$weibull_site_obs_cdpm
p1 <- ggplot(df_pct, aes(dt,plot_value)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") +
  ylab("Observed Percentile") +
  theme_classic() +
  ylim(0, 100) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = c(0.5, 0.9)) +
  # theme(legend.position = c(0.5, 0.9)) +
  scale_fill_manual("", values = "orange")

df_pct$plot_value <- df_pct$weibull_site_nwm_cdpm
p2 <- ggplot(df_pct, aes(dt,plot_value)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 month",
               labels=scales::date_format("%m-%Y"),
               limits =c(plot_start,plot_end)) +
  xlab("Date") +
  ylab("NWM Percentile") +
  theme_classic() +
  ylim(0, 100) +
  theme(legend.position = c(0.5, 0.9)) +
  # theme(legend.position = c(0.5, 0.9)) +
  theme(legend.position = 'none') +
  scale_fill_manual("", values = "orange")


ggdraw() +
  draw_plot(p1, x=0.0, y = .53, width = 1, height = 0.47)  +
  draw_plot(p2, x=0.0, y = .0, width = 1, height = 0.53)
ggsave(file = paste0(data_path, '/../Figures/Flow_Chart/NWM_percentiles_', site_name, ".png"))
# ggsave(file = paste0(data_path, '/../Figures/Presentations/NWM_percentiles_', site_name, ".png"), width = 4, height = 4)

plot_2 <- ggdraw() +
  draw_plot(p1 + theme(axis.title.y = element_blank()), x=0.0, y = .53, width = 1, height = 0.47)  +
  draw_plot(p2 + theme(axis.title.y = element_blank()), x=0.0, y = .0, width = 1, height = 0.53)

# ==============================================================================
# Plot with drought markers
# df_pct$plot_value <- df_pct$weibull_site_obs_cdpm
# p1 <- ggplot(df_pct, aes(dt,plot_value)) +
#   geom_line(size = 1) +
#   scale_x_date(date_breaks = "2 month",
#                labels=scales::date_format("%m-%Y"),
#                limits =c(plot_start,plot_end)) +
#   geom_ribbon(data=df_pct, aes(ymin=ifelse(plot_value > 20,20,plot_value),ymax=20, fill="drought conditions"), alpha=0.75)  +
#   xlab("Date") +
#   ylab("Observed Percentile") +
#   theme_classic() +
#   ylim(0, 100) +
#   theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = c(0.5, 0.9)) +
#   # theme(legend.position = c(0.5, 0.9)) +
#   scale_fill_manual("", values = "orange")

df_pct$plot_value <- df_pct$weibull_site_obs_cdpm
p3 <- p1 +
  geom_ribbon(data=df_pct, aes(ymin=ifelse(plot_value > 20,20,plot_value),ymax=20, fill="drought conditions"), alpha=0.75)


df_pct$plot_value <- df_pct$weibull_site_nwm_cdpm
p4 <- p2 +
  geom_ribbon(data=df_pct, aes(ymin=ifelse(plot_value > 20,20,plot_value),ymax=20, fill="drought conditions"), alpha=0.75)



# p2 <- ggplot(df_pct, aes(dt,plot_value)) +
#   geom_line(size = 1) +
#   scale_x_date(date_breaks = "2 month",
#                labels=scales::date_format("%m-%Y"),
#                limits =c(plot_start,plot_end)) +
#   geom_ribbon(data=df_pct, aes(ymin=ifelse(plot_value > 20,20,plot_value),ymax=20, fill="drought conditions"), alpha=0.75)  +
#   xlab("Date") +
#   ylab("NWM Percentile") +
#   theme_classic() +
#   ylim(0, 100) +
#   theme(legend.position = c(0.5, 0.9)) +
#   geom_point(aes(x=dt, y=100, color = matches_threshold), shape = 15) +
#   # theme(legend.position = c(0.5, 0.9)) +
#   theme(legend.position = 'none') +
#   scale_fill_manual("", values = "orange")

library('irr')

# Calculate Kappa For Short Series
df_pct_short <- filter(df_pct, dt >= plot_start) %>%
  filter(dt <= plot_end)

df <- tibble(mod = df_pct_short$nwm_site_below, obs = df_pct_short$obs_site_below)

kappa <- irr::kappa2(df)

yes_yes <- length(filter(df, mod == 1 & obs == 1)$obs)
no_no <- length(filter(df, mod == 0 & obs == 0)$obs)
no_yes <- length(filter(df, mod == 1 & obs == 0)$obs)
yes_no <- length(filter(df, mod == 0 & obs == 1)$obs)



min_value_obs <- min(df_pct_short$weibull_site_obs_cdpm)

df_pct$plot_value <- df_pct$weibull_site_obs_cdpm

df_pct_short_nwm_drought <- df_pct_short %>%
  filter(nwm_site_below == 1)
nwm_start <- as.Date(head(df_pct_short_nwm_drought$dt, 1))
nwm_end <- as.Date(tail(df_pct_short_nwm_drought$dt, 1))
min_value <- min(df_pct_short_nwm_drought$weibull_site_nwm_cdpm)


## Make Main Plots

p5 <- p4 +
  geom_point(aes(x=dt, y=100, color = matches_threshold), shape = 15)

ggdraw() +
  draw_plot(p3, x=0.03, y = .53, width = 1, height = 0.47)  +
  draw_plot(p4, x=0.03, y = .0, width = 1, height = 0.53)
ggsave(file = paste0(data_path, '/../Figures/Flow_Chart/NWM_drought_example_', site_name, ".png"))

ggdraw() +
  draw_plot(p3, x=0.03, y = .53, width = 1, height = 0.47)  +
  draw_plot(p5, x=0.03, y = .0, width = 1, height = 0.53)
ggsave(file = paste0(data_path, '/../Figures/Flow_Chart/NWM_drought_comparison_', site_name, ".png"))

p3_annotated <- p3 +
  geom_segment(aes(y=22, yend = 22, x = plot_start+60, xend = plot_end - 62)) +
  annotate(geom="text", x=nwm_end - 25, y=27, label="Duration", color="black") +
  geom_point(aes(x= plot_start+60, y = 22)) +
  geom_point(aes(x= plot_end - 62, y = 22)) +
  geom_segment(aes(y=20, yend = min_value_obs, x = plot_end - 57, xend = plot_end - 57)) +
  annotate(geom="text", x=nwm_end + 28, y=12, label="Intensity", color="black") +
  geom_point(aes(x= plot_end - 57, y = 20)) +
  geom_point(aes(x= plot_end - 57, y = min_value_obs)) +
  annotate(geom="text", x=nwm_end -20, y=12, label="Severity", color="black")

p5_annotated <- p5 +
  geom_segment(aes(y=22, yend = 22, x = nwm_start, xend = nwm_end - 1)) +
  annotate(geom="text", x=nwm_end - 25, y=27, label="Duration", color="black") +
  geom_point(aes(x= nwm_start, y = 22)) +
  geom_point(aes(x= nwm_end - 1, y = 22)) +
  geom_segment(aes(y=20, yend = min_value, x = nwm_end+3, xend = nwm_end+3)) +
  annotate(geom="text", x=nwm_end + 17, y=12, label="Intensity", color="black") +
  geom_point(aes(x= nwm_end+3, y = 20)) +
  geom_point(aes(x= nwm_end +3, y = min_value)) +
  annotate(geom="text", x=nwm_end -20, y=14, label="Severity", color="black")

plot_3 <- ggdraw() +
  draw_plot(p3 + theme(axis.title.y = element_blank()), x=0.0, y = .53, width = 1, height = 0.47)  +
  draw_plot(p4 + theme(axis.title.y = element_blank()), x=0.0, y = .0, width = 1, height = 0.53)

plot_4 <- ggdraw() +
  draw_plot(p3_annotated + theme(axis.title.y = element_blank()), x=0.0, y = .53, width = 1, height = 0.47)  +
  draw_plot(p5_annotated + theme(axis.title.y = element_blank()), x=0.0, y = .0, width = 1, height = 0.53)

# spear_text <- "The rank values from the \nentire period of both \nobserved and modeled \ndischarges subset to only \nperiods of observed drought"
# bsd_text <- "Observed discharge during \nperiods of observed drought and \nmodeled discharge during \nperiods of modeled drought."

master_plot <- ggdraw() +
  draw_plot(plot_1, x=0.0, y = 0.6, width = 0.47, height = 0.35) +
  draw_plot(plot_3, x=0.51, y = 0.53, width = 0.47, height = 0.47) +
  draw_plot(plot_2, x=0.0, y = 0, width = 0.47, height = 0.47) +
  draw_plot(plot_4, x=0.53, y = 0.0, width = 0.47, height = 0.47) +
  # draw_plot(plot_5_sp, x=0.5, y = 0.44, width = 0.25, height = 0.2) +
  # draw_plot(plot_5_bsd, x=0.75, y = 0.33, width = 0.25, height = 0.2) +
  # draw_plot(plot_6, x=0.5, y = 0.0, width = 0.5, height = 0.31) +
  draw_label("Fixed", colour = "black", size = 12, angle = 0, x = 0.14, y = 0.8) +
  draw_label("Variable", colour = "black", size = 12, angle = 0, x = 0.35, y = 0.8) +

  draw_label("Observed", colour = "black", size = 12, angle = 0, x = 0.16, y = 0.4) +
  draw_label("NWM", colour = "black", size = 12, angle = 0, x = 0.16, y = 0.18) +

  draw_label("Observed", colour = "black", size = 12, angle = 0, x = 0.68, y = 0.94) +
  draw_label("NWM", colour = "black", size = 12, angle = 0, x = 0.68, y = 0.72) +

  draw_label("Observed", colour = "black", size = 12, angle = 0, x = 0.68, y = 0.4) +
  draw_label("NWM", colour = "black", size = 12, angle = 0, x = 0.68, y = 0.18) +

  draw_label("a) Flow Thresholds", hjust = 0, colour = "black", size = 14, angle = 0, x = 0.02, y = 0.985) +
  draw_label("b) Streamflow Percentiles", hjust = 0, colour = "black", size = 14, angle = 0, x = 0.02, y = 0.5) +
  draw_label("c) Drought Identification", hjust = 0, colour = "black", size = 14, angle = 0, x = 0.55, y = 0.985) +
  draw_label("d) Evaluation", hjust = 0, colour = "black", size = 14, angle = 0, x = 0.55, y = 0.49)
  # draw_label("e) Spearman's, Bias and Distributional", hjust = 0, colour = "black", size = 14, angle = 0, x = 0.5, y = 0.65) +
  # draw_label("f) Drought Signatures", hjust = 0, colour = "black", size = 14, angle = 0, x = 0.5, y = 0.32)
  # draw_label(spear_text, hjust = 0, vjust = 0, colour = "black", size = 12, angle = 0, x = 0.5, y = 0.36) +
  # draw_label(bsd_text, hjust = 0, vjust = 0, colour = "black", size = 12, angle = 0, x = 0.75, y = 0.54)

# master_plot
ggsave(plot = master_plot, file = paste0(data_path, '/../Figures/Flow_Chart/Flow_chart_', site_name, ".png"), height = 10, width = 12)

