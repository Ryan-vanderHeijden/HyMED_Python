# This script runs drought evaluation on a combined data set of NHM and NWM data.
# It runs though each site individually and iteratively.

# required packages
library(tibble)
library(dplyr)
library(lubridate)
library(stringr)
library(zoo)
library(tidyr)
library(rlang)
library(readr)
library(hydroGOF)
library(irr)

# This is the location of the scripts
scripts_path <- "R"

# Source functions from R scripts within parent directory 'R'
function_files <- list.files(scripts_path, pattern = "\\.R$", full.names = T)
for(file in function_files){source(file)}

# Run input data to split up large file into many individual ones for each site.
# The setup_individual_files script was used to extract, standardize and add important metadata
# for the input files for calculating percentiles. In this example we use data from the Sciencebase data release
# that have already been standardized for that formatting. If a user is starting with a different dataset they
# may use that function as a guide for setting up their data for the calculate_site_percentiles_function.

options(dplyr.summarise.inform = FALSE)

# Function that runs drought evaluation for a single site. Each run returns event
# metrics as a csv for a single site.
# Right now I'm not sure if we should include this script as a function or if we should leave it here as an example of a wrapper script.
# My hunch is towards the latter as I think its important to show that this can be customizable for the user based on their data.
run_site_eval <- function(site_name, data_path, model){
  # Read in data
  df <- read_csv(paste0(data_path, "/input_data/", site_name, ".csv"), guess_max = 11000, col_types = cols(site = col_character())) %>%
    mutate(site = as.character(site))

  # In the original implementation of this code, these names were hard coded. I've started to make this more robust removing that hard coding.
  df$q_mod <- df[[paste0('q_cms_', model)]]
  df$q_obs <- df[['q_cms_obs']]
  df <- select(df, -c(q_cms_obs, paste0('q_cms_', model)))

  # Percentiles function
  df_pct <- calculate_site_percentiles(df, site_name, units = 'cms')
  write_csv(df_pct, paste0(data_path, "/percentiles/", site_name,  ".csv"))

  # Function to calculate drought events and their properties.
  df_both <- calculate_site_properties(df_pct, 
                                       site_name,
                                       thresholds = c(5, 10, 20, 30),
                                       percent_type_list = c('weibull_jd_obs', 
                                                             'weibull_jd_mod', 
                                                             'weibull_site_obs', 
                                                             'weibull_site_mod'),
                                       flow_name_list = c('q_obs', 'q_mod', 
                                                          'q_obs', 'q_mod'),
                                       start_cy = 1985,
                                       end_cy = 2016)
  df_properties <- df_both[[1]] %>% ungroup()
  df_annual_stats <- df_both[[2]] %>% ungroup()

  # Calculate cohen's kappa
  df_bool <- calculate_site_boolean(df_properties, site_name, "1984-04-01", "2016-03-31")

  # Calculate booleans only based on thresholds with no pooling for simplicity.
  df_bool_threshold_only <- calculate_site_boolean_threshold_only(df_pct)

  # Here I use just the threshold only method.
  df_kappa <- site_cohens_kappa(df_bool_threshold_only, site_name)
  write_csv(df_kappa, paste0(data_path, "/kappa/kappa_", site_name, ".csv"))

  # Calculate Spearman's Rho
  df_spear <- site_spearmans(df_pct, site_name)
  write_csv(df_spear, paste0(data_path, "/spearmans/spearman_", site_name, ".csv"))

  # Calculate bias and sd distribution
  df_bias_dist <- site_bias_distribution(df_pct, site_name)
  write_csv(df_bias_dist, paste0(data_path, "/bias_dist/bias_dist_", site_name, ".csv"))

  # Calculate annual event based metrics.
  df_ann_eval <- site_annual_signatures(df_annual_stats, site_name)
  write_csv(df_ann_eval, paste0(data_path, "/ann_eval/ann_eval_", site_name, ".csv"))
}

# Update the model name to match the location
model_name <- 'nhm'

# This the the location of the data.
data_path <- paste0("inst/extdata/", model_name)

# Create new folders if necessary
if (!file.exists(paste0(data_path, "/percentiles"))) {dir.create(paste0(data_path, "/percentiles"))}
if (!file.exists(paste0(data_path, "/kappa"))) {dir.create(paste0(data_path, "/kappa"))}
if (!file.exists(paste0(data_path, "/spearmans"))) {dir.create(paste0(data_path, "/spearmans"))}
if (!file.exists(paste0(data_path, "/bias_dist"))) {dir.create(paste0(data_path, "/bias_dist"))}
if (!file.exists(paste0(data_path, "/ann_eval"))) {dir.create(paste0(data_path, "/ann_eval"))}

# Pull in all sites in input data as a list to be run through.
site_list <- tools::file_path_sans_ext(list.files(paste0(data_path, "/input_data") ))

# Run all sites in a for loop.
for (i in 1:length(site_list)){
# for (i in 1:length(site_list)){
  site_name <- site_list[[i]]
  print(paste0("site: ", site_name, " - iteration: ", i))
  run_site_eval(site_name = site_name, data_path = data_path, model = model_name)
}

# Gather all of the individual kappa files and pivot them to long format and write them out.
df_kappa <- gather_data("kappa", data_path) %>%
  pivot_longer(names_to = "temp", values_to = "value", mod_kappa:mod_classification_accuracy) %>%
  mutate(source = substr(temp, start = 1, stop = 3),
         metric = substr(temp, start = 5, stop = length(temp))) %>%
  select(site, threshold, pct_type, metric, source, pct_type, value) %>%
  mutate(pct_type = ifelse(pct_type == 'jd', 'variable',
                           ifelse(pct_type == 'site', 'fixed', pct_type))) %>%
  write_csv(paste0(data_path, '/kappa_long.csv'))

# Gather all of the spearmans data files and pivot them to long format
df_spearmans <- gather_data("spearmans", data_path) %>%
  select(-quant_length) %>%
  rename(mod_rho_obs = rho_obs_mod) %>%
  pivot_longer(names_to = 'temp', values_to = "value", mod_rho_obs:mod_nse) %>%
  mutate(source = substr(temp, start = 1, stop = 3),
         metric = substr(temp, start = 5, stop = length(temp))) %>%
  select(-temp)

# Gather all of the bias and distribution data files and pivot them to long format.
df_bias_dist <- gather_data("bias_dist", data_path) %>%
  pivot_longer(names_to = 'temp', values_to = "value", mod_bias:mod_sd_ratio) %>%
  mutate(source = substr(temp, start = 1, stop = 3),
         metric = substr(temp, start = 5, stop = length(temp))) %>%
  select(-temp) %>%
  rename(threshold = thresh)

# Combine spearmans and bias/distribution files and write them out.
df_spear_bias_dist <- bind_rows(df_spearmans, df_bias_dist) %>%
  select(site, threshold, source, metric, pct_type, value) %>%
  arrange(site, threshold) %>%
  mutate(pct_type = ifelse(pct_type == "weibull_site", 'fixed',
                           ifelse(pct_type == 'weibull_jd', 'variable', pct_type))) %>%
  write_csv(paste0(data_path, '/spear_bias_dist_long.csv'))

# Gather all of the individual annual attributes files and pivot them to long format and write them out.
df_ann_eval <- gather_data("ann_eval", data_path) %>%
  relocate(mean_obs, .after = nmae_mod) %>%
  pivot_longer(names_to = 'temp', values_to = "value", NSE_mod:nmae_mod) %>%
  separate(col = temp, into = c('metric', 'source'), sep = "_") %>%
  mutate(type = ifelse(type == 'jd', 'variable',
                       ifelse(type == 'site', 'fixed', type))) %>%
  write_csv(paste0(data_path, '/ann_eval_long.csv'))
