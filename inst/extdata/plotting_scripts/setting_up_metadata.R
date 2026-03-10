# Use the metadata file from the COBALT data release not what I have now. 

library(readr)
library(tibble)
library(dplyr)
# Local
data_path <- "../data"

# Read in metadata
meta_df <- read_csv(paste0(data_path, "/data_from_sydney/NHM_NWM_gage_info_n5520.txt")) %>%
  rename(site = STAID,
         lat = dec_lat_va,
         long = dec_long_va)

meta_df_new <- read_csv(paste0(data_path, "/streamflow_gages_v1_n5390.csv")) %>%
  rename(site = site_no,
         lat = dec_lat_va,
         long = dec_long_va) %>%
  select(c(site, lat, long, drain_sqkm)) %>%
  write_csv(paste0(data_path, "/data_from_sydney/streamflow_gages_v1_n5390.csv"))
