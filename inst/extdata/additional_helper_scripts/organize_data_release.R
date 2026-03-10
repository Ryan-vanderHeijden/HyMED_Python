# Organize data for data release

library(tibble)
library(readr)
library(dplyr)

kappa_long <- read_csv("../data/single_site/kappa_long.csv") %>%
  filter(!metric %in% c('mle', 'ml')) %>%
  write_csv("../data/Data_Release/kappa_long.csv")

ann_eval_long <- read_csv("../data/single_site/ann_eval_long.csv") %>%
  write_csv("../data/Data_Release/ann_eval_long.csv")

write_csv(head(ann_eval_long, 5000), "../data/Data_Release/ann_eval_long_short.csv")

spear_bias_dist_long <- read_csv("../data/single_site/spear_bias_dist_long.csv") %>%
  write_csv("../data/Data_Release/spear_bias_dist_long.csv")

file_list <- list.files(path="../data/single_site/percentiles", pattern = '.csv', all.files=TRUE)
file_list_transfered <- list.files(path="../data/Data_Release/streamflow_percentiles", pattern = '.csv', all.files=TRUE)
file_list <- file_list[!(file_list %in% file_list_transfered)]
for (file in file_list){
  print(file)
  read_csv(paste0("../data/single_site/percentiles/", file)) %>%
    select(c("site","dt", "q_cms_obs", "q_cms_nhm", "q_cms_nwm", "jd", "year",
             "month", "wy", "cy",  "weibull_jd_obs", "weibull_jd_nhm", "weibull_jd_nwm",
             "weibull_site_obs", "weibull_site_nhm", "weibull_site_nwm")) %>%
    write_csv(paste0("../data/Data_Release/streamflow_percentiles/", file))
}

# Pulling in other metadata
df_gages_hydro <- read_csv("../data/Metadata/Gages-II/gagesII_sept30_2011_conterm_hydro.csv") %>%
  rename(site = STAID) %>%
  select(c(site, BFI_AVE)) %>%
  rename(bfi = BFI_AVE)

df_aridity <- read_csv("../data/Metadata/aridity.csv")

meta_df <- read_csv(paste0("../data/data_from_sydney/streamflow_gages_v1_n5390.csv")) %>%
  left_join(df_aridity) %>%
  left_join(df_gages_hydro) %>%
  mutate(aridity = round(aridity, 2)) %>%
  write_csv("../data/Data_Release/streamflow_gages_v1_n5390.csv")
