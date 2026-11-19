"""
config.py

Constants shared across HyMED. Change values here, not at the call sites.
"""

# Drought percentile thresholds evaluated everywhere (most to least extreme).
THRESHOLDS = [5, 10, 20, 30]

# Percentile columns produced by calculate_site_percentiles(), and the flow column
# each one is computed from. "jd" = variable (Julian-day) percentiles, "site" = fixed.
PCT_TYPES = ["weibull_jd_obs", "weibull_jd_mod", "weibull_site_obs", "weibull_site_mod"]
PCT_FLOW_COLS = ["q_obs", "q_mod", "q_obs", "q_mod"]

# Labels used in the long-format summary outputs.
PCT_TYPE_LABELS = {"jd": "variable", "site": "fixed"}

# Flows below this are treated as zero for the CDPM, by units.
ZERO_FLOW_CUTOFF = {"cms": 0.00028, "cfs": 0.01}

# CDPM: amount subtracted per preceding consecutive zero-flow day, so that longer
# zero runs rank as drier.
CDPM_STEP = 0.001

# Drought event pooling (Simeone and Foks, 2024).
# Below-threshold runs shorter than this are dropped, and inter-event gaps shorter
# than this (and shorter than the preceding run) are pooled into one event.
MIN_EVENT_DURATION = 5
# Inter-event gaps whose severity is below this fraction of the preceding run's
# severity are also pooled.
POOLING_SEVERITY_RATIO = 0.1

# Climate year starts in April, water year in October; both are labeled by the
# calendar year in which they end.
CLIMATE_YEAR_START_MONTH = 4
WATER_YEAR_START_MONTH = 10
