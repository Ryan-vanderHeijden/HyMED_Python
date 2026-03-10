"""
bias_dist.py

Calculate modified bias and distributional (std ratio) errors for low-flow periods.
Translated from R/site_bias_dist.R.
"""

import numpy as np
import pandas as pd


def site_bias_distribution(
    df_pct: pd.DataFrame,
    site_name: str,
    thresholds: list = None,
) -> pd.DataFrame:
    """
    Calculate bias and SD ratio for modeled vs observed low flows.

    Parameters
    ----------
    df_pct : pd.DataFrame
        Percentile dataframe from calculate_site_percentiles().
    site_name : str
        Site identifier.
    thresholds : list of int
        Percentile thresholds. Default: [5, 10, 20, 30].

    Returns
    -------
    pd.DataFrame
        Columns: mod_bias, mod_pct_bias, mod_sd_ratio, thresh, pct_type, site.
    """
    if thresholds is None:
        thresholds = [5, 10, 20, 30]

    df = df_pct.dropna(subset=["q_obs"]).copy()
    records = []

    for thresh in thresholds:
        for pct_type in ["weibull_site", "weibull_jd"]:
            obs_col = f"{pct_type}_obs"
            mod_col = f"{pct_type}_mod"

            # Subset to periods where observed is below threshold
            df_obs = df[df[obs_col] <= thresh]
            mean_obs = df_obs["q_obs"].mean()
            sd_obs = df_obs["q_obs"].std(ddof=1)

            # Subset to periods where modeled is below threshold
            df_mod = df[df[mod_col] <= thresh]
            mean_mod = df_mod["q_mod"].mean()
            sd_mod = df_mod["q_mod"].std(ddof=1)

            mod_bias = mean_mod - mean_obs
            mod_pct_bias = ((mean_mod - mean_obs) / abs(mean_obs)) * 100
            mod_sd_ratio = sd_mod / sd_obs if sd_obs != 0 else np.nan

            records.append({
                "mod_bias": mod_bias,
                "mod_pct_bias": mod_pct_bias,
                "mod_sd_ratio": mod_sd_ratio,
                "thresh": thresh,
                "pct_type": pct_type,
                "site": site_name,
            })

    return pd.DataFrame(records)
