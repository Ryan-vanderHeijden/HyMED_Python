"""
bias_dist.py

Calculate modified bias and distributional (std ratio) errors for low-flow periods.
Translated from R/site_bias_dist.R.
"""

import numpy as np
import pandas as pd

from ._util import metric_rows
from .config import METRIC_COLS, PCT_TYPE_LABELS, THRESHOLDS


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
        Percentile thresholds. Default: config.THRESHOLDS.

    Returns
    -------
    pd.DataFrame
        Long format (config.METRIC_COLS): site, pct_type ('variable'/'fixed'), threshold,
        metric ('bias', 'pct_bias', 'sd_ratio'), source ('mod'), value. Observed and modeled
        flows are each subset to their own days at or below the threshold.
    """
    if thresholds is None:
        thresholds = THRESHOLDS

    df = df_pct.dropna(subset=["q_obs"]).copy()
    records = []

    for thresh in thresholds:
        for group in ["site", "jd"]:
            obs_col = f"weibull_{group}_obs"
            mod_col = f"weibull_{group}_mod"

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

            key = {"site": site_name, "pct_type": PCT_TYPE_LABELS[group], "threshold": thresh}
            records += metric_rows(
                key, {"bias": mod_bias, "pct_bias": mod_pct_bias, "sd_ratio": mod_sd_ratio}
            )

    return pd.DataFrame(records, columns=METRIC_COLS)
