"""
spearmans.py

Calculate Spearman's rank correlation for low-flow periods.
Translated from R/site_spearmans.R.
"""

import numpy as np
import pandas as pd

from ._util import nse
from .config import THRESHOLDS


def site_spearmans(
    df_pct: pd.DataFrame,
    site_name: str,
    thresholds: list = None,
) -> pd.DataFrame:
    """
    Calculate Spearman's rho between observed and modeled flow for low-flow periods.

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
        Columns: rho_obs_mod, quant_length, mod_nse, threshold, site, pct_type.
    """
    if thresholds is None:
        thresholds = THRESHOLDS

    df = df_pct.dropna(subset=["q_obs"]).copy()

    # Compute ranks on full dataset (min_rank equivalent)
    df["obs_rank"] = df["q_obs"].rank(method="min")
    df["mod_rank"] = df["q_mod"].rank(method="min")

    records = []

    for thresh in thresholds:
        for pct_type in ["weibull_site", "weibull_jd"]:
            obs_col = f"{pct_type}_obs"
            df_sub = df[df[obs_col] <= thresh]

            if len(df_sub) < 2:
                rho = np.nan
                nse_val = np.nan
            else:
                obs_r = df_sub["obs_rank"].values
                mod_r = df_sub["mod_rank"].values
                cov = np.cov(obs_r, mod_r, ddof=1)[0, 1]
                rho = cov / (obs_r.std(ddof=1) * mod_r.std(ddof=1))
                nse_val = nse(df_sub["q_mod"].values, df_sub["q_obs"].values)

            records.append({
                "rho_obs_mod": rho,
                "quant_length": len(df_sub),
                "mod_nse": nse_val,
                "threshold": thresh,
                "site": site_name,
                "pct_type": pct_type,
            })

    return pd.DataFrame(records)
