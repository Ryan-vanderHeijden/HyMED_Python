"""
spearmans.py

Calculate Spearman's rank correlation for low-flow periods.
Translated from R/site_spearmans.R.
"""

import numpy as np
import pandas as pd


def _nse(sim: np.ndarray, obs: np.ndarray) -> float:
    """Nash-Sutcliffe Efficiency."""
    mask = ~np.isnan(obs) & ~np.isnan(sim)
    obs, sim = obs[mask], sim[mask]
    if len(obs) == 0 or np.sum((obs - obs.mean()) ** 2) == 0:
        return np.nan
    return float(1.0 - np.sum((obs - sim) ** 2) / np.sum((obs - obs.mean()) ** 2))


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
        Percentile thresholds. Default: [5, 10, 20, 30].

    Returns
    -------
    pd.DataFrame
        Columns: rho_obs_mod, quant_length, mod_nse, threshold, site, pct_type.
    """
    if thresholds is None:
        thresholds = [5, 10, 20, 30]

    df = df_pct.dropna(subset=["q_obs"]).copy()

    # Compute ranks on full dataset (min_rank equivalent)
    df["obs_rank"] = df["q_obs"].rank(method="min")
    df["mod_rank"] = df["q_mod"].rank(method="min")

    records = []

    for thresh in thresholds:
        for pct_type in ["weibull_site", "weibull_jd"]:
            obs_col = f"{pct_type}_obs"
            df["pct_value_obs"] = df[obs_col]

            df_sub = df[df["pct_value_obs"] <= thresh].copy()

            if len(df_sub) < 2:
                rho = np.nan
                nse_val = np.nan
            else:
                obs_r = df_sub["obs_rank"].values
                mod_r = df_sub["mod_rank"].values
                cov = np.cov(obs_r, mod_r, ddof=1)[0, 1]
                rho = cov / (obs_r.std(ddof=1) * mod_r.std(ddof=1))
                nse_val = _nse(df_sub["q_mod"].values, df_sub["q_obs"].values)

            records.append({
                "rho_obs_mod": rho,
                "quant_length": len(df_sub),
                "mod_nse": nse_val,
                "threshold": thresh,
                "site": site_name,
                "pct_type": pct_type,
            })

    return pd.DataFrame(records)
