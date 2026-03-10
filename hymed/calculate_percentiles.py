"""
calculate_percentiles.py

Calculates drought percentiles on modeled and observed streamflow data using the
Consecutive Drought Period Method (CDPM) to handle zero flow periods.
Translated from R/calculate_percentiles_single.R.
"""

import numpy as np
import pandas as pd


def _rleid(series: pd.Series) -> pd.Series:
    """R's rleid(): assign a group id that increments each time the value changes."""
    return (series != series.shift()).cumsum()


def calculate_site_percentiles(df: pd.DataFrame, site_name: str, units: str) -> pd.DataFrame:
    """
    Calculate Weibull drought percentiles for observed and modeled streamflow.

    Parameters
    ----------
    df : pd.DataFrame
        Must contain columns: site, dt (date), q_mod, q_obs.
    site_name : str
        USGS station identifier.
    units : str
        'cms' or 'cfs'.

    Returns
    -------
    pd.DataFrame
        Input dataframe augmented with percentile columns.
    """
    df = df[["site", "dt", "q_mod", "q_obs"]].copy()
    df["dt"] = pd.to_datetime(df["dt"])
    df["jd"] = df["dt"].dt.dayofyear
    df["year"] = df["dt"].dt.year
    df["month"] = df["dt"].dt.month
    df["wy"] = np.where(df["month"] >= 10, df["year"] + 1, df["year"])
    df["cy"] = np.where(df["month"] >= 4, df["year"] + 1, df["year"])

    # Zero flow cutoffs (CDPM)
    if units == "cms":
        zero_cutoff = 0.00028
    else:
        zero_cutoff = 0.01

    df["zero_bool_obs"] = (df["q_obs"] < zero_cutoff).astype(int)
    df["zero_bool_mod"] = (df["q_mod"] < zero_cutoff).astype(int)

    # Run-length IDs for consecutive zero/non-zero periods
    df["zero_id_obs"] = _rleid(df["zero_bool_obs"])
    df["zero_id_mod"] = _rleid(df["zero_bool_mod"])

    # Count consecutive zeros within each zero run (1-based row number within group)
    df_zero_obs = (
        df[df["zero_bool_obs"] == 1]
        .copy()
        .assign(previous_zeros_obs=lambda x: x.groupby("zero_id_obs").cumcount() + 1)
        [["dt", "previous_zeros_obs"]]
    )
    df_zero_mod = (
        df[df["zero_bool_mod"] == 1]
        .copy()
        .assign(previous_zeros_mod=lambda x: x.groupby("zero_id_mod").cumcount() + 1)
        [["dt", "previous_zeros_mod"]]
    )

    # Merge zero counts back
    df = df.merge(df_zero_obs, on="dt", how="left")
    df = df.merge(df_zero_mod, on="dt", how="left")
    df["previous_zeros_obs"] = df["previous_zeros_obs"].fillna(0)
    df["previous_zeros_mod"] = df["previous_zeros_mod"].fillna(0)

    # CDPM-adjusted flow values (shift zero runs slightly for ranking)
    df["obs_prev_zeros"] = df["q_obs"] - df["previous_zeros_obs"] * 0.001
    df["mod_prev_zeros"] = df["q_mod"] - df["previous_zeros_mod"] * 0.001

    # --- Weibull percentile calculations ---
    # Weibull formula: 100 * min_rank / (n + 1)
    n = len(df)

    def weibull(series: pd.Series) -> pd.Series:
        return 100.0 * series.rank(method="min") / (len(series) + 1)

    df = df.sort_values("dt").reset_index(drop=True)

    # Site-level percentiles (no CDPM)
    df["weibull_site_obs_no_cdpm"] = weibull(df["q_obs"])
    df["weibull_site_mod_no_cdpm"] = weibull(df["q_mod"])

    # Site-level percentiles (CDPM)
    df["weibull_site_obs_cdpm"] = weibull(df["obs_prev_zeros"])
    df["weibull_site_mod_cdpm"] = weibull(df["mod_prev_zeros"])

    # Julian-day-level percentiles (grouped by jd)
    def weibull_group(series: pd.Series) -> pd.Series:
        return 100.0 * series.rank(method="min") / (len(series) + 1)

    df["weibull_jd_obs_no_cdpm"] = df.groupby("jd")["q_obs"].transform(weibull_group)
    df["weibull_jd_mod_no_cdpm"] = df.groupby("jd")["q_mod"].transform(weibull_group)
    df["weibull_jd_obs_cdpm"] = df.groupby("jd")["obs_prev_zeros"].transform(weibull_group)
    df["weibull_jd_mod_cdpm"] = df.groupby("jd")["mod_prev_zeros"].transform(weibull_group)

    # Use CDPM versions as final columns
    df["weibull_jd_obs"] = df["weibull_jd_obs_cdpm"]
    df["weibull_jd_mod"] = df["weibull_jd_mod_cdpm"]
    df["weibull_site_obs"] = df["weibull_site_obs_cdpm"]
    df["weibull_site_mod"] = df["weibull_site_mod_cdpm"]

    df["feature_id"] = df["site"]

    return df
