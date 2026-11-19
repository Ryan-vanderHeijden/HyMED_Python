"""
calculate_percentiles.py

Calculates drought percentiles on modeled and observed streamflow data using the
Consecutive Drought Period Method (CDPM) to handle zero flow periods.
Translated from R/calculate_percentiles_single.R.
"""

import pandas as pd

from ._util import add_time_columns, rleid
from .config import CDPM_STEP, ZERO_FLOW_CUTOFF


def _weibull(series: pd.Series) -> pd.Series:
    """Weibull plotting position in percent: 100 * min_rank / (n + 1)."""
    return 100.0 * series.rank(method="min") / (len(series) + 1)


def _previous_zeros(is_zero: pd.Series) -> pd.Series:
    """Position within the current run of zero flows (1, 2, ...), 0 on non-zero days."""
    run_position = is_zero.groupby(rleid(is_zero)).cumcount() + 1
    return run_position.where(is_zero == 1, 0).astype(float)


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
    add_time_columns(df)

    # CDPM: zero-flow days are shifted down by CDPM_STEP per preceding zero day, so
    # that longer zero runs rank as drier.
    zero_cutoff = ZERO_FLOW_CUTOFF.get(units, ZERO_FLOW_CUTOFF["cfs"])
    for src in ["obs", "mod"]:
        df[f"zero_bool_{src}"] = (df[f"q_{src}"] < zero_cutoff).astype(int)
    for src in ["obs", "mod"]:
        df[f"zero_id_{src}"] = rleid(df[f"zero_bool_{src}"])
    for src in ["obs", "mod"]:
        df[f"previous_zeros_{src}"] = _previous_zeros(df[f"zero_bool_{src}"])
    for src in ["obs", "mod"]:
        df[f"{src}_prev_zeros"] = df[f"q_{src}"] - df[f"previous_zeros_{src}"] * CDPM_STEP

    df = df.sort_values("dt").reset_index(drop=True)

    # The *_no_cdpm columns are diagnostic outputs; the CDPM versions are used downstream.
    df["weibull_site_obs_no_cdpm"] = _weibull(df["q_obs"])
    df["weibull_site_mod_no_cdpm"] = _weibull(df["q_mod"])
    df["weibull_site_obs_cdpm"] = _weibull(df["obs_prev_zeros"])
    df["weibull_site_mod_cdpm"] = _weibull(df["mod_prev_zeros"])

    by_jd = df.groupby("jd")
    df["weibull_jd_obs_no_cdpm"] = by_jd["q_obs"].transform(_weibull)
    df["weibull_jd_mod_no_cdpm"] = by_jd["q_mod"].transform(_weibull)
    df["weibull_jd_obs_cdpm"] = by_jd["obs_prev_zeros"].transform(_weibull)
    df["weibull_jd_mod_cdpm"] = by_jd["mod_prev_zeros"].transform(_weibull)

    df["weibull_jd_obs"] = df["weibull_jd_obs_cdpm"]
    df["weibull_jd_mod"] = df["weibull_jd_mod_cdpm"]
    df["weibull_site_obs"] = df["weibull_site_obs_cdpm"]
    df["weibull_site_mod"] = df["weibull_site_mod_cdpm"]

    df["feature_id"] = df["site"]

    return df
