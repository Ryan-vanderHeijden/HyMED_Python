"""
calculate_booleans.py

Convert percentile series into drought boolean (True/False) timeseries.
Translated from R/calculate_site_booleans_threshold_only.R.
"""

import pandas as pd

from .config import PCT_TYPES, THRESHOLDS


def calculate_site_boolean_threshold_only(df_pct: pd.DataFrame) -> pd.DataFrame:
    """
    Create drought boolean timeseries directly from percentile thresholds (no pooling).

    Parameters
    ----------
    df_pct : pd.DataFrame
        Percentile dataframe from calculate_site_percentiles().

    Returns
    -------
    pd.DataFrame
        Long-format DataFrame with columns: site, day, pct_type, threshold, drought.
    """
    weibull_cols = PCT_TYPES
    df = df_pct[["site", "dt"] + weibull_cols].copy()

    # Melt percentile columns to long format
    df_long = df.melt(
        id_vars=["site", "dt"],
        value_vars=weibull_cols,
        var_name="pct_type",
        value_name="pct_value",
    )

    # Add boolean columns for each threshold
    for t in THRESHOLDS:
        df_long[str(t)] = df_long["pct_value"] <= t

    # Melt thresholds to long format
    df_bool = df_long.melt(
        id_vars=["site", "dt", "pct_type"],
        value_vars=[str(t) for t in THRESHOLDS],
        var_name="threshold",
        value_name="drought",
    )
    df_bool["threshold"] = df_bool["threshold"].astype(int)
    df_bool = df_bool.rename(columns={"dt": "day"})

    return df_bool[["site", "day", "pct_type", "threshold", "drought"]]
