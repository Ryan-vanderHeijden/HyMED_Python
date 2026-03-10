"""
calculate_booleans.py

Convert drought events into boolean (True/False) timeseries.
Translated from R/calculate_site_booleans.R and R/calculate_site_booleans_threshold_only.R.
"""

import itertools

import numpy as np
import pandas as pd


def calculate_site_boolean(
    df_prop: pd.DataFrame,
    site_name: str,
    sdate: str = "1984-04-01",
    edate: str = "2016-03-31",
) -> pd.DataFrame:
    """
    Expand drought event properties into a per-day drought boolean timeseries.

    Parameters
    ----------
    df_prop : pd.DataFrame
        Drought event properties from calculate_site_properties() (first element).
        Must have columns: drought_id, threshold, pct_type, start, end.
    site_name : str
        Site identifier.
    sdate, edate : str
        Date range for the output timeseries (YYYY-MM-DD).

    Returns
    -------
    pd.DataFrame
        Columns: threshold, pct_type, day, drought (bool), site.
    """
    if df_prop.empty:
        # Return a fully False timeseries
        all_dates = pd.date_range(start=sdate, end=edate, freq="D")
        return pd.DataFrame({
            "threshold": [],
            "pct_type": [],
            "day": [],
            "drought": [],
            "site": [],
        })

    # Expand each event [start, end] into per-day rows
    rows = []
    for _, row in df_prop.iterrows():
        for day in pd.date_range(start=row["start"], end=row["end"], freq="D"):
            rows.append({
                "threshold": row["threshold"],
                "pct_type": row["pct_type"],
                "day": day,
                "drought": True,
            })

    if rows:
        df_bool = pd.DataFrame(rows)
    else:
        df_bool = pd.DataFrame(columns=["threshold", "pct_type", "day", "drought"])

    # Build complete date grid: all combinations of threshold × pct_type × date
    all_dates = pd.date_range(start=sdate, end=edate, freq="D")
    thresholds = df_bool["threshold"].unique() if not df_bool.empty else []
    pct_types = df_bool["pct_type"].unique() if not df_bool.empty else []

    grid = pd.DataFrame(
        list(itertools.product(thresholds, pct_types, all_dates)),
        columns=["threshold", "pct_type", "day"],
    )

    dates_df = grid.merge(df_bool, on=["threshold", "pct_type", "day"], how="left")
    dates_df["drought"] = dates_df["drought"].notna()
    dates_df = dates_df.sort_values("day").reset_index(drop=True)
    dates_df["site"] = site_name

    return dates_df


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
    weibull_cols = ["weibull_jd_obs", "weibull_jd_mod", "weibull_site_obs", "weibull_site_mod"]
    df = df_pct[["site", "dt"] + weibull_cols].copy()

    # Melt percentile columns to long format
    df_long = df.melt(
        id_vars=["site", "dt"],
        value_vars=weibull_cols,
        var_name="pct_type",
        value_name="pct_value",
    )

    # Add boolean columns for each threshold
    for t in [5, 10, 20, 30]:
        df_long[str(t)] = df_long["pct_value"] <= t

    # Melt thresholds to long format
    df_bool = df_long.melt(
        id_vars=["site", "dt", "pct_type"],
        value_vars=["5", "10", "20", "30"],
        var_name="threshold",
        value_name="drought",
    )
    df_bool["threshold"] = df_bool["threshold"].astype(int)
    df_bool = df_bool.drop(columns=[], errors="ignore")
    df_bool = df_bool.rename(columns={"dt": "day"})

    return df_bool[["site", "day", "pct_type", "threshold", "drought"]]
