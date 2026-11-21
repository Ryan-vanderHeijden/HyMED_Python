"""
_util.py

Small helpers shared by several HyMED modules.
"""

import numpy as np
import pandas as pd

from .config import CLIMATE_YEAR_START_MONTH, WATER_YEAR_START_MONTH


def rleid(series: pd.Series) -> pd.Series:
    """R's data.table::rleid(): a group id that increments each time the value changes."""
    return (series != series.shift()).cumsum()


def nse(sim: np.ndarray, obs: np.ndarray) -> float:
    """Nash-Sutcliffe Efficiency over pairs where both values are present.

    Returns NaN when there are no pairs or the observations have zero variance.
    """
    mask = ~np.isnan(obs) & ~np.isnan(sim)
    obs, sim = obs[mask], sim[mask]
    if len(obs) == 0:
        return np.nan
    denom = np.sum((obs - obs.mean()) ** 2)
    if denom == 0:
        return np.nan
    return float(1.0 - np.sum((obs - sim) ** 2) / denom)


def metric_rows(key: dict, metrics: dict, source: str = "mod") -> list:
    """Long-format rows: one per metric, each {**key, metric, source, value}."""
    return [{**key, "metric": m, "source": source, "value": v} for m, v in metrics.items()]


def _year_starting(dt: pd.Series, start_month: int) -> np.ndarray:
    """Year label for a year beginning in start_month, named by the year it ends in."""
    return np.where(dt.dt.month >= start_month, dt.dt.year + 1, dt.dt.year)


def climate_year(dt: pd.Series) -> np.ndarray:
    """Climate year (April to March), labeled by the year it ends in."""
    return _year_starting(dt, CLIMATE_YEAR_START_MONTH)


def add_time_columns(df: pd.DataFrame, date_col: str = "dt") -> pd.DataFrame:
    """Add jd, year, month, wy and cy columns derived from a datetime column, in place."""
    dt = df[date_col]
    df["jd"] = dt.dt.dayofyear
    df["year"] = dt.dt.year
    df["month"] = dt.dt.month
    df["wy"] = _year_starting(dt, WATER_YEAR_START_MONTH)
    df["cy"] = climate_year(dt)
    return df


def decimal_date(dt) -> float:
    """Approximate lubridate::decimal_date for one timestamp: year + (doy - 1) / 365."""
    return dt.year + (dt.dayofyear - 1) / 365.0
