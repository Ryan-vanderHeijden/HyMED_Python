"""
calculate_properties.py

Identifies drought events and calculates their properties at the annual (climate year) level.
Translated from R/calculate_properties_single.R.

Two labelings share one identification step (identify_droughts):

* calculate_site_properties — the R package's scheme. Events are identified separately
  at each percentile threshold (5/10/20/30), and severity/deficits are measured relative
  to that threshold. The ``threshold`` column is the identification threshold.

* calculate_site_properties_universal — events are identified once, at the baseline
  (30th) percentile, so the event set and every per-event value are identical to the
  per-threshold scheme at threshold 30. Each event is additionally labeled with one
  severity class (5/10/20/30): the most extreme class its lowest percentile reaches.
  The ``threshold`` column holds that class, and annual statistics are grouped by it.
  This is a different drought definition, not a correction of the per-threshold one.
"""

from typing import List, Optional, Tuple

import numpy as np
import pandas as pd

from ._util import climate_year, decimal_date, rleid
from .config import (
    MIN_EVENT_DURATION, PCT_FLOW_COLS, PCT_TYPES, POOLING_SEVERITY_RATIO, THRESHOLDS,
)

# Baseline percentile for the universal scheme's event identification.
BASELINE_THRESHOLD = 30

# Annual statistics of the raw (unpooled) below-threshold runs, and the value used for
# climate years without any run.
RUN_ANNUAL_FILL = {
    "total_duration_below_threshold": 0,
    "longest_duration_below_threshold": 0,
    "total_severity_below_threshold": 0,
    "largest_severity_below_threshold": 0,
    "maximum_drought_intensity": 0,
    "minimum_flow_cms": np.nan,
    "total_flow_volume_below_threshold_cms": 0,
    "largest_flow_volume_below_threshold_cms": 0,
}

# Annual statistics of the pooled drought events, and their fill values.
EVENT_ANNUAL_FILL = {
    "total_drought_severity": 0,
    "total_drought_duration": 0,
    "number_of_drought_events": 0,
    "largest_drought_severity": 0,
    "longest_drought_duration": 0,
    "total_drought_flow_volume_cms": 0,
    "longest_drought_flow_volume_cms": 0,
    "start_day_of_longest_drought": np.nan,
    "start_date_of_longest_drought": np.nan,
    "end_date_of_longest_drought": np.nan,
    "end_day_of_longest_drought": np.nan,
}

LONG_COLS = ["site", "cy", "measure", "value", "threshold", "pct_type"]


def _sum(s: pd.Series) -> float:
    """Per-group sum via Series.sum (numpy pairwise), not groupby's compensated "sum".

    The two differ around 1e-16, which is enough to make or break exact ties between
    climate years, and ties change the Spearman/Kendall ranks in site_annual_signatures.
    This keeps results identical to the original translation.
    """
    return s.sum()


def classify_severity(min_pct, class_bins: List[int]) -> np.ndarray:
    """Most extreme class reached: the smallest bin >= min_pct (else the largest bin).

    class_bins must be ascending, e.g. [5, 10, 20, 30]: 3 -> 5, 7 -> 10, 25 -> 30.
    """
    bins = np.asarray(class_bins)
    idx = np.searchsorted(bins, np.asarray(min_pct), side="left")
    return bins[np.minimum(idx, len(bins) - 1)]


def identify_droughts(
    df_pct: pd.DataFrame,
    percent_type: str,
    flow_col: str,
    threshold: float,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Identify below-threshold runs and pooled drought events for one percentile series.

    Parameters
    ----------
    df_pct : pd.DataFrame
        Percentile dataframe from calculate_site_percentiles().
    percent_type : str
        Percentile column, e.g. 'weibull_jd_obs'. Its second part ('jd' or 'site')
        decides whether the flow threshold is computed per Julian day or per site.
    flow_col : str
        Flow column matching percent_type ('q_obs' or 'q_mod').
    threshold : float
        Percentile at or below which a day counts as drought.

    Returns
    -------
    runs : pd.DataFrame
        One row per unpooled run of below-threshold days, split by climate year:
        site, cy, drought_id, days, lowest_percent, severity, lowest_flow, flow_volume.
    events : pd.DataFrame
        One row per pooled drought event lasting at least MIN_EVENT_DURATION days:
        site, drought_id, severity, mean_intensity, max_intensity, duration, start, end,
        min_flow_cms, mean_flow_cms, max_flow_cms, flow_volume_cms, cy.
        Empty if there are no such events.
    """
    group_col = percent_type.split("_")[1]  # 'jd' or 'site'

    df = df_pct[["site", "dt", "cy", "jd"]].copy()
    df["flow_value"] = df_pct[flow_col]
    df["value"] = df_pct[percent_type]
    df = df.sort_values(["site", "dt"]).reset_index(drop=True)
    df["below"] = (df["value"] <= threshold).astype(int)
    df["drought_id"] = rleid(df["below"])

    # Flow at the threshold percentile (per Julian day or per site), and daily deficits.
    df["flow_thresh"] = df.groupby(group_col)["flow_value"].transform(
        "quantile", threshold / 100.0
    )
    df["deficit"] = threshold - df["value"]
    df["flow_deficit"] = df["flow_thresh"] - df["flow_value"]

    # ---- Unpooled below-threshold runs, split at climate-year boundaries ----
    runs = (
        df[df["below"] == 1]
        .groupby(["site", "cy", "drought_id"])
        .agg(
            days=("value", "count"),
            lowest_percent=("value", "min"),
            severity=("deficit", _sum),
            lowest_flow=("flow_value", "min"),
            flow_volume=("flow_deficit", _sum),
        )
        .reset_index()
    )
    runs["severity"] = runs["severity"].abs()

    # ---- Pooling: drop short or weak inter-event gaps between drought runs ----
    all_runs = (
        df.groupby(["site", "drought_id"])
        .agg(
            duration=("value", "count"),
            drought_bool=("below", "mean"),
            severity=("deficit", _sum),
        )
        .reset_index()
        .sort_values(["site", "drought_id"])
        .reset_index(drop=True)
    )
    all_runs["severity"] = all_runs["severity"].abs()
    by_site = all_runs.groupby("site")
    previous_duration = by_site["duration"].shift(1)
    severity_ratio = all_runs["severity"] / by_site["severity"].shift(1)
    pooled_gap = (
        (
            (all_runs["duration"] < MIN_EVENT_DURATION)
            & (previous_duration > all_runs["duration"])
        )
        | (severity_ratio < POOLING_SEVERITY_RATIO)
    ) & (all_runs["drought_bool"] == 0)
    gap_ids = all_runs.loc[pooled_gap, "drought_id"]

    pooled = df[~df["drought_id"].isin(gap_ids)].sort_values(["site", "dt"]).reset_index(drop=True)
    pooled["drought_id"] = rleid(pooled["below"])

    # ---- Pooled drought events ----
    in_event = pooled[pooled["below"] == 1]
    if in_event.empty:
        return runs, pd.DataFrame()
    events = (
        in_event.groupby(["site", "drought_id"])
        .agg(
            severity=("deficit", _sum),
            mean_intensity=("value", "mean"),
            max_intensity=("value", "min"),
            duration=("value", "count"),
            start=("dt", "first"),
            end=("dt", "last"),
            min_flow_cms=("flow_value", "min"),
            mean_flow_cms=("flow_value", "mean"),
            max_flow_cms=("flow_value", "max"),
            flow_volume_cms=("flow_deficit", _sum),
        )
        .reset_index()
    )
    events["drought_id"] = events.groupby("site")["drought_id"].rank(method="dense").astype(int)
    events = events[events["duration"] >= MIN_EVENT_DURATION].copy()
    events["cy"] = climate_year(events["start"])
    return runs, events


def _annual_run_stats(
    runs: pd.DataFrame, threshold: float, labels: List[int], cy_range: List[int],
) -> pd.DataFrame:
    """Annual statistics of labeled runs, with every (site, cy, label) present."""
    by_cy = (
        runs.groupby(["site", "cy", "threshold"])
        .agg(
            total_duration_below_threshold=("days", "sum"),
            longest_duration_below_threshold=("days", "max"),
            total_severity_below_threshold=("severity", "sum"),
            largest_severity_below_threshold=("severity", "max"),
            maximum_drought_intensity=("lowest_percent", "min"),
            minimum_flow_cms=("lowest_flow", "min"),
            total_flow_volume_below_threshold_cms=("flow_volume", "sum"),
            largest_flow_volume_below_threshold_cms=("flow_volume", "max"),
        )
    )
    by_cy["maximum_drought_intensity"] = threshold - by_cy["maximum_drought_intensity"]
    complete = pd.MultiIndex.from_product(
        [runs["site"].unique(), cy_range, labels], names=["site", "cy", "threshold"]
    )
    return by_cy.reindex(complete).fillna(RUN_ANNUAL_FILL).reset_index()


def _annual_event_stats(
    events: pd.DataFrame, labels: List[int], cy_range: List[int],
) -> pd.DataFrame:
    """Annual statistics of labeled events, with every (site, cy, label) present."""
    records = []
    for (site, cy, label), grp in events.groupby(["site", "cy", "threshold"]):
        longest = grp.loc[grp["duration"].idxmax()]
        start_day = longest["start"].dayofyear
        records.append({
            "site": site,
            "cy": cy,
            "threshold": label,
            "total_drought_severity": grp["severity"].sum(),
            "total_drought_duration": grp["duration"].sum(),
            "number_of_drought_events": len(grp),
            "largest_drought_severity": grp["severity"].max(),
            "longest_drought_duration": grp["duration"].max(),
            "total_drought_flow_volume_cms": grp["flow_volume_cms"].sum(),
            "longest_drought_flow_volume_cms": grp["flow_volume_cms"].max(),
            "start_day_of_longest_drought": start_day,
            "start_date_of_longest_drought": decimal_date(longest["start"]),
            "end_date_of_longest_drought": decimal_date(longest["end"]),
            # As in the R package: start day-of-year plus duration (may exceed 365).
            "end_day_of_longest_drought": start_day + longest["duration"],
        })
    complete = pd.MultiIndex.from_product(
        [events["site"].unique(), cy_range, labels], names=["site", "cy", "threshold"]
    )
    return (
        pd.DataFrame(records)
        .set_index(["site", "cy", "threshold"])
        .reindex(complete)
        .fillna(EVENT_ANNUAL_FILL)
        .reset_index()
    )


def _to_long(frames: List[pd.DataFrame], measures: List[str]) -> pd.DataFrame:
    if not frames:
        return pd.DataFrame()
    return pd.concat(frames, ignore_index=True).melt(
        id_vars=["site", "cy", "threshold", "pct_type"],
        value_vars=measures,
        var_name="measure",
        value_name="value",
    )[LONG_COLS]


def _site_properties(
    df_pct: pd.DataFrame,
    id_thresholds: List[int],
    class_bins: Optional[List[int]],
    percent_type_list: List[str],
    flow_name_list: List[str],
    start_cy: int,
    end_cy: int,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Identify events at each of id_thresholds and label them.

    class_bins None: label = identification threshold (per-threshold scheme).
    Otherwise: label = severity class from the lowest percentile reached (universal).
    """
    cy_range = list(range(start_cy, end_cy + 1))
    events_list, run_annual, event_annual = [], [], []

    for percent_type, flow_col in zip(percent_type_list, flow_name_list):
        for thresh in id_thresholds:
            runs, events = identify_droughts(df_pct, percent_type, flow_col, thresh)
            if class_bins is None:
                labels = [thresh]
                runs["threshold"] = thresh
                if not events.empty:
                    events["threshold"] = thresh
            else:
                labels = list(class_bins)
                runs["threshold"] = classify_severity(runs["lowest_percent"], labels)
                if not events.empty:
                    events["threshold"] = classify_severity(events["max_intensity"], labels)

            if not runs.empty:
                run_annual.append(
                    _annual_run_stats(runs, thresh, labels, cy_range).assign(pct_type=percent_type)
                )
            if not events.empty:
                events["pct_type"] = percent_type
                events_list.append(events)
                event_annual.append(
                    _annual_event_stats(events, labels, cy_range).assign(pct_type=percent_type)
                )

    df_events = pd.concat(events_list, ignore_index=True) if events_list else pd.DataFrame()
    annual_stats = pd.concat(
        [_to_long(event_annual, list(EVENT_ANNUAL_FILL)),
         _to_long(run_annual, list(RUN_ANNUAL_FILL))],
        ignore_index=True,
    )
    annual_stats = annual_stats.sort_values(["site", "cy"]).reset_index(drop=True)
    return df_events, annual_stats


def calculate_site_properties(
    df_pct: pd.DataFrame,
    site_name: str,
    thresholds: List[int] = None,
    percent_type_list: List[str] = None,
    flow_name_list: List[str] = None,
    start_cy: int = 1985,
    end_cy: int = 2016,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Identify drought events separately at each threshold and calculate annual properties.

    Parameters
    ----------
    df_pct : pd.DataFrame
        Percentile dataframe from calculate_site_percentiles().
    site_name : str
        Site identifier (unused; kept for API compatibility).
    thresholds : list of int
        Percentile thresholds. Default: config.THRESHOLDS.
    percent_type_list : list of str
        Percentile column names. Default: config.PCT_TYPES.
    flow_name_list : list of str
        Corresponding flow column names. Default: config.PCT_FLOW_COLS.
    start_cy, end_cy : int
        Climate year range for filling missing years.

    Returns
    -------
    (df_drought_events_all, annual_stats)
        Per-event properties, and long-format annual statistics
        (site, cy, measure, value, threshold, pct_type).
    """
    return _site_properties(
        df_pct,
        id_thresholds=thresholds or THRESHOLDS,
        class_bins=None,
        percent_type_list=percent_type_list or PCT_TYPES,
        flow_name_list=flow_name_list or PCT_FLOW_COLS,
        start_cy=start_cy,
        end_cy=end_cy,
    )


def calculate_site_properties_universal(
    df_pct: pd.DataFrame,
    site_name: str,
    percent_type_list: List[str] = None,
    flow_name_list: List[str] = None,
    start_cy: int = 1985,
    end_cy: int = 2016,
    baseline: int = BASELINE_THRESHOLD,
    class_bins: List[int] = None,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Identify drought events at the baseline percentile and label each with a severity class.

    The events are those of calculate_site_properties at threshold=baseline; all
    characteristics are relative to the baseline. The ``threshold`` column holds the
    severity class (the smallest of class_bins at or above the event's lowest percentile),
    and annual statistics are grouped by class, with every class present for every year.

    Parameters
    ----------
    df_pct, site_name, percent_type_list, flow_name_list, start_cy, end_cy
        As for calculate_site_properties.
    baseline : int
        Identification percentile. Default 30.
    class_bins : list of int
        Ascending severity classes. Default: config.THRESHOLDS.

    Returns
    -------
    (df_drought_events_all, annual_stats)
        As for calculate_site_properties, with ``threshold`` meaning severity class.
    """
    return _site_properties(
        df_pct,
        id_thresholds=[baseline],
        class_bins=class_bins or THRESHOLDS,
        percent_type_list=percent_type_list or PCT_TYPES,
        flow_name_list=flow_name_list or PCT_FLOW_COLS,
        start_cy=start_cy,
        end_cy=end_cy,
    )
