"""
calculate_properties_universal.py

Identifies drought events and calculates their properties using a "universal"
classification scheme.

Difference from calculate_properties.py
---------------------------------------
The original `calculate_site_properties` applies each percentile threshold
(30, 20, 10, 5) independently: drought events are re-identified and re-characterized
for every threshold, and each event's severity/deficit is measured *relative to that
threshold* (e.g. ``sum(5 - pct)``). This produces the counter-intuitive result that
more extreme thresholds report smaller deficits, because both the qualifying window
and the per-day deficit shrink as the threshold drops.

This module instead:

1. Uses the 30th percentile as the single baseline threshold to identify drought
   event onset and boundaries (and to drive inter-event pooling).
2. Tracks the minimum percentile reached during each event and uses it to assign a
   single severity *classification* (5 / 10 / 20 / 30).
3. Computes every characteristic (duration, severity/deficit, intensity, flow volume,
   ...) relative to the 30th-percentile baseline, regardless of the event's class.

The result: every drought event has exactly one classification (determined by how far
streamflow dropped), but all metrics are computed against a consistent 30th-percentile
baseline.

Output format
-------------
To mirror the existing workflow, the returned DataFrames keep the same column names
as ``calculate_site_properties``. In particular the ``threshold`` column now holds the
event's *severity classification* rather than the identification threshold. This lets
the downstream functions (``site_annual_signatures``, the gather/reshape steps in the
run script, etc.) be reused without modification.
"""

from typing import List, Tuple

import numpy as np
import pandas as pd

from ._util import climate_year, decimal_date, rleid
from .config import (
    MIN_EVENT_DURATION, PCT_FLOW_COLS, PCT_TYPES, POOLING_SEVERITY_RATIO, THRESHOLDS,
)

# Single baseline threshold (percentile) used to identify drought onset/boundaries
# and against which all characteristics are computed.
BASELINE_THRESHOLD = 30

# Severity classification bins, ordered from most to least extreme. An event is
# assigned the first (smallest) bin whose value is >= the minimum percentile reached.
CLASS_BINS = THRESHOLDS


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
    Identify drought events at the baseline (30th) percentile and characterize them
    using the universal classification scheme.

    Parameters
    ----------
    df_pct : pd.DataFrame
        Percentile dataframe from calculate_site_percentiles().
    site_name : str
        Site identifier.
    percent_type_list : list of str
        Percentile column names. Default: weibull_jd_obs/mod and weibull_site_obs/mod.
    flow_name_list : list of str
        Corresponding flow column names.
    start_cy, end_cy : int
        Climate year range for filling missing years.
    baseline : int
        Baseline percentile used to identify event onset/boundaries and against which
        all characteristics are computed. Default 30.
    class_bins : list of int
        Severity classification bins (most to least extreme). Default [5, 10, 20, 30].

    Returns
    -------
    [df_drought_events_all, annual_stats] : list of two DataFrames

        ``df_drought_events_all`` is per-event, with the same columns as
        ``calculate_site_properties`` plus the ``threshold`` column holding the event's
        severity *classification*.

        ``annual_stats`` is the long-format annual aggregation keyed by
        (site, cy, measure, value, threshold, pct_type), where ``threshold`` again
        denotes the severity classification.
    """
    if percent_type_list is None:
        percent_type_list = PCT_TYPES
    if flow_name_list is None:
        flow_name_list = PCT_FLOW_COLS
    if class_bins is None:
        class_bins = list(CLASS_BINS)

    inter_event_duration = MIN_EVENT_DURATION
    pooling_severity_ratio = POOLING_SEVERITY_RATIO

    low_flow_list = []
    drought_event_list = []

    cy_range = list(range(start_cy, end_cy + 1))

    def classify(min_pct: float) -> int:
        """Most extreme class reached: the smallest bin >= min_pct (else the last bin)."""
        for c in class_bins:
            if min_pct <= c:
                return c
        return class_bins[-1]

    for h, (percent_type, flow_name) in enumerate(zip(percent_type_list, flow_name_list)):
        # Determine grouping dimension from percent_type (jd or site)
        pct_parts = percent_type.split("_")
        group_dim = pct_parts[1]  # 'jd' or 'site'

        df_sub = df_pct.copy()
        df_sub["flow_value"] = df_sub[flow_name]
        df_sub["value"] = df_sub[percent_type]
        # Onset/boundaries are defined by the single baseline threshold.
        df_sub["less_than_thresh"] = (df_sub["value"] <= baseline).astype(int)

        # Sort and assign drought_id via rleid
        df_sub = df_sub.sort_values(["site", "dt"]).reset_index(drop=True)
        df_sub["drought_id"] = rleid(df_sub["less_than_thresh"])

        # Compute flow threshold (quantile of flow_value at baseline/100) per group
        group_col = "jd" if group_dim == "jd" else "site"
        flow_thresh_map = (
            df_sub.groupby(group_col)["flow_value"]
            .quantile(baseline / 100.0)
            .rename("flow_thresh")
        )
        df_sub = df_sub.join(flow_thresh_map, on=group_col)

        # ---- Low-flow statistics (all periods below the baseline) ----
        df_below = df_sub[df_sub["value"] <= baseline].copy()

        if not df_below.empty:
            low_flow_by_event = (
                df_below.groupby(["site", "cy", "drought_id"])
                .agg(
                    days=("value", "count"),
                    lowest_percent=("value", "min"),
                    severity=("value", lambda x: abs(np.sum(baseline - x))),
                    lowest_flow=("flow_value", "min"),
                    flow_volume=pd.NamedAgg(
                        column="flow_value",
                        aggfunc=lambda fv: np.sum(
                            df_sub.loc[fv.index, "flow_thresh"] - fv
                        ),
                    ),
                )
                .reset_index()
            )

            # Classify each below-baseline run by the minimum percentile it reached.
            low_flow_by_event["classification"] = (
                low_flow_by_event["lowest_percent"].apply(classify)
            )

            low_flow_by_cy = (
                low_flow_by_event.groupby(["site", "cy", "classification"])
                .agg(
                    total_duration_below_threshold=("days", "sum"),
                    longest_duration_below_threshold=("days", "max"),
                    total_severity_below_threshold=("severity", "sum"),
                    largest_severity_below_threshold=("severity", "max"),
                    maximum_drought_intensity=("lowest_percent", lambda x: baseline - x.min()),
                    minimum_flow_cms=("lowest_flow", "min"),
                    total_flow_volume_below_threshold_cms=("flow_volume", "sum"),
                    largest_flow_volume_below_threshold_cms=("flow_volume", "max"),
                )
                .reset_index()
            )

            # Fill missing (climate year x classification) combinations with zeros so
            # every site/class has a complete annual record (mirrors the original,
            # where each threshold carried a full climate-year range).
            sites = low_flow_by_cy["site"].unique()
            fill_vals = {
                "total_duration_below_threshold": 0,
                "longest_duration_below_threshold": 0,
                "total_severity_below_threshold": 0,
                "largest_severity_below_threshold": 0,
                "maximum_drought_intensity": 0,
                "minimum_flow_cms": np.nan,
                "total_flow_volume_below_threshold_cms": 0,
                "largest_flow_volume_below_threshold_cms": 0,
            }
            complete_idx = pd.MultiIndex.from_product(
                [sites, cy_range, class_bins], names=["site", "cy", "classification"]
            )
            low_flow_by_cy = (
                low_flow_by_cy.set_index(["site", "cy", "classification"])
                .reindex(complete_idx)
                .fillna(fill_vals)
                .reset_index()
            )
            # 'threshold' column holds the severity classification (mirrors output format).
            low_flow_by_cy["threshold"] = low_flow_by_cy["classification"].astype(int)
            low_flow_by_cy = low_flow_by_cy.drop(columns=["classification"])
            low_flow_by_cy["pct_type"] = percent_type

            low_flow_list.append(low_flow_by_cy)

        # ---- Drought event identification with pooling (baseline threshold) ----
        df_sub["feature_event_id"] = df_sub["site"] + "_" + df_sub["drought_id"].astype(str)

        # Summarize each run (drought + inter-event)
        ic_summary = (
            df_sub.groupby(["site", "drought_id"])
            .agg(
                duration=("value", "count"),
                drought_bool=("less_than_thresh", "mean"),
                severity=("value", lambda x: abs(np.sum(baseline - x))),
            )
            .reset_index()
        )
        ic_summary = ic_summary.sort_values(["site", "drought_id"]).reset_index(drop=True)
        ic_summary["previous_duration"] = ic_summary.groupby("site")["duration"].shift(1)
        ic_summary["previous_severity"] = ic_summary.groupby("site")["severity"].shift(1)
        ic_summary["severity_ratio"] = ic_summary["severity"] / ic_summary["previous_severity"]

        # Inter-event periods to remove (pool into surrounding droughts)
        ic_to_remove = ic_summary[
            (
                (
                    (ic_summary["duration"] < inter_event_duration)
                    & (ic_summary["previous_duration"] > ic_summary["duration"])
                )
                | (ic_summary["severity_ratio"] < pooling_severity_ratio)
            )
            & (ic_summary["drought_bool"] == 0)
        ].copy()
        ic_to_remove["feature_event_id"] = (
            ic_to_remove["site"] + "_" + ic_to_remove["drought_id"].astype(str)
        )

        # Remove inter-event periods and re-assign drought IDs
        df_pooled = df_sub[
            ~df_sub["feature_event_id"].isin(ic_to_remove["feature_event_id"])
        ].copy()
        df_pooled = df_pooled.sort_values(["site", "dt"]).reset_index(drop=True)
        df_pooled["drought_id"] = rleid(df_pooled["less_than_thresh"])

        # Keep only drought (below baseline) periods
        df_events = df_pooled[df_pooled["less_than_thresh"] == 1].copy()

        if df_events.empty:
            continue

        event_stats = (
            df_events.groupby(["site", "drought_id"])
            .agg(
                severity=("value", lambda x: np.sum(baseline - x)),
                mean_intensity=("value", "mean"),
                max_intensity=("value", "min"),
                duration=("value", "count"),
                start=("dt", "first"),
                end=("dt", "last"),
                min_flow_cms=("flow_value", "min"),
                mean_flow_cms=("flow_value", "mean"),
                max_flow_cms=("flow_value", "max"),
                flow_volume_cms=pd.NamedAgg(
                    column="flow_value",
                    aggfunc=lambda fv: np.sum(
                        df_pooled.loc[fv.index, "flow_thresh"] - fv
                    ),
                ),
            )
            .reset_index()
        )

        # Assign a single severity classification from the minimum percentile reached.
        # ``max_intensity`` holds the minimum percentile (deepest point) of the event.
        event_stats["classification"] = event_stats["max_intensity"].apply(classify)

        # Re-number drought IDs as dense rank within site
        event_stats["drought_id"] = event_stats.groupby("site")["drought_id"].rank(
            method="dense"
        ).astype(int)

        # Filter events shorter than minimum duration
        event_stats = event_stats[event_stats["duration"] >= inter_event_duration].copy()

        if event_stats.empty:
            continue

        event_stats["cy"] = climate_year(event_stats["start"])
        # 'threshold' column holds the severity classification (mirrors output format).
        event_stats["threshold"] = event_stats["classification"].astype(int)
        event_stats = event_stats.drop(columns=["classification"])
        event_stats["pct_type"] = percent_type

        drought_event_list.append(event_stats)

    # ---- Combine all iterations ----
    df_drought_events_all = (
        pd.concat(drought_event_list, ignore_index=True) if drought_event_list else pd.DataFrame()
    )
    df_low_flows_all = (
        pd.concat(low_flow_list, ignore_index=True) if low_flow_list else pd.DataFrame()
    )

    # ---- Annual aggregation of drought events -> long format ----
    if not df_drought_events_all.empty:
        drought_annual = _compute_longest_event_cols(
            df_drought_events_all, start_cy, end_cy, class_bins
        )
    else:
        drought_annual = pd.DataFrame()

    # ---- Low-flow long format ----
    low_flow_cols = [
        "total_duration_below_threshold",
        "longest_duration_below_threshold",
        "total_severity_below_threshold",
        "largest_severity_below_threshold",
        "maximum_drought_intensity",
        "minimum_flow_cms",
        "total_flow_volume_below_threshold_cms",
        "largest_flow_volume_below_threshold_cms",
    ]
    if not df_low_flows_all.empty:
        df_low_flow_long = df_low_flows_all.melt(
            id_vars=["site", "cy", "threshold", "pct_type"],
            value_vars=low_flow_cols,
            var_name="measure",
            value_name="value",
        )[["site", "cy", "measure", "value", "threshold", "pct_type"]]
    else:
        df_low_flow_long = pd.DataFrame()

    # Combine
    annual_stats = pd.concat([drought_annual, df_low_flow_long], ignore_index=True)
    annual_stats = annual_stats.sort_values(["site", "cy"]).drop_duplicates().reset_index(drop=True)

    return [df_drought_events_all, annual_stats]


def _compute_longest_event_cols(
    df_drought_events_all: pd.DataFrame,
    start_cy: int,
    end_cy: int,
    class_bins: List[int],
) -> pd.DataFrame:
    """
    Aggregate drought events to annual level (per severity classification) and fill
    missing (climate year x classification) combinations. Returns a long-format
    DataFrame keyed by (site, cy, measure, value, threshold, pct_type).
    """
    records = []
    for (site, cy, threshold, pct_type), grp in df_drought_events_all.groupby(
        ["site", "cy", "threshold", "pct_type"]
    ):
        idx_longest = grp["duration"].idxmax()
        start_longest = grp.loc[idx_longest, "start"]
        end_longest = grp.loc[idx_longest, "end"]
        start_day = start_longest.dayofyear
        end_day = start_day + grp.loc[idx_longest, "duration"]

        records.append({
            "site": site,
            "cy": cy,
            "threshold": threshold,
            "pct_type": pct_type,
            "total_drought_severity": grp["severity"].sum(),
            "total_drought_duration": grp["duration"].sum(),
            "number_of_drought_events": len(grp),
            "largest_drought_severity": grp["severity"].max(),
            "longest_drought_duration": grp["duration"].max(),
            "total_drought_flow_volume_cms": grp["flow_volume_cms"].sum(),
            "longest_drought_flow_volume_cms": grp["flow_volume_cms"].max(),
            "start_day_of_longest_drought": start_day,
            "start_date_of_longest_drought": decimal_date(start_longest),
            "end_date_of_longest_drought": decimal_date(end_longest),
            "end_day_of_longest_drought": end_day,
        })

    if not records:
        return pd.DataFrame()

    agg_df = pd.DataFrame(records)

    # Fill missing climate years / classifications with zeros
    fill_event = {
        "total_drought_severity": 0,
        "total_drought_duration": 0,
        "number_of_drought_events": 0,
        "largest_drought_severity": 0,
        "longest_drought_duration": 0,
        "total_drought_flow_volume_cms": 0,
        "longest_drought_flow_volume_cms": 0,
        "start_date_of_longest_drought": np.nan,
        "start_day_of_longest_drought": np.nan,
        "end_date_of_longest_drought": np.nan,
        "end_day_of_longest_drought": np.nan,
    }

    cy_range = list(range(start_cy, end_cy + 1))
    # Seed every (site, pct_type) with the full set of classification bins so that
    # obs/mod records line up across classes in the downstream annual signatures.
    site_pct = agg_df[["site", "pct_type"]].drop_duplicates()
    complete_dfs = []
    for _, row in site_pct.iterrows():
        sub_all = agg_df[
            (agg_df["site"] == row["site"]) & (agg_df["pct_type"] == row["pct_type"])
        ]
        for cls in class_bins:
            sub = sub_all[sub_all["threshold"] == cls].set_index("cy")
            full_idx = pd.Index(cy_range, name="cy")
            sub = sub.reindex(full_idx).fillna(fill_event).reset_index()
            sub["site"] = row["site"]
            sub["threshold"] = cls
            sub["pct_type"] = row["pct_type"]
            complete_dfs.append(sub)

    if not complete_dfs:
        return pd.DataFrame()

    agg_full = pd.concat(complete_dfs, ignore_index=True)

    # Melt to long format
    value_cols = [
        "total_drought_severity", "total_drought_duration", "number_of_drought_events",
        "largest_drought_severity", "longest_drought_duration",
        "total_drought_flow_volume_cms", "longest_drought_flow_volume_cms",
        "start_day_of_longest_drought", "start_date_of_longest_drought",
        "end_date_of_longest_drought", "end_day_of_longest_drought",
    ]
    long_df = agg_full.melt(
        id_vars=["site", "cy", "threshold", "pct_type"],
        value_vars=value_cols,
        var_name="measure",
        value_name="value",
    )[["site", "cy", "measure", "value", "threshold", "pct_type"]]

    return long_df
