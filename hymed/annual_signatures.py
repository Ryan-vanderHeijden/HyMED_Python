"""
annual_signatures.py

Compare modeled to observed annual drought signatures and calculate evaluation metrics.
Translated from R/site_annual_signature_metrics.R.
"""

import numpy as np
import pandas as pd
from scipy import stats

from ._util import metric_rows, nse
from .config import METRIC_COLS, PCT_TYPE_LABELS, THRESHOLDS

# config.METRIC_COLS with the annual drought property being compared.
ANN_EVAL_COLS = METRIC_COLS[:3] + ["measure"] + METRIC_COLS[3:]


def site_annual_signatures(df_annual_stats: pd.DataFrame, site_name: str) -> pd.DataFrame:
    """
    Calculate annual drought signature evaluation metrics.

    Parameters
    ----------
    df_annual_stats : pd.DataFrame
        Annual statistics from calculate_site_properties() (second element).
        Columns: site, cy, measure, value, threshold, pct_type.
    site_name : str
        Site identifier.

    Returns
    -------
    pd.DataFrame
        Long format (ANN_EVAL_COLS): site, pct_type ('variable'/'fixed'), threshold,
        measure, metric, source, value. Metrics comparing modeled to observed annual
        values have source 'mod' (NSE, pearson, spearman, kendall, r2, mse, rmse, mspe,
        mae, nmae); the observed annual mean is metric 'mean' with source 'obs'.
        (site, pct_type, threshold, measure) groups with fewer than two years are omitted.
    """
    measure_list = [
        "total_duration_below_threshold",
        "total_severity_below_threshold",
        "total_flow_volume_below_threshold_cms",
        "number_of_drought_events",
        "maximum_drought_intensity",
    ]

    # Filter to measures of interest
    df = df_annual_stats[df_annual_stats["measure"].isin(measure_list)].copy()
    df["value"] = df["value"].fillna(0)

    # Split pct_type 'weibull_jd_obs' → weibull, type='jd', source='obs'
    split = df["pct_type"].str.split("_", expand=True)
    df["weibull"] = split[0]
    df["type"] = split[1]
    df["source"] = split[2]

    # Filter to valid thresholds
    df = df[df["threshold"].isin(THRESHOLDS)]

    # Pivot wide: obs and mod as separate columns
    df_wide = df.pivot_table(
        index=["site", "cy", "measure", "threshold", "type"],
        columns="source",
        values="value",
        aggfunc="first",
    ).reset_index()
    df_wide.columns.name = None

    if "obs" not in df_wide.columns or "mod" not in df_wide.columns:
        return pd.DataFrame(columns=ANN_EVAL_COLS)

    records = []
    for (site, measure, threshold, type_), grp in df_wide.groupby(
        ["site", "measure", "threshold", "type"]
    ):
        obs = grp["obs"].values.astype(float)
        mod = grp["mod"].values.astype(float)

        mask = ~np.isnan(obs) & ~np.isnan(mod)
        obs_m, mod_m = obs[mask], mod[mask]

        if len(obs_m) < 2:
            continue

        mean_obs = float(np.nanmean(obs_m))
        nse_val = nse(mod_m, obs_m)
        pearson = float(np.corrcoef(obs_m, mod_m)[0, 1])
        spearman = float(stats.spearmanr(obs_m, mod_m).statistic)
        kendall = float(stats.kendalltau(obs_m, mod_m).statistic)
        r2 = pearson ** 2 if not np.isnan(pearson) else np.nan
        mse = float(np.mean((obs_m - mod_m) ** 2))
        rmse = float(np.sqrt(mse))
        mspe = (rmse / mean_obs * 100) if mean_obs != 0 else np.nan
        mae = float(np.mean(np.abs(obs_m - mod_m)))
        nmae = mae / mean_obs if mean_obs != 0 else np.nan

        key = {
            "site": site,
            "pct_type": PCT_TYPE_LABELS[type_],
            "threshold": threshold,
            "measure": measure,
        }
        records += metric_rows(key, {
            "NSE": nse_val,
            "pearson": pearson,
            "spearman": spearman,
            "kendall": kendall,
            "r2": r2,
            "mse": mse,
            "rmse": rmse,
            "mspe": mspe,
            "mae": mae,
            "nmae": nmae,
        })
        records += metric_rows(key, {"mean": mean_obs}, source="obs")

    return pd.DataFrame(records, columns=ANN_EVAL_COLS)
