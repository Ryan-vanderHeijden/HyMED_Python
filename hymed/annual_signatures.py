"""
annual_signatures.py

Compare modeled to observed annual drought signatures and calculate evaluation metrics.
Translated from R/site_annual_signature_metrics.R.
"""

import numpy as np
import pandas as pd
from scipy import stats


def _nse(sim: np.ndarray, obs: np.ndarray) -> float:
    """Nash-Sutcliffe Efficiency."""
    mask = ~np.isnan(obs) & ~np.isnan(sim)
    obs, sim = obs[mask], sim[mask]
    denom = np.sum((obs - obs.mean()) ** 2)
    if denom == 0:
        return np.nan
    return float(1.0 - np.sum((obs - sim) ** 2) / denom)


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
        Columns: site, measure, threshold, type, NSE_mod, pearson_mod, spearman_mod,
                 kendall_mod, r2_mod, mse_mod, rmse_mod, mspe_mod, mae_mod, mean_obs, nmae_mod.
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
    df = df[df["threshold"].isin([5, 10, 20, 30])]

    # Pivot wide: obs and mod as separate columns
    df_wide = df.pivot_table(
        index=["site", "cy", "measure", "threshold", "type"],
        columns="source",
        values="value",
        aggfunc="first",
    ).reset_index()
    df_wide.columns.name = None

    if "obs" not in df_wide.columns or "mod" not in df_wide.columns:
        return pd.DataFrame()

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
        nse_val = _nse(mod_m, obs_m)
        pearson = float(np.corrcoef(obs_m, mod_m)[0, 1]) if len(obs_m) > 1 else np.nan
        spearman = float(stats.spearmanr(obs_m, mod_m).statistic)
        kendall = float(stats.kendalltau(obs_m, mod_m).statistic)
        r2 = pearson ** 2 if not np.isnan(pearson) else np.nan
        mse = float(np.mean((obs_m - mod_m) ** 2))
        rmse = float(np.sqrt(mse))
        mspe = (rmse / mean_obs * 100) if mean_obs != 0 else np.nan
        mae = float(np.mean(np.abs(obs_m - mod_m)))
        nmae = mae / mean_obs if mean_obs != 0 else np.nan

        records.append({
            "site": site,
            "measure": measure,
            "threshold": threshold,
            "type": type_,
            "NSE_mod": nse_val,
            "pearson_mod": pearson,
            "spearman_mod": spearman,
            "kendall_mod": kendall,
            "r2_mod": r2,
            "mse_mod": mse,
            "rmse_mod": rmse,
            "mspe_mod": mspe,
            "mae_mod": mae,
            "mean_obs": mean_obs,
            "nmae_mod": nmae,
        })

    return pd.DataFrame(records)
