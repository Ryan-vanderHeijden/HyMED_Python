"""
run_single_site.py

Runs drought evaluation for each site in the input data directory.
Translated from inst/extdata/run_single_site.R.

Usage:
    python run_single_site.py
"""

import os
import glob

import numpy as np
import pandas as pd

from hymed import (
    calculate_site_percentiles,
    calculate_site_properties,
    calculate_site_boolean,
    calculate_site_boolean_threshold_only,
    site_cohens_kappa,
    site_spearmans,
    site_bias_distribution,
    site_annual_signatures,
    gather_data,
)


def run_site_eval(site_name: str, data_path: str, model: str) -> None:
    """
    Run full drought evaluation for a single site.

    Parameters
    ----------
    site_name : str
        USGS station ID (matches CSV filename without extension).
    data_path : str
        Path to the model data directory (e.g. 'inst/extdata/nhm').
    model : str
        Model name string (e.g. 'nhm' or 'nwm').
    """
    # Read input data
    df = pd.read_csv(
        os.path.join(data_path, "input_data", f"{site_name}.csv"),
        dtype={"site": str},
    )
    df["dt"] = pd.to_datetime(df["dt"])

    # Assign q_mod and q_obs
    df["q_mod"] = df[f"q_cms_{model}"]
    df["q_obs"] = df["q_cms_obs"]
    df = df.drop(columns=["q_cms_obs", f"q_cms_{model}"])

    # Derive date/climate-year range from input data
    month = df["dt"].dt.month
    cy = np.where(month >= 4, df["dt"].dt.year + 1, df["dt"].dt.year)
    start_cy = int(cy.min())
    end_cy = int(cy.max())
    sdate = df["dt"].min().strftime("%Y-%m-%d")
    edate = df["dt"].max().strftime("%Y-%m-%d")

    # Calculate percentiles
    df_pct = calculate_site_percentiles(df, site_name, units="cms")
    df_pct.to_csv(
        os.path.join(data_path, "percentiles", f"{site_name}.csv"), index=False
    )

    # Calculate drought events and properties
    df_both = calculate_site_properties(
        df_pct,
        site_name,
        thresholds=[5, 10, 20, 30],
        percent_type_list=[
            "weibull_jd_obs", "weibull_jd_mod",
            "weibull_site_obs", "weibull_site_mod",
        ],
        flow_name_list=["q_obs", "q_mod", "q_obs", "q_mod"],
        start_cy=start_cy,
        end_cy=end_cy,
    )
    df_properties = df_both[0]
    df_annual_stats = df_both[1]

    # Calculate drought booleans (with pooling - for reference)
    df_bool = calculate_site_boolean(df_properties, site_name, sdate, edate)

    # Calculate booleans using threshold-only method (no pooling) - used for kappa
    df_bool_threshold_only = calculate_site_boolean_threshold_only(df_pct)

    # Cohen's kappa
    df_kappa = site_cohens_kappa(df_bool_threshold_only, site_name)
    df_kappa.to_csv(
        os.path.join(data_path, "kappa", f"kappa_{site_name}.csv"), index=False
    )

    # Spearman's rho
    df_spear = site_spearmans(df_pct, site_name)
    df_spear.to_csv(
        os.path.join(data_path, "spearmans", f"spearman_{site_name}.csv"), index=False
    )

    # Bias and SD distribution
    df_bias_dist = site_bias_distribution(df_pct, site_name)
    df_bias_dist.to_csv(
        os.path.join(data_path, "bias_dist", f"bias_dist_{site_name}.csv"), index=False
    )

    # Annual event-based metrics
    df_ann_eval = site_annual_signatures(df_annual_stats, site_name)
    df_ann_eval.to_csv(
        os.path.join(data_path, "ann_eval", f"ann_eval_{site_name}.csv"), index=False
    )


if __name__ == "__main__":
    # Update model name to match the data location
    model_name = "nhm"

    # Path to the data
    data_path = os.path.join("inst", "extdata", model_name)

    # Create output folders if needed
    for folder in ["percentiles", "kappa", "spearmans", "bias_dist", "ann_eval"]:
        os.makedirs(os.path.join(data_path, folder), exist_ok=True)

    # Get list of all sites from input_data directory
    input_files = sorted(glob.glob(os.path.join(data_path, "input_data", "*.csv")))
    site_list = [os.path.splitext(os.path.basename(f))[0] for f in input_files]

    # Run evaluation for each site
    for i, site_name in enumerate(site_list, start=1):
        print(f"site: {site_name} - iteration: {i}")
        run_site_eval(site_name=site_name, data_path=data_path, model=model_name)

    # ---- Gather and reshape kappa data to long format ----
    df_kappa = gather_data("kappa", data_path)
    df_kappa_long = df_kappa.melt(
        id_vars=["pct_type", "threshold", "site"],
        value_vars=["mod_kappa", "mod_classification_accuracy"],
        var_name="temp",
        value_name="value",
    )
    df_kappa_long["source"] = df_kappa_long["temp"].str[:3]
    df_kappa_long["metric"] = df_kappa_long["temp"].str[4:]
    df_kappa_long = df_kappa_long[["site", "threshold", "pct_type", "metric", "source", "value"]]
    df_kappa_long["pct_type"] = df_kappa_long["pct_type"].replace(
        {"jd": "variable", "site": "fixed"}
    )
    df_kappa_long.to_csv(os.path.join(data_path, "kappa_long.csv"), index=False)

    # ---- Gather and reshape Spearman's data to long format ----
    df_spearmans = gather_data("spearmans", data_path)
    df_spearmans = df_spearmans.drop(columns=["quant_length"], errors="ignore")
    df_spearmans = df_spearmans.rename(columns={"rho_obs_mod": "mod_rho_obs"})
    df_spear_long = df_spearmans.melt(
        id_vars=["threshold", "site", "pct_type"],
        value_vars=["mod_rho_obs", "mod_nse"],
        var_name="temp",
        value_name="value",
    )
    df_spear_long["source"] = df_spear_long["temp"].str[:3]
    df_spear_long["metric"] = df_spear_long["temp"].str[4:]
    df_spear_long = df_spear_long.drop(columns=["temp"])

    # ---- Gather and reshape bias/distribution data to long format ----
    df_bias = gather_data("bias_dist", data_path)
    df_bias_long = df_bias.melt(
        id_vars=["thresh", "pct_type", "site"],
        value_vars=["mod_bias", "mod_pct_bias", "mod_sd_ratio"],
        var_name="temp",
        value_name="value",
    )
    df_bias_long["source"] = df_bias_long["temp"].str[:3]
    df_bias_long["metric"] = df_bias_long["temp"].str[4:]
    df_bias_long = df_bias_long.drop(columns=["temp"])
    df_bias_long = df_bias_long.rename(columns={"thresh": "threshold"})

    # Combine Spearman's and bias/distribution
    df_spear_bias_dist = pd.concat([df_spear_long, df_bias_long], ignore_index=True)
    df_spear_bias_dist = df_spear_bias_dist[
        ["site", "threshold", "source", "metric", "pct_type", "value"]
    ].sort_values(["site", "threshold"]).reset_index(drop=True)
    df_spear_bias_dist["pct_type"] = df_spear_bias_dist["pct_type"].replace(
        {"weibull_site": "fixed", "weibull_jd": "variable"}
    )
    df_spear_bias_dist.to_csv(
        os.path.join(data_path, "spear_bias_dist_long.csv"), index=False
    )

    # ---- Gather and reshape annual evaluation data to long format ----
    df_ann = gather_data("ann_eval", data_path)
    # Pivot value columns to long format (NSE_mod through nmae_mod)
    value_cols = [c for c in df_ann.columns if c.endswith("_mod")]
    id_cols = [c for c in df_ann.columns if c not in value_cols]
    df_ann_long = df_ann.melt(
        id_vars=id_cols,
        value_vars=value_cols,
        var_name="temp",
        value_name="value",
    )
    # Split 'NSE_mod' → metric='NSE', source='mod'
    split = df_ann_long["temp"].str.rsplit("_", n=1, expand=True)
    df_ann_long["metric"] = split[0]
    df_ann_long["source"] = split[1]
    df_ann_long = df_ann_long.drop(columns=["temp"])
    df_ann_long["type"] = df_ann_long["type"].replace({"jd": "variable", "site": "fixed"})
    df_ann_long.to_csv(os.path.join(data_path, "ann_eval_long.csv"), index=False)
