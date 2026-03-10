"""
setup_files.py

Combine paired observation and model output files, perform QA/QC,
and write individual per-site CSV files.
Translated from R/setup_individual_files.R.
"""

import os

import numpy as np
import pandas as pd


def setup_individual_files(
    input_path_model1: str,
    input_path_model2: str,
    output_path: str,
    start_cy: int = 1985,
    end_cy: int = 2016,
) -> None:
    """
    Combine two model output files and write one CSV per site.

    Parameters
    ----------
    input_path_model1, input_path_model2 : str
        Paths to space-delimited model output files.
        Expected columns: siteno, UTC_date, q_cms_obs, q_cms_<model>.
    output_path : str
        Directory where individual site CSVs will be written.
    start_cy, end_cy : int
        Climate year range for filtering.
    """
    df1 = pd.read_csv(input_path_model1, delim_whitespace=True, dtype={"siteno": str})
    df2 = pd.read_csv(input_path_model2, delim_whitespace=True, dtype={"siteno": str})

    # Validate
    for df, label in [(df1, "model1"), (df2, "model2")]:
        assert pd.api.types.is_numeric_dtype(df["q_cms_obs"]), \
            f"q_cms_obs must be numeric in {label}"
        assert pd.api.types.is_string_dtype(df["siteno"]), \
            f"siteno must be character in {label}"
        df["UTC_date"] = pd.to_datetime(df["UTC_date"])

    # Full join; keep obs values from model1
    df_all = pd.merge(df1, df2, on=["siteno", "UTC_date"], how="outer", suffixes=("_m1", "_m2"))
    df_all = df_all.drop(columns=["q_cms_obs_m2"], errors="ignore")
    df_all = df_all.rename(columns={"q_cms_obs_m1": "q_cms_obs"})

    # Add timing metadata
    df_all = df_all.rename(columns={"UTC_date": "dt", "siteno": "site"})
    df_all["jd"] = df_all["dt"].dt.dayofyear
    df_all["year"] = df_all["dt"].dt.year
    df_all["month"] = df_all["dt"].dt.month
    df_all["wy"] = np.where(df_all["month"] >= 10, df_all["year"] + 1, df_all["year"])
    df_all["cy"] = np.where(df_all["month"] >= 4, df_all["year"] + 1, df_all["year"])

    # Filter to climate year range
    df_all = df_all[(df_all["cy"] >= start_cy) & (df_all["cy"] <= end_cy)]

    # Audit: require at least 50% observation coverage (full length = 11688 days)
    full_length = 11688
    df_audit = (
        df_all.dropna(subset=["q_cms_obs"])
        .groupby("site")["q_cms_obs"]
        .count()
        .reset_index(name="obs_length")
    )
    valid_sites = df_audit[df_audit["obs_length"] >= full_length / 2]["site"]
    df_all = df_all[df_all["site"].isin(valid_sites)]

    os.makedirs(output_path, exist_ok=True)

    for site_name in df_all["site"].unique():
        df_site = df_all[df_all["site"] == site_name]
        df_site.to_csv(os.path.join(output_path, f"{site_name}.csv"), index=False)
        print(site_name)
