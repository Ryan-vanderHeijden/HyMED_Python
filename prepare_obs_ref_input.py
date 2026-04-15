"""
prepare_obs_ref_input.py

Prepares observed-only HyMED input CSVs for GAGES-II reference sites.

Reads streamflow.parquet and GAGES-II_ref_non_ref.csv from DATA_DIR,
filters to reference sites (Ref == 1), applies USGS quality-code filtering,
and writes one CSV per site in HyMED input format to OUTPUT_DIR.

Output columns:
  site, dt, q_cms_obs, q_cms_obs_ref, jd, year, month, wy, cy

Usage:
    python prepare_obs_ref_input.py
    python prepare_obs_ref_input.py --data-dir /path/to/CONUS_analysis
    python prepare_obs_ref_input.py --out-path inst/extdata/obs_ref/input_data
    python prepare_obs_ref_input.py --min-years 10
"""

import os
import argparse

import numpy as np
import pandas as pd
from tqdm import tqdm

# ── Defaults ──────────────────────────────────────────────────────────────────

_CONUS_ANALYSIS = (
    "/Users/ryanvan/Library/CloudStorage/OneDrive-UniversityofVermont"
    "/Documents/_UVM/Research/CIROH/_CRB Study/CONUS_analysis"
)

_DEFAULT_OUT = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "inst", "extdata", "obs", "input_data",
)

OBS_WINDOW_START = pd.Timestamp("1979-01-01")
OBS_WINDOW_END   = pd.Timestamp("2023-12-31")
CFS_TO_CMS       = 0.0283168
APPROVED_CODES   = {"A", "A, e", "A, R", "A, <"}


def prepare(data_dir: str, out_path: str, min_years: float) -> None:
    sf_path  = os.path.join(data_dir, "streamflow.parquet")
    ref_path = os.path.join(data_dir, "GAGES-II_ref_non_ref.csv")

    # ── Reference site list ────────────────────────────────────────────────────
    print("Loading GAGES-II reference site list...")
    ref_df   = pd.read_csv(ref_path, dtype={"site": str})
    ref_df["site"] = ref_df["site"].str.zfill(8)
    ref_sites = set(ref_df.loc[ref_df["Ref"] == 1, "site"])
    print(f"  {len(ref_sites):,} reference sites")

    # ── Load and filter streamflow ─────────────────────────────────────────────
    print("Loading observed streamflow...")
    obs = pd.read_parquet(
        sf_path,
        columns=["site_no", "date", "discharge_cfs", "discharge_cd"],
    )
    obs["date"] = pd.to_datetime(obs["date"], utc=True).dt.tz_localize(None)
    obs = obs[
        obs["site_no"].isin(ref_sites)
        & obs["discharge_cd"].isin(APPROVED_CODES)
        & obs["discharge_cfs"].notna()
        & (obs["date"] >= OBS_WINDOW_START)
        & (obs["date"] <= OBS_WINDOW_END)
    ].copy()
    obs["q_cms_obs"] = obs["discharge_cfs"] * CFS_TO_CMS

    qualifying_sites = obs["site_no"].unique().tolist()
    print(f"  {len(qualifying_sites):,} reference sites with approved data in window")

    # ── Optional minimum-coverage filter ──────────────────────────────────────
    if min_years > 0:
        coverage = (
            obs.groupby("site_no")["date"]
            .agg(obs_start="min", obs_end="max")
            .reset_index()
        )
        coverage["years"] = (coverage["obs_end"] - coverage["obs_start"]).dt.days / 365.25
        qualifying_sites = coverage.loc[
            coverage["years"] >= min_years, "site_no"
        ].tolist()
        obs = obs[obs["site_no"].isin(qualifying_sites)]
        print(f"  {len(qualifying_sites):,} sites after >= {min_years} year filter")

    # ── Write per-site CSVs ────────────────────────────────────────────────────
    os.makedirs(out_path, exist_ok=True)
    print(f"\nWriting per-site CSVs to:\n  {out_path}\n")

    skipped = []
    for site in tqdm(qualifying_sites, desc="Sites"):
        df = obs[obs["site_no"] == site].copy()
        if df.empty:
            skipped.append(site)
            continue

        df = df.sort_values("date").reset_index(drop=True)
        df["site"]         = int(site)
        df["dt"]           = df["date"].dt.strftime("%Y-%m-%d")
        df["jd"]           = df["date"].dt.day_of_year
        df["year"]         = df["date"].dt.year
        df["month"]        = df["date"].dt.month
        df["wy"]           = np.where(df["month"] >= 10, df["year"] + 1, df["year"])
        df["cy"]           = np.where(df["month"] >= 4,  df["year"] + 1, df["year"])
        df["q_cms_obs_ref"] = df["q_cms_obs"]

        out = df[["site", "dt", "q_cms_obs", "q_cms_obs_ref", "jd", "year", "month", "wy", "cy"]]
        out.to_csv(os.path.join(out_path, f"{site}.csv"), index=False)

    written = len(qualifying_sites) - len(skipped)
    print(f"\nDone. Wrote {written:,} files.")
    if skipped:
        print(f"Skipped {len(skipped)} empty sites: {skipped}")
    print("Run evaluation with:")
    print("  python run_single_site.py --model obs_ref --data-path inst/extdata/obs")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Prepare observed-only HyMED input CSVs for GAGES-II reference sites."
    )
    parser.add_argument(
        "--data-dir",
        default=_CONUS_ANALYSIS,
        help="Directory containing streamflow.parquet and GAGES-II_ref_non_ref.csv",
    )
    parser.add_argument(
        "--out-path",
        default=_DEFAULT_OUT,
        help="Output directory for per-site CSVs (default: inst/extdata/obs/input_data)",
    )
    parser.add_argument(
        "--min-years",
        type=float,
        default=0,
        help="Minimum years of approved data required (default: 0 = no filter)",
    )
    args = parser.parse_args()
    prepare(data_dir=args.data_dir, out_path=args.out_path, min_years=args.min_years)
