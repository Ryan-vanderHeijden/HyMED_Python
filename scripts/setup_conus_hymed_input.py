"""
setup_conus_hymed_input.py

Prepares per-site HyMED input CSVs for CONUS NWM analysis.

Reads (from --data-dir):
  - streamflow.parquet       : USGS observed daily streamflow
  - nwm_streamflow.parquet   : NWM v3 retrospective daily streamflow

Filters:
  - Approved USGS quality codes: A, A, e, A, R, A, <
  - Sites with >= --min-overlap-years years of obs coverage within the NWM window

Writes:
  - One CSV per site to --out-path (HyMED input_data format)
  - <data-dir>/site_coverage.csv : site-level overlap stats (for threshold tuning)

HyMED input format:
  site, dt, q_cms_obs, q_cms_nwm, jd, year, month, wy, cy

Site coverage table columns:
  site_no, obs_start, obs_end, overlap_start, overlap_end, overlap_years

Usage:
  python scripts/setup_conus_hymed_input.py --data-dir <CONUS_analysis> \\
      --out-path <hymed data dir>/input_data
"""

import argparse
import os

import numpy as np
import pandas as pd
from tqdm import tqdm

NWM_START = pd.Timestamp("1979-01-01")
NWM_END = pd.Timestamp("2023-12-31")
CFS_TO_CMS = 0.0283168
APPROVED_CODES = {"A", "A, e", "A, R", "A, <"}


def _load_obs(data_dir: str, sites) -> pd.DataFrame:
    """Approved, non-missing observed discharge for the given sites."""
    obs = pd.read_parquet(
        os.path.join(data_dir, "streamflow.parquet"),
        columns=["site_no", "date", "discharge_cfs", "discharge_cd"],
    )
    obs["date"] = pd.to_datetime(obs["date"], utc=True).dt.tz_localize(None)
    return obs[
        obs["site_no"].isin(sites)
        & obs["discharge_cd"].isin(APPROVED_CODES)
        & obs["discharge_cfs"].notna()
    ]


def main() -> None:
    ap = argparse.ArgumentParser(description="Prepare HyMED input CSVs for CONUS NWM sites.")
    ap.add_argument("--data-dir", required=True,
                    help="Directory with streamflow.parquet and nwm_streamflow.parquet.")
    ap.add_argument("--out-path", required=True, help="Output directory for per-site CSVs.")
    ap.add_argument("--min-overlap-years", type=float, default=30,
                    help="Minimum years of obs coverage within the NWM window (default 30).")
    args = ap.parse_args()
    data_dir = args.data_dir

    # ── Site coverage analysis ────────────────────────────────────────────────
    print("Loading NWM site list...")
    nwm = pd.read_parquet(os.path.join(data_dir, "nwm_streamflow.parquet"),
                          columns=["site_no", "date"])
    nwm_sites = set(nwm["site_no"].unique())

    print("Loading observed streamflow (site/date/discharge only)...")
    obs_meta = _load_obs(data_dir, nwm_sites)

    print("Computing site-level coverage stats...")
    site_ranges = obs_meta.groupby("site_no")["date"].agg(obs_start="min", obs_end="max").reset_index()
    site_ranges["overlap_start"] = site_ranges["obs_start"].clip(lower=NWM_START)
    site_ranges["overlap_end"] = site_ranges["obs_end"].clip(upper=NWM_END)
    site_ranges["overlap_years"] = (
        (site_ranges["overlap_end"] - site_ranges["overlap_start"]).dt.days / 365.25
    ).clip(lower=0)

    coverage_path = os.path.join(data_dir, "site_coverage.csv")
    site_ranges.to_csv(coverage_path, index=False)
    print(f"Saved site coverage table -> {coverage_path}")

    print("\nSite count by minimum overlap threshold:")
    for cutoff in [10, 15, 20, 25, 30, 40, 44]:
        n = (site_ranges["overlap_years"] >= cutoff).sum()
        print(f"  >= {cutoff:2d} years: {n:,} sites")

    # ── Filter to qualifying sites ────────────────────────────────────────────
    qualifying = site_ranges[
        site_ranges["overlap_years"] >= args.min_overlap_years
    ]["site_no"].tolist()
    print(f"\nUsing >= {args.min_overlap_years} year threshold: {len(qualifying):,} sites")

    os.makedirs(args.out_path, exist_ok=True)

    # ── Load full obs and NWM for qualifying sites only ───────────────────────
    print("Loading full observed streamflow for qualifying sites...")
    obs_full = _load_obs(data_dir, qualifying).copy()
    obs_full["q_cms_obs"] = obs_full["discharge_cfs"] * CFS_TO_CMS
    obs_full = obs_full[["site_no", "date", "q_cms_obs"]]

    print("Loading NWM streamflow for qualifying sites...")
    nwm_cms = pd.read_parquet(
        os.path.join(data_dir, "nwm_streamflow.parquet"),
        columns=["site_no", "date", "streamflow_cms"],
    )
    nwm_cms["date"] = pd.to_datetime(nwm_cms["date"])
    nwm_cms = nwm_cms[nwm_cms["site_no"].isin(qualifying)].rename(
        columns={"streamflow_cms": "q_cms_nwm"}
    )

    # ── Write per-site CSVs ───────────────────────────────────────────────────
    print(f"\nWriting per-site CSVs to:\n  {args.out_path}\n")

    skipped = []
    for site in tqdm(qualifying, desc="Sites"):
        obs_site = obs_full[obs_full["site_no"] == site]
        nwm_site = nwm_cms[nwm_cms["site_no"] == site]

        merged = pd.merge(obs_site, nwm_site, on="date", how="inner")
        merged = merged[(merged["date"] >= NWM_START) & (merged["date"] <= NWM_END)]

        if merged.empty:
            skipped.append(site)
            continue

        merged = merged.sort_values("date").reset_index(drop=True)
        merged["site"] = site
        merged["dt"] = merged["date"]
        merged["jd"] = merged["date"].dt.day_of_year
        merged["year"] = merged["date"].dt.year
        merged["month"] = merged["date"].dt.month
        merged["wy"] = np.where(merged["month"] >= 10, merged["year"] + 1, merged["year"])
        merged["cy"] = np.where(merged["month"] >= 4, merged["year"] + 1, merged["year"])

        out = merged[["site", "dt", "q_cms_obs", "q_cms_nwm", "jd", "year", "month", "wy", "cy"]]
        out.to_csv(os.path.join(args.out_path, f"{site}.csv"), index=False)

    print(f"\nDone. Wrote {len(qualifying) - len(skipped):,} files.")
    if skipped:
        print(f"Skipped {len(skipped)} sites with no inner-join overlap: {skipped}")


if __name__ == "__main__":
    main()
