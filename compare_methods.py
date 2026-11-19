"""
compare_methods.py

Compare drought-event properties between the two identification methods:

  * "original"  - each percentile threshold (5/10/20/30) applied independently;
                  events re-identified per threshold, deficits measured vs that
                  threshold (hymed/calculate_properties.py).
  * "universal" - single 30th-percentile baseline identifies events; each event
                  gets one severity class (5/10/20/30) from its minimum percentile;
                  all metrics measured vs the 30th-percentile baseline
                  (calculate_site_properties_universal in hymed/calculate_properties.py).

The ``threshold`` column means *identification threshold* in the original output and
*severity classification* in the universal output. Both share the same event schema,
so they are compared on equal footing per (pct_type, threshold) bin.

Outputs (written to --out):
  cmp_aggregate_by_threshold.csv      headline rollup, per method x pct_type x threshold
  cmp_per_site_by_threshold.csv       per site x pct_type x threshold x method (for plots)
  cmp_equivalence_per_site.csv        original threshold=30 vs universal (all classes),
                                      per site x pct_type, to validate that the two
                                      methods identify the *same* below-30 event set.

Usage:
  python compare_methods.py \
    --orig /home/ryan/data/CONUS_drought_props/obs_ref_only/drought_props \
    --univ /home/ryan/data/CONUS_drought_props/obs_ref_only_universal/obs_drought_props_long.csv \
    --out  /home/ryan/data/CONUS_drought_props/method_comparison
"""

import argparse
import glob
import os

import numpy as np
import pandas as pd

# Per-event columns used in the comparison.
EVENT_COLS = ["site", "duration", "severity", "max_intensity", "flow_volume_cms",
              "threshold", "pct_type"]


def load_events(path: str) -> pd.DataFrame:
    """Load drought events from a concatenated long CSV or a directory of per-site CSVs.

    Empty per-site files (sites with no drought events) are skipped.
    """
    if os.path.isfile(path):
        df = pd.read_csv(path, dtype={"site": str})
        return df
    files = sorted(glob.glob(os.path.join(path, "*.csv")))
    if not files:
        raise FileNotFoundError(f"No CSVs found at {path!r}")
    dfs = []
    for f in files:
        try:
            dfs.append(pd.read_csv(f, dtype={"site": str}))
        except pd.errors.EmptyDataError:
            pass  # site with no drought events
    return pd.concat(dfs, ignore_index=True)


def aggregate_by_threshold(events: pd.DataFrame) -> pd.DataFrame:
    """Roll up events to method x pct_type x threshold.

    ``max_intensity`` is the minimum percentile reached (event depth); we report its
    mean as ``mean_min_percentile``.
    """
    g = events.groupby(["method", "pct_type", "threshold"])
    out = g.agg(
        n_sites=("site", "nunique"),
        n_events=("severity", "size"),
        total_severity=("severity", "sum"),
        mean_severity=("severity", "mean"),
        total_duration=("duration", "sum"),
        mean_duration=("duration", "mean"),
        total_deficit_volume_cms=("flow_volume_cms", "sum"),
        mean_deficit_volume_cms=("flow_volume_cms", "mean"),
        mean_min_percentile=("max_intensity", "mean"),
    ).reset_index()
    return out.sort_values(["method", "pct_type", "threshold"]).reset_index(drop=True)


def per_site_by_threshold(events: pd.DataFrame) -> pd.DataFrame:
    """Per site x pct_type x threshold x method summary (for distribution plots)."""
    g = events.groupby(["method", "site", "pct_type", "threshold"])
    out = g.agg(
        n_events=("severity", "size"),
        total_severity=("severity", "sum"),
        mean_severity=("severity", "mean"),
        total_duration=("duration", "sum"),
        total_deficit_volume_cms=("flow_volume_cms", "sum"),
        mean_min_percentile=("max_intensity", "mean"),
    ).reset_index()
    return out


def equivalence_orig30_vs_universal(orig: pd.DataFrame, univ: pd.DataFrame) -> pd.DataFrame:
    """Compare original threshold==30 events to the full universal event set per site.

    Both should describe the same below-30 events (identical identification + pooling +
    severity definition), so totals should match closely. Differences flag any drift.
    """
    def site_totals(df):
        return df.groupby(["site", "pct_type"]).agg(
            n_events=("severity", "size"),
            total_duration=("duration", "sum"),
            total_severity=("severity", "sum"),
            total_deficit_volume_cms=("flow_volume_cms", "sum"),
        ).reset_index()

    o = site_totals(orig[orig["threshold"] == 30]).add_suffix("_orig30")
    o = o.rename(columns={"site_orig30": "site", "pct_type_orig30": "pct_type"})
    u = site_totals(univ).add_suffix("_univ")
    u = u.rename(columns={"site_univ": "site", "pct_type_univ": "pct_type"})

    m = o.merge(u, on=["site", "pct_type"], how="outer")
    for col in ["n_events", "total_duration", "total_severity", "total_deficit_volume_cms"]:
        m[f"{col}_orig30"] = m[f"{col}_orig30"].fillna(0)
        m[f"{col}_univ"] = m[f"{col}_univ"].fillna(0)
        m[f"{col}_diff"] = m[f"{col}_univ"] - m[f"{col}_orig30"]
    return m.sort_values(["site", "pct_type"]).reset_index(drop=True)


def main():
    ap = argparse.ArgumentParser(description="Compare original vs universal drought events.")
    ap.add_argument(
        "--orig",
        default="/home/ryan/data/CONUS_drought_props/obs_ref_only/drought_props",
        help="Original events: a long CSV or a directory of per-site CSVs.",
    )
    ap.add_argument(
        "--univ",
        default="/home/ryan/data/CONUS_drought_props/obs_ref_only_universal/obs_drought_props_long.csv",
        help="Universal events: a long CSV or a directory of per-site CSVs.",
    )
    ap.add_argument(
        "--out",
        default="/home/ryan/data/CONUS_drought_props/method_comparison",
        help="Directory to write comparison CSVs.",
    )
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)

    print("Loading original events...", flush=True)
    orig = load_events(args.orig)
    print(f"  {len(orig):,} rows", flush=True)
    print("Loading universal events...", flush=True)
    univ = load_events(args.univ)
    print(f"  {len(univ):,} rows", flush=True)

    orig = orig[EVENT_COLS].copy()
    univ = univ[EVENT_COLS].copy()
    orig["method"] = "original"
    univ["method"] = "universal"
    events = pd.concat([orig, univ], ignore_index=True)

    agg = aggregate_by_threshold(events)
    per_site = per_site_by_threshold(events)
    equiv = equivalence_orig30_vs_universal(orig, univ)

    agg.to_csv(os.path.join(args.out, "cmp_aggregate_by_threshold.csv"), index=False)
    per_site.to_csv(os.path.join(args.out, "cmp_per_site_by_threshold.csv"), index=False)
    equiv.to_csv(os.path.join(args.out, "cmp_equivalence_per_site.csv"), index=False)

    # ---- Console headline ----
    pd.set_option("display.width", 160)
    pd.set_option("display.max_columns", 20)
    print("\n=== Mean severity by threshold/class (pooled over pct_type) ===")
    head = (
        events.groupby(["method", "threshold"])
        .agg(n_events=("severity", "size"),
             mean_severity=("severity", "mean"),
             mean_duration=("duration", "mean"),
             mean_min_pct=("max_intensity", "mean"))
        .round(1)
    )
    print(head)

    print("\n=== Equivalence (original thr=30 vs universal all-classes) ===")
    for col in ["n_events", "total_duration", "total_severity"]:
        d = equiv[f"{col}_diff"]
        denom = equiv[f"{col}_orig30"].replace(0, np.nan)
        rel = (d / denom).abs()
        print(f"  {col:16s}: max|diff|={d.abs().max():.4g}  "
              f"mean|diff|={d.abs().mean():.4g}  "
              f"sites exact match={int((d.abs() < 1e-6).sum())}/{len(equiv)}  "
              f"mean|rel|={np.nanmean(rel):.4g}")

    print(f"\nWrote 3 CSVs to {args.out}")


if __name__ == "__main__":
    main()
