"""
pipeline.py

Full HyMED evaluation workflow: per-site metrics (run in parallel) followed by the
long-format summary files. Shared by run_single_site.py and
run_single_site_universal.py, which differ only in how drought events are labeled
(see hymed/calculate_properties_universal.py).
Translated from inst/extdata/run_single_site.R.
"""

import argparse
import glob
import multiprocessing
import os

import pandas as pd

from ._util import climate_year
from .annual_signatures import site_annual_signatures
from .bias_dist import site_bias_distribution
from .calculate_booleans import calculate_site_boolean_threshold_only
from .calculate_percentiles import calculate_site_percentiles
from .calculate_properties import calculate_site_properties
from .calculate_properties_universal import calculate_site_properties_universal
from .cohens_kappa import site_cohens_kappa
from .config import PCT_TYPE_LABELS
from .gather_data import gather_data
from .spearmans import site_spearmans

SCHEMES = {
    "per_threshold": calculate_site_properties,
    "universal": calculate_site_properties_universal,
}

# Per-site output folders, and the filename prefix used in each.
OUTPUT_DIRS = {
    "percentiles": "",
    "drought_props": "",
    "kappa": "kappa_",
    "spearmans": "spearman_",
    "bias_dist": "bias_dist_",
    "ann_eval": "ann_eval_",
}


def _site_file(out_path: str, folder: str, site_name: str) -> str:
    return os.path.join(out_path, folder, f"{OUTPUT_DIRS[folder]}{site_name}.csv")


def read_site_input(data_path: str, site_name: str, model: str) -> pd.DataFrame:
    """Read input_data/<site>.csv and rename the flow columns to q_obs / q_mod."""
    df = pd.read_csv(
        os.path.join(data_path, "input_data", f"{site_name}.csv"),
        dtype={"site": str},
    )
    df["dt"] = pd.to_datetime(df["dt"])
    df["q_mod"] = df[f"q_cms_{model}"]
    df["q_obs"] = df["q_cms_obs"]
    return df.drop(columns=["q_cms_obs", f"q_cms_{model}"])


def run_site_eval(
    site_name: str,
    data_path: str,
    model: str,
    out_path: str,
    scheme: str = "per_threshold",
) -> None:
    """
    Run full drought evaluation for a single site and write its per-site outputs.

    Parameters
    ----------
    site_name : str
        USGS station ID (matches CSV filename without extension).
    data_path : str
        Path to the model data directory containing input_data/ (may be read-only).
    model : str
        Model name string (e.g. 'nhm' or 'nwm'); selects the q_cms_<model> column.
    out_path : str
        Path to write output CSVs (must be writable).
    scheme : str
        Drought event labeling: 'per_threshold' or 'universal'.
    """
    df = read_site_input(data_path, site_name, model)
    cy = climate_year(df["dt"])

    df_pct = calculate_site_percentiles(df, site_name, units="cms")
    df_pct.to_csv(_site_file(out_path, "percentiles", site_name), index=False)

    df_properties, df_annual_stats = SCHEMES[scheme](
        df_pct, site_name, start_cy=int(cy.min()), end_cy=int(cy.max()),
    )
    df_properties.to_csv(_site_file(out_path, "drought_props", site_name), index=False)

    # Kappa uses threshold-only booleans (no event pooling).
    df_kappa = site_cohens_kappa(calculate_site_boolean_threshold_only(df_pct), site_name)
    df_kappa.to_csv(_site_file(out_path, "kappa", site_name), index=False)

    df_spear = site_spearmans(df_pct, site_name)
    df_spear.to_csv(_site_file(out_path, "spearmans", site_name), index=False)

    df_bias_dist = site_bias_distribution(df_pct, site_name)
    df_bias_dist.to_csv(_site_file(out_path, "bias_dist", site_name), index=False)

    df_ann_eval = site_annual_signatures(df_annual_stats, site_name)
    df_ann_eval.to_csv(_site_file(out_path, "ann_eval", site_name), index=False)


def _run_site_eval_worker(args: tuple) -> None:
    """Multiprocessing worker: unpacks args and calls run_site_eval."""
    site_name, data_path, out_path, model, scheme, i = args
    print(f"site: {site_name} - iteration: {i}", flush=True)
    run_site_eval(site_name, data_path, model, out_path, scheme)


def _split_source_metric(df: pd.DataFrame) -> pd.DataFrame:
    """Split 'mod_kappa'-style column names in `temp` into source='mod', metric='kappa'."""
    df["source"] = df["temp"].str[:3]
    df["metric"] = df["temp"].str[4:]
    return df.drop(columns=["temp"])


def write_long_outputs(out_path: str) -> None:
    """Gather the per-site outputs into kappa_long, spear_bias_dist_long and ann_eval_long."""
    # ---- Kappa ----
    df_kappa = gather_data("kappa", out_path)
    df_kappa_long = _split_source_metric(df_kappa.melt(
        id_vars=["pct_type", "threshold", "site"],
        value_vars=["mod_kappa", "mod_classification_accuracy"],
        var_name="temp",
        value_name="value",
    ))
    df_kappa_long = df_kappa_long[["site", "threshold", "pct_type", "metric", "source", "value"]]
    df_kappa_long["pct_type"] = df_kappa_long["pct_type"].replace(PCT_TYPE_LABELS)
    df_kappa_long.to_csv(os.path.join(out_path, "kappa_long.csv"), index=False)

    # ---- Spearman's rho + bias/distribution ----
    df_spearmans = gather_data("spearmans", out_path)
    df_spearmans = df_spearmans.drop(columns=["quant_length"], errors="ignore")
    df_spearmans = df_spearmans.rename(columns={"rho_obs_mod": "mod_rho_obs"})
    df_spear_long = _split_source_metric(df_spearmans.melt(
        id_vars=["threshold", "site", "pct_type"],
        value_vars=["mod_rho_obs", "mod_nse"],
        var_name="temp",
        value_name="value",
    ))

    df_bias = gather_data("bias_dist", out_path)
    df_bias_long = _split_source_metric(df_bias.melt(
        id_vars=["thresh", "pct_type", "site"],
        value_vars=["mod_bias", "mod_pct_bias", "mod_sd_ratio"],
        var_name="temp",
        value_name="value",
    ))
    df_bias_long = df_bias_long.rename(columns={"thresh": "threshold"})

    df_spear_bias_dist = pd.concat([df_spear_long, df_bias_long], ignore_index=True)
    df_spear_bias_dist = df_spear_bias_dist[
        ["site", "threshold", "source", "metric", "pct_type", "value"]
    ].sort_values(["site", "threshold"]).reset_index(drop=True)
    df_spear_bias_dist["pct_type"] = df_spear_bias_dist["pct_type"].replace(
        {f"weibull_{k}": v for k, v in PCT_TYPE_LABELS.items()}
    )
    df_spear_bias_dist.to_csv(os.path.join(out_path, "spear_bias_dist_long.csv"), index=False)

    # ---- Annual signature evaluation ----
    df_ann = gather_data("ann_eval", out_path)
    value_cols = [c for c in df_ann.columns if c.endswith("_mod")]
    id_cols = [c for c in df_ann.columns if c not in value_cols]
    df_ann_long = df_ann.melt(
        id_vars=id_cols,
        value_vars=value_cols,
        var_name="temp",
        value_name="value",
    )
    # Split 'NSE_mod' -> metric='NSE', source='mod'
    split = df_ann_long["temp"].str.rsplit("_", n=1, expand=True)
    df_ann_long["metric"] = split[0]
    df_ann_long["source"] = split[1]
    df_ann_long = df_ann_long.drop(columns=["temp"])
    df_ann_long["type"] = df_ann_long["type"].replace(PCT_TYPE_LABELS)
    df_ann_long.to_csv(os.path.join(out_path, "ann_eval_long.csv"), index=False)


def list_sites(data_path: str) -> list:
    """Site ids (CSV basenames) in data_path/input_data."""
    input_dir = os.path.join(data_path, "input_data")
    input_files = sorted(glob.glob(os.path.join(input_dir, "*.csv")))
    if not input_files:
        raise FileNotFoundError(
            f"No CSV files found in {input_dir!r}. "
            "Check that --data-path is accessible from this machine."
        )
    return [os.path.splitext(os.path.basename(f))[0] for f in input_files]


def main(scheme: str = "per_threshold", argv: list = None) -> None:
    """Command-line entry point for the full workflow."""
    universal = scheme == "universal"
    out_suffix = "_universal" if universal else ""
    parser = argparse.ArgumentParser(
        description="Run HyMED drought evaluation"
        + (" (universal classification scheme)." if universal else ".")
    )
    parser.add_argument("--model", default="nwm", help="Model name (e.g. nwm)")
    parser.add_argument(
        "--data-path",
        default=None,
        help="Path to model data directory containing input_data/ (may be read-only). "
             "Defaults to inst/extdata/<model>.",
    )
    parser.add_argument(
        "--ref-sites",
        default=None,
        help="Path to a CSV with columns 'site' and 'Ref'. "
             "If provided, only sites where Ref==1 are evaluated.",
    )
    parser.add_argument(
        "--out-path",
        default=None,
        help=f"Path to write output CSVs. Defaults to inst/extdata/<model>{out_suffix}.",
    )
    parser.add_argument(
        "--skip-existing",
        action="store_true",
        help="Skip sites that already have an ann_eval output file.",
    )
    args = parser.parse_args(argv)

    model_name = args.model
    data_path = args.data_path or os.path.join("inst", "extdata", model_name)
    # Allow passing the input_data subfolder directly; normalize to parent
    if os.path.basename(data_path.rstrip("/\\")) == "input_data":
        data_path = os.path.dirname(data_path.rstrip("/\\"))
    # Universal results default to a separate directory so they don't overwrite
    # the per-threshold outputs.
    out_path = args.out_path or os.path.join("inst", "extdata", model_name + out_suffix)

    for folder in OUTPUT_DIRS:
        os.makedirs(os.path.join(out_path, folder), exist_ok=True)

    site_list = list_sites(data_path)

    if args.skip_existing:
        before = len(site_list)
        site_list = [
            s for s in site_list
            if not os.path.exists(_site_file(out_path, "ann_eval", s))
        ]
        print(f"Skipping {before - len(site_list)} existing sites, {len(site_list)} remaining.", flush=True)

    if args.ref_sites:
        ref_df = pd.read_csv(args.ref_sites, dtype={"site": str})
        ref_sites = set(ref_df.loc[ref_df["Ref"] == 1, "site"].tolist())
        site_list = [s for s in site_list if s in ref_sites]
        print(f"Filtered to {len(site_list)} reference sites.", flush=True)

    n_workers = int(os.environ.get("HYMED_WORKERS", "16"))
    worker_args = [
        (s, data_path, out_path, model_name, scheme, i)
        for i, s in enumerate(site_list, start=1)
    ]
    with multiprocessing.Pool(processes=n_workers) as pool:
        pool.map(_run_site_eval_worker, worker_args)

    write_long_outputs(out_path)
