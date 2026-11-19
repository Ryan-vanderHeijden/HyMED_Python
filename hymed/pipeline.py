"""
pipeline.py

Full HyMED evaluation workflow over many sites (run_evaluation; `hymed run` on the
command line) and for a single site in memory (evaluate_site). The two schemes differ
only in how drought events are labeled (see hymed/calculate_properties.py).
Translated from the R package's inst/extdata/run_single_site.R.

Outputs (in out_path):

  drought_events.csv          drought events, all sites
  kappa_long.csv              Cohen's kappa / classification accuracy, all sites
  spear_bias_dist_long.csv    Spearman's rho, NSE, bias, SD ratio, all sites
  ann_eval_long.csv           annual drought signature metrics, all sites
  percentiles/<site>.csv      daily percentiles (only with --write-percentiles)
  sites/<output>/<site>.csv   per-site checkpoints of the four combined files, used to
                              resume with --skip-existing; ann_eval is written last
"""

import glob
import multiprocessing
import os
import warnings

import pandas as pd

from ._util import climate_year
from .annual_signatures import site_annual_signatures
from .bias_dist import site_bias_distribution
from .calculate_booleans import calculate_site_boolean_threshold_only
from .calculate_percentiles import calculate_site_percentiles
from .calculate_properties import calculate_site_properties, calculate_site_properties_universal
from .cohens_kappa import site_cohens_kappa
from .spearmans import site_spearmans

SCHEMES = {
    "per_threshold": calculate_site_properties,
    "universal": calculate_site_properties_universal,
}

# Combined output file -> rows are stably sorted by these columns after concatenation.
# "drought_events" keeps the per-site order (site, then pct_type and threshold blocks).
OUTPUTS = {
    "drought_events": ["site"],
    "kappa_long": ["site", "pct_type", "threshold"],
    "spear_bias_dist_long": ["site", "pct_type", "threshold"],
    "ann_eval_long": ["site", "pct_type", "threshold", "measure"],
}
# Written last for each site, so its presence means the site's checkpoint is complete.
DONE_MARKER = "ann_eval_long"


def _checkpoint(out_path: str, output: str, site_name: str) -> str:
    return os.path.join(out_path, "sites", output, f"{site_name}.csv")


def _write_atomic(df: pd.DataFrame, path: str) -> None:
    """Write via a temp file so an interrupted run never leaves a partial checkpoint."""
    tmp = f"{path}.tmp"
    df.to_csv(tmp, index=False)
    os.replace(tmp, path)


def read_site_input(data_path: str, site_name: str, model: str) -> pd.DataFrame:
    """Read input_data/<site>.csv and rename the flow columns to q_obs / q_mod.

    The site id is taken from the file name, so it keeps its leading zeros (the USGS
    convention) even if the file's own site column was written as a number.
    """
    df = pd.read_csv(
        os.path.join(data_path, "input_data", f"{site_name}.csv"),
        dtype={"site": str},
    )
    df["site"] = site_name
    df["dt"] = pd.to_datetime(df["dt"])
    df["q_mod"] = df[f"q_cms_{model}"]
    df["q_obs"] = df["q_cms_obs"]
    df = df.drop(columns=["q_cms_obs", f"q_cms_{model}"])
    return drop_duplicate_dates(df, site_name)


def drop_duplicate_dates(df: pd.DataFrame, site_name: str) -> pd.DataFrame:
    """Drop repeated days with identical flows; raise if a day has conflicting flows.

    Each day must appear once, or it is counted twice in the percentiles and drought
    durations. Exact repeats (e.g. from overlapping downloads) are dropped with a warning.
    """
    dup = df.duplicated(["dt", "q_obs", "q_mod"])
    if dup.any():
        warnings.warn(f"{site_name}: dropped {int(dup.sum())} duplicated day(s)", stacklevel=2)
        df = df[~dup].reset_index(drop=True)
    conflict = df["dt"].duplicated(keep=False)
    if conflict.any():
        days = sorted(df.loc[conflict, "dt"].dt.strftime("%Y-%m-%d").unique())
        raise ValueError(f"{site_name}: conflicting flows on duplicated day(s) {days[:5]}"
                         + (f" and {len(days) - 5} more" if len(days) > 5 else ""))
    return df


def evaluate_site(df: pd.DataFrame, site_name: str, scheme: str = "per_threshold") -> dict:
    """
    Run the full drought evaluation for one site's paired series.

    Parameters
    ----------
    df : pd.DataFrame
        Columns site, dt, q_obs, q_mod (see read_site_input).
    site_name : str
        Site identifier.
    scheme : str
        Drought event labeling: 'per_threshold' or 'universal'.

    Returns
    -------
    dict
        'percentiles' (daily) plus one DataFrame per key of OUTPUTS.
    """
    cy = climate_year(df["dt"])
    df_pct = calculate_site_percentiles(df, site_name, units="cms")
    df_events, df_annual_stats = SCHEMES[scheme](
        df_pct, site_name, start_cy=int(cy.min()), end_cy=int(cy.max()),
    )
    return {
        "percentiles": df_pct,
        "drought_events": df_events,
        # Kappa uses threshold-only booleans (no event pooling).
        "kappa_long": site_cohens_kappa(calculate_site_boolean_threshold_only(df_pct), site_name),
        "spear_bias_dist_long": pd.concat(
            [site_spearmans(df_pct, site_name), site_bias_distribution(df_pct, site_name)],
            ignore_index=True,
        ),
        "ann_eval_long": site_annual_signatures(df_annual_stats, site_name),
    }


def run_site_eval(
    site_name: str,
    data_path: str,
    model: str,
    out_path: str,
    scheme: str = "per_threshold",
    write_percentiles: bool = False,
) -> None:
    """Evaluate one site from data_path/input_data and write its checkpoints to out_path."""
    results = evaluate_site(read_site_input(data_path, site_name, model), site_name, scheme)
    if write_percentiles:
        results["percentiles"].to_csv(
            os.path.join(out_path, "percentiles", f"{site_name}.csv"), index=False
        )
    for output in [o for o in OUTPUTS if o != DONE_MARKER] + [DONE_MARKER]:
        _write_atomic(results[output], _checkpoint(out_path, output, site_name))


def _run_site_eval_worker(args: tuple) -> None:
    """Multiprocessing worker: unpacks args and calls run_site_eval."""
    i, n, *run_args = args
    print(f"site: {run_args[0]} ({i}/{n})", flush=True)
    run_site_eval(*run_args)


def combine_outputs(out_path: str, sites: list) -> None:
    """Concatenate the sites' checkpoints into the combined output files."""
    for output, sort_cols in OUTPUTS.items():
        frames = []
        for site in sorted(sites):
            path = _checkpoint(out_path, output, site)
            try:
                frames.append(pd.read_csv(path, dtype={"site": str}))
            except pd.errors.EmptyDataError:
                pass  # e.g. a site without drought events
        df = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
        if not df.empty:
            df = df.sort_values(sort_cols, kind="stable").reset_index(drop=True)
        df.to_csv(os.path.join(out_path, f"{output}.csv"), index=False)


def _site_key(site: str) -> str:
    """Site id with leading zeros removed, for matching ids that were stored as numbers."""
    return site.strip().lstrip("0")


def select_ref_sites(sites: list, path: str) -> list:
    """The sites listed with Ref == 1 in a CSV with columns 'site' and 'Ref'.

    Lists like GAGES-II often store ids as numbers, which drops leading zeros (and
    padding to 8 digits does not restore 10- or 15-digit ids), so ids are matched with
    leading zeros ignored. The returned ids are the ones in `sites`.
    """
    ref_df = pd.read_csv(path, dtype={"site": str})
    ref_keys = {_site_key(s) for s in ref_df.loc[ref_df["Ref"] == 1, "site"]}
    return [s for s in sites if _site_key(s) in ref_keys]


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


def run_evaluation(
    data_path: str,
    out_path: str,
    model: str,
    scheme: str = "per_threshold",
    ref_sites: str = None,
    skip_existing: bool = False,
    write_percentiles: bool = False,
    workers: int = None,
) -> None:
    """
    Evaluate every site in data_path/input_data in parallel and write the combined outputs.

    Parameters
    ----------
    data_path : str
        Directory containing input_data/<site>.csv (may be read-only). Passing the
        input_data directory itself also works.
    out_path : str
        Directory for the outputs (see module docstring).
    model : str
        Selects the q_cms_<model> column of the inputs.
    scheme : str
        'per_threshold' or 'universal'.
    ref_sites : str, optional
        CSV with columns 'site' and 'Ref'; only sites with Ref == 1 are evaluated.
        Ids are matched ignoring leading zeros (lists often store them as numbers).
    skip_existing : bool
        Resume: skip sites whose checkpoint is complete.
    write_percentiles : bool
        Also write the daily percentiles per site.
    workers : int, optional
        Number of processes. Default: $HYMED_WORKERS, else 16.
    """
    if scheme not in SCHEMES:
        raise ValueError(f"scheme must be one of {list(SCHEMES)}, got {scheme!r}")
    if os.path.basename(data_path.rstrip("/\\")) == "input_data":
        data_path = os.path.dirname(data_path.rstrip("/\\"))

    for output in OUTPUTS:
        os.makedirs(os.path.join(out_path, "sites", output), exist_ok=True)
    if write_percentiles:
        os.makedirs(os.path.join(out_path, "percentiles"), exist_ok=True)

    sites = list_sites(data_path)

    if ref_sites:
        sites = select_ref_sites(sites, ref_sites)
        print(f"Filtered to {len(sites)} reference sites.", flush=True)

    to_run = sites
    if skip_existing:
        to_run = [
            s for s in sites if not os.path.exists(_checkpoint(out_path, DONE_MARKER, s))
        ]
        print(f"Skipping {len(sites) - len(to_run)} existing sites, {len(to_run)} remaining.", flush=True)

    if workers is None:
        workers = int(os.environ.get("HYMED_WORKERS", "16"))
    worker_args = [
        (i, len(to_run), s, data_path, model, out_path, scheme, write_percentiles)
        for i, s in enumerate(to_run, start=1)
    ]
    with multiprocessing.Pool(processes=workers) as pool:
        pool.map(_run_site_eval_worker, worker_args)

    # Combine every selected site, including ones completed by an earlier run.
    combine_outputs(out_path, sites)
