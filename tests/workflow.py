"""
Run the full HyMED workflow on the frozen synthetic inputs, end to end.

This is the single place the regression tests touch the workflow entry points,
so it is the only thing that needs updating when the CLI changes during the
refactor. The contract under test is the set of files written and their contents.
"""

import glob
import gzip
import os
import shutil
import subprocess
import sys

from synthetic import MODEL

TESTS = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(TESTS)
GOLDEN = os.path.join(TESTS, "golden")
# Inputs are frozen alongside the golden outputs (numpy's Generator streams are not
# guaranteed stable across versions, so regenerating them could drift).
GOLDEN_INPUTS = os.path.join(GOLDEN, "input_data")
GOLDEN_ANNUAL = os.path.join(GOLDEN, "annual_stats")

# Scheme -> extra `hymed run` arguments.
SCHEMES = {
    "per_threshold": [],
    "universal": ["--universal"],
}


def stage_inputs(input_dir: str) -> None:
    """Decompress the frozen synthetic inputs into input_dir."""
    os.makedirs(input_dir, exist_ok=True)
    files = sorted(glob.glob(os.path.join(GOLDEN_INPUTS, "*.csv.gz")))
    assert files, f"no frozen inputs in {GOLDEN_INPUTS}; run tests/make_golden.py"
    for f in files:
        name = os.path.basename(f)[: -len(".gz")]
        with gzip.open(f, "rb") as src, open(os.path.join(input_dir, name), "wb") as dst:
            shutil.copyfileobj(src, dst)


def run_workflow(scheme: str, root: str, *extra_args: str, stage: bool = True) -> str:
    """Stage inputs under root/scheme, run `scheme` there, and return the output dir.

    Runs with --write-percentiles so the daily percentiles stay under test.
    The process's stdout is stored on the returned path's `.stdout` file for inspection.
    """
    work = os.path.join(root, scheme)
    if stage:
        stage_inputs(os.path.join(work, "input_data"))
    env = dict(os.environ, HYMED_WORKERS="2")
    proc = subprocess.run(
        [sys.executable, "-m", "hymed", "run", *SCHEMES[scheme], "--model", MODEL,
         "--data-path", work, "--out-path", work, "--write-percentiles", *extra_args],
        cwd=REPO, env=env, check=True, capture_output=True, text=True,
    )
    with open(os.path.join(root, f"{scheme}.stdout"), "w") as f:
        f.write(proc.stdout)
    return work


ANNUAL_STATS_KEY = ["site", "pct_type", "threshold", "measure", "cy"]


def compute_annual_stats(scheme: str, root: str) -> dict:
    """Return {site: annual_stats} from the properties step, which no file output exposes.

    Rows are sorted by ANNUAL_STATS_KEY: the row order of annual_stats is not part of
    the contract (it is only consumed via pivot/groupby in site_annual_signatures).
    """
    if REPO not in sys.path:
        sys.path.insert(0, REPO)
    from hymed import calculate_site_percentiles
    from hymed._util import climate_year
    from hymed.pipeline import SCHEMES, list_sites, read_site_input

    work = os.path.join(root, f"annual_stats_{scheme}")
    stage_inputs(os.path.join(work, "input_data"))
    out = {}
    for site in list_sites(work):
        df = read_site_input(work, site, MODEL)
        cy = climate_year(df["dt"])
        df_pct = calculate_site_percentiles(df, site, units="cms")
        _, annual = SCHEMES[scheme](df_pct, site, start_cy=int(cy.min()), end_cy=int(cy.max()))
        out[site] = annual.sort_values(ANNUAL_STATS_KEY).reset_index(drop=True)
    return out


# Not part of the output contract: the inputs, and the per-site resume checkpoints.
INTERNAL_DIRS = {"input_data", "sites"}


def output_files(work: str) -> list:
    """Relative paths of every output CSV (inputs and checkpoints excluded), sorted."""
    out = []
    for dirpath, _, files in os.walk(work):
        rel_dir = os.path.relpath(dirpath, work)
        if rel_dir.split(os.sep)[0] in INTERNAL_DIRS:
            continue
        out += [os.path.normpath(os.path.join(rel_dir, f)) for f in files if f.endswith(".csv")]
    return sorted(out)
