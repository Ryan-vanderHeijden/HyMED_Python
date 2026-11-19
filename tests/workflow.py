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

SCHEMES = {
    "per_threshold": "run_single_site.py",
    "universal": "run_single_site_universal.py",
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


def run_workflow(scheme: str, root: str) -> str:
    """Stage inputs under root/scheme, run `scheme` there, and return the output dir.

    data_path and out_path are the same directory, so outputs land in one tree
    regardless of which of the two each runner writes drought_props to.
    """
    work = os.path.join(root, scheme)
    stage_inputs(os.path.join(work, "input_data"))
    env = dict(os.environ, HYMED_WORKERS="2")
    subprocess.run(
        [sys.executable, SCHEMES[scheme],
         "--model", MODEL, "--data-path", work, "--out-path", work],
        cwd=REPO, env=env, check=True, capture_output=True, text=True,
    )
    return work


def output_files(work: str) -> list:
    """Relative paths of every output CSV (inputs excluded), sorted."""
    out = []
    for dirpath, _, files in os.walk(work):
        rel_dir = os.path.relpath(dirpath, work)
        if rel_dir.split(os.sep)[0] == "input_data":
            continue
        out += [os.path.normpath(os.path.join(rel_dir, f)) for f in files if f.endswith(".csv")]
    return sorted(out)
