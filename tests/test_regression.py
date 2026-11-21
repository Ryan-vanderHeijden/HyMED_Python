"""
End-to-end regression tests: both workflows must reproduce tests/golden/ exactly.

Every output CSV is compared as a DataFrame: same columns in the same order, same
rows in the same order, same values (floats to rtol=1e-9, to tolerate summation-order
noise from refactoring but nothing a reader of the outputs could notice).

If a test fails, the refactor changed an output. Fix the code; only regenerate the
golden files (tests/make_golden.py) when the change is deliberate.
"""

import os
import shutil

import pandas as pd
import pytest

from workflow import (
    GOLDEN_ANNUAL, SCHEMES, compute_annual_stats, output_files, run_workflow,
)

GOLDEN = os.path.join(os.path.dirname(os.path.abspath(__file__)), "golden")


def _golden_files(scheme):
    root = os.path.join(GOLDEN, scheme)
    return sorted(
        os.path.relpath(os.path.join(d, f), root)[: -len(".gz")]
        for d, _, files in os.walk(root) for f in files if f.endswith(".csv.gz")
    )


@pytest.fixture(scope="module")
def outputs(tmp_path_factory):
    """Run each workflow once per test session; map scheme -> output dir."""
    root = tmp_path_factory.mktemp("hymed")
    return {scheme: run_workflow(scheme, str(root)) for scheme in SCHEMES}


@pytest.mark.parametrize("scheme", SCHEMES)
def test_same_file_set(outputs, scheme):
    assert output_files(outputs[scheme]) == _golden_files(scheme)


@pytest.mark.parametrize(
    "scheme,rel", [(s, f) for s in SCHEMES for f in _golden_files(s)]
)
def test_file_matches_golden(outputs, scheme, rel):
    path = os.path.join(outputs[scheme], rel)
    assert os.path.exists(path), f"missing output {rel}"
    read = dict(dtype={"site": str}, keep_default_na=True)
    expected = pd.read_csv(os.path.join(GOLDEN, scheme, rel + ".gz"), **read)
    actual = pd.read_csv(path, **read)
    pd.testing.assert_frame_equal(actual, expected, check_exact=False, rtol=1e-9, atol=1e-12)


@pytest.fixture(scope="module")
def annual_stats(tmp_path_factory):
    root = tmp_path_factory.mktemp("annual")
    return {scheme: compute_annual_stats(scheme, str(root)) for scheme in SCHEMES}


@pytest.mark.parametrize(
    "scheme,site",
    [(s, f[: -len(".csv.gz")])
     for s in SCHEMES for f in sorted(os.listdir(os.path.join(GOLDEN_ANNUAL, s)))],
)
def test_annual_stats_match_golden(annual_stats, tmp_path, scheme, site):
    """annual_stats rows (sorted) match golden: guards the zero-fill rules directly."""
    path = tmp_path / "actual.csv"
    annual_stats[scheme][site].to_csv(path, index=False)
    read = dict(dtype={"site": str})
    expected = pd.read_csv(os.path.join(GOLDEN_ANNUAL, scheme, f"{site}.csv.gz"), **read)
    actual = pd.read_csv(path, **read)
    pd.testing.assert_frame_equal(actual, expected, check_exact=False, rtol=1e-9, atol=1e-12)


COMBINED = ["drought_events.csv", "kappa_long.csv", "spear_bias_dist_long.csv", "ann_eval_long.csv"]


@pytest.mark.parametrize("scheme", ["per_threshold"])
def test_resume_with_skip_existing(tmp_path, scheme):
    """An interrupted run resumes from checkpoints, and stale checkpoints are ignored."""
    work = run_workflow(scheme, str(tmp_path))
    sites_dir = os.path.join(work, "sites")
    for output in os.listdir(sites_dir):
        d = os.path.join(sites_dir, output)
        # Simulate a site that never finished, and a leftover site from another run.
        os.remove(os.path.join(d, "01000002.csv"))
        shutil.copy(os.path.join(d, "01000001.csv"), os.path.join(d, "09999999.csv"))
    for f in COMBINED:
        os.remove(os.path.join(work, f))

    run_workflow(scheme, str(tmp_path), "--skip-existing", stage=False)

    with open(tmp_path / f"{scheme}.stdout") as f:
        stdout = f.read()
    assert "Skipping 3 existing sites, 1 remaining." in stdout
    assert "site: 01000002" in stdout
    read = dict(dtype={"site": str})
    for f in COMBINED:
        actual = pd.read_csv(os.path.join(work, f), **read)
        assert "09999999" not in set(actual["site"]), f
        expected = pd.read_csv(os.path.join(GOLDEN, scheme, f + ".gz"), **read)
        pd.testing.assert_frame_equal(actual, expected, check_exact=False, rtol=1e-9, atol=1e-12)
