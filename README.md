# HyMED Python

**Hy**drologic **M**odel **E**valuation for **D**rought — Python translation

This is a Python translation of the original HyMED R package (Simeone and Foks, 2024). All core functions have been translated to Python with outputs validated against the original R implementation. Please cite the original R package when using this code.

## Original R Package Citation

Simeone, C.E., and Foks, S.S, 2024, HyMED—Hydrologic Model Evaluation for Drought: R package version 1.0.0, U.S. Geological Survey software release, https://doi.org/10.5066/P1TLARC5

## Overview

HyMED evaluates hydrologic model streamflow output during periods of drought. The package calculates a suite of metrics comparing modeled and observed streamflow drought, including:

- **Cohen's Kappa** — agreement between modeled and observed drought classification
- **Spearman's rho** — rank correlation during low-flow periods
- **Percent bias and SD ratio** — distributional errors during low flows
- **Annual drought signature metrics** — NSE, Pearson/Spearman/Kendall correlation, MAE, NMAE, and more for five annual drought measures

Drought percentiles are computed using the Weibull formula with optional **variable** (Julian-day) or **fixed** (site-level) threshold approaches, and the Consecutive Drought Period Method (CDPM) to handle zero-flow periods. Default percentile thresholds are the 5th, 10th, 20th, and 30th percentiles.

## Installation

Requires Python 3.9 or higher. From a clone of this repository:

```bash
pip install -e .              # the hymed package and the `hymed` command
pip install -e ".[scripts]"   # plus pyarrow, tqdm, matplotlib for the scripts in scripts/
pip install -e ".[dev]"       # plus pytest
```

**Dependencies:** `pandas`, `numpy`, `scipy`, `scikit-learn`

## Usage

### Running the full workflow

```bash
hymed run --model nwm --data-path <dir containing input_data/> --out-path <dir>
hymed run --model nwm --data-path <dir> --out-path <dir> --universal
```

(`python -m hymed run ...` works the same without installing the command.)
`--model` selects the `q_cms_<model>` column of the input files. Every site in `input_data/`
is evaluated in parallel and the combined output files described below are written.

| Option | Effect |
|---|---|
| `--universal` | Identify drought events once at the 30th percentile and label each with a severity class (5/10/20/30) from the lowest percentile it reaches, instead of identifying events separately at each threshold |
| `--workers <n>` | Number of parallel processes (default: `$HYMED_WORKERS`, else 16) |
| `--ref-sites <csv>` | Only evaluate sites with `Ref == 1` in a CSV with `site` and `Ref` columns |
| `--skip-existing` | Resume an interrupted run: sites already checkpointed in `sites/` are not recomputed |
| `--write-percentiles` | Also write the daily percentiles per site to `percentiles/` |

### Using the package directly

```python
import pandas as pd
from hymed import evaluate_site

# Load input data for one site
df = pd.read_csv("inst/extdata/nhm/input_data/01011000.csv", dtype={"site": str})
df["dt"] = pd.to_datetime(df["dt"])
df["q_mod"] = df["q_cms_nhm"]
df["q_obs"] = df["q_cms_obs"]

results = evaluate_site(df, "01011000", scheme="per_threshold")  # or "universal"
results["kappa_long"]       # same layout as the combined output files
results["drought_events"]
results["percentiles"]
```

To run many sites from Python, use `hymed.pipeline.run_evaluation(data_path, out_path, model, ...)`,
which takes the same options as `hymed run`. The individual steps (`calculate_site_percentiles`, `calculate_site_properties`,
`site_cohens_kappa`, `site_spearmans`, `site_bias_distribution`, `site_annual_signatures`)
are also exported; see `hymed/pipeline.py:evaluate_site` for how they fit together.

### Running the tests

`tests/` holds end-to-end regression tests: `hymed run` (both schemes) is run on small synthetic
inputs (`tests/golden/input_data/`) and every output file is compared against the golden
outputs in `tests/golden/`. Requires `pytest`.

```bash
python -m pytest
```

Regenerate the golden files with `python tests/make_golden.py` only when an output change is intended.

## Input Data Format

Input CSVs (one per site) must contain the following columns:

| Column | Description |
|---|---|
| `site` | USGS streamgage identifier (string, preserves leading zeros) |
| `dt` | Date of streamflow measurement (YYYY-MM-DD) |
| `q_cms_obs` | Mean daily observed streamflow (m³/s) |
| `q_cms_<model>` | Mean daily modeled streamflow (m³/s) |
| `jd` | Julian day of year |
| `year` | Calendar year |
| `month` | Month (1–12) |
| `wy` | Water year (Oct 1 – Sep 30) |
| `cy` | Climate year (Apr 1 – Mar 31) |

Each day should appear once per site. `hymed run` drops repeated days with identical flows
(with a warning) and stops with an error if a day repeats with different flows.

## Output Files

All sites are combined into one file per output, in the `--out-path` directory:

| File | Contents |
|---|---|
| `drought_events.csv` | One row per drought event: duration, severity, intensity, flow volume, start/end, `threshold`, `pct_type` |
| `kappa_long.csv` | Cohen's kappa and classification accuracy |
| `spear_bias_dist_long.csv` | Spearman's rho and NSE for low-flow days; bias, percent bias and SD ratio |
| `ann_eval_long.csv` | Annual drought signature metrics (NSE, correlations, errors) per annual `measure` |

The three metric files share one long layout: `site, pct_type, threshold, metric, source, value`,
with an extra `measure` column after `threshold` in `ann_eval_long.csv`. `pct_type` is
`variable` (Julian-day percentiles) or `fixed` (site percentiles). `source` is `mod` for
model-vs-observed metrics and `obs` for observed reference values (the observed annual
`mean` in `ann_eval_long.csv`). In the universal scheme, `threshold` is the event's severity
class rather than the identification threshold.

Also written:

| Folder | Contents |
|---|---|
| `percentiles/<site>.csv` | Daily Weibull percentiles for obs and mod (only with `--write-percentiles`) |
| `sites/<output>/<site>.csv` | Per-site checkpoints of the combined files, used by `--skip-existing` |

## Package Structure

```
hymed/
├── cli.py                          # `hymed run` command line
├── pipeline.py                     # Multi-site workflow (run_evaluation) and evaluate_site
├── config.py                       # Shared constants (thresholds, pooling parameters, ...)
├── _util.py                        # Shared helpers (rleid, NSE, climate/water year columns)
├── calculate_percentiles.py        # Weibull percentiles with CDPM zero-flow handling
├── calculate_properties.py         # Drought event identification (per-threshold and universal) and annual statistics
├── calculate_booleans.py           # Threshold-only drought boolean timeseries (for kappa)
├── cohens_kappa.py                 # Cohen's kappa and classification accuracy
├── spearmans.py                    # Spearman's rho for low-flow periods
├── bias_dist.py                    # Bias and standard deviation ratio
└── annual_signatures.py            # Annual drought signature evaluation metrics
scripts/                            # Study-specific data preparation and method comparison
├── setup_conus_hymed_input.py      # CONUS NWM + USGS parquet -> per-site input CSVs
├── prepare_obs_ref_input.py        # GAGES-II reference sites, observed-only inputs
├── prepare_obs_input.py            # Observed-only inputs from existing model inputs
├── compare_methods.py              # Per-threshold vs universal drought events
└── compare_methods.ipynb
tests/                              # End-to-end regression tests
pyproject.toml
```

## Associated Publications

- Simeone, C.E., Foks, S.S., Towler, E., Hodson, T.O., and Over, T., 2024a, Evaluating Hydrologic Model Performance for Characterizing Streamflow Drought in the Conterminous United States. *Water*, *16*(20), p.2996. https://doi.org/10.3390/w16202996

- Simeone, C.E., Staub, L.E., Kolb, K.R., and Foks, S.S., 2024b, Results of benchmarking National Water Model v2.1 simulations of streamflow drought: U.S. Geological Survey data release, https://doi.org/10.5066/P9P4DHZE

- Simeone, C.E., Staub, L.E., Kolb, K.R., and Foks, S.S., 2024c, Results of benchmarking National Hydrologic Model application of PRMS simulations of streamflow drought: U.S. Geological Survey data release, https://doi.org/10.5066/P9YQSKC0

## License

See [LICENSE.md](LICENSE.md).
