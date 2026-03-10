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

Python 3.8 or higher is recommended. Install dependencies with:

```bash
pip install -r requirements.txt
```

**Dependencies:** `pandas`, `numpy`, `scipy`, `scikit-learn`

## Usage

### Running the full workflow

Edit `run_single_site.py` to set the model name (`nhm` or `nwm`), then run:

```bash
python run_single_site.py
```

The script iterates over all sites in `input_data/`, computes all metrics, writes per-site output files, and combines results into three long-format summary files.

### Using the package directly

```python
import pandas as pd
from hymed import (
    calculate_site_percentiles,
    calculate_site_properties,
    calculate_site_boolean_threshold_only,
    site_cohens_kappa,
    site_spearmans,
    site_bias_distribution,
    site_annual_signatures,
)

# Load input data for one site
df = pd.read_csv("inst/extdata/nhm/input_data/01011000.csv", dtype={"site": str})
df["dt"] = pd.to_datetime(df["dt"])
df["q_mod"] = df["q_cms_nhm"]
df["q_obs"] = df["q_cms_obs"]

# Calculate percentiles
df_pct = calculate_site_percentiles(df, site_name="01011000", units="cms")

# Calculate drought event properties and annual statistics
df_properties, df_annual_stats = calculate_site_properties(df_pct, site_name="01011000")

# Calculate evaluation metrics
df_kappa    = site_cohens_kappa(calculate_site_boolean_threshold_only(df_pct), "01011000")
df_spear    = site_spearmans(df_pct, "01011000")
df_bias     = site_bias_distribution(df_pct, "01011000")
df_ann_eval = site_annual_signatures(df_annual_stats, "01011000")
```

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

## Output Files

Per-site outputs are written to subfolders within the model data directory:

| Folder / File | Contents |
|---|---|
| `percentiles/<site>.csv` | Weibull percentile columns for obs and mod |
| `kappa/kappa_<site>.csv` | Cohen's kappa and classification accuracy |
| `spearmans/spearman_<site>.csv` | Spearman's rho and NSE for low-flow subsets |
| `bias_dist/bias_dist_<site>.csv` | Bias, percent bias, and SD ratio |
| `ann_eval/ann_eval_<site>.csv` | Annual drought signature evaluation metrics |

Three aggregated long-format files are also written to the model directory root:

| File | Contents |
|---|---|
| `kappa_long.csv` | Kappa metrics for all sites, thresholds, and percentile types |
| `spear_bias_dist_long.csv` | Spearman and bias metrics in long format |
| `ann_eval_long.csv` | Annual evaluation metrics in long format |

## Package Structure

```
hymed/
├── __init__.py
├── gather_data.py                  # Concatenate per-site CSV files from a folder
├── calculate_percentiles.py        # Weibull percentiles with CDPM zero-flow handling
├── calculate_properties.py         # Drought event identification and annual statistics
├── calculate_booleans.py           # Drought boolean timeseries (pooled and threshold-only)
├── cohens_kappa.py                 # Cohen's kappa and classification accuracy
├── spearmans.py                    # Spearman's rho for low-flow periods
├── bias_dist.py                    # Bias and standard deviation ratio
├── annual_signatures.py            # Annual drought signature evaluation metrics
└── setup_files.py                  # Combine model files and write per-site CSVs
run_single_site.py                  # Main workflow script
requirements.txt
```

## Associated Publications

- Simeone, C.E., Foks, S.S., Towler, E., Hodson, T.O., and Over, T., 2024a, Evaluating Hydrologic Model Performance for Characterizing Streamflow Drought in the Conterminous United States. *Water*, *16*(20), p.2996. https://doi.org/10.3390/w16202996

- Simeone, C.E., Staub, L.E., Kolb, K.R., and Foks, S.S., 2024b, Results of benchmarking National Water Model v2.1 simulations of streamflow drought: U.S. Geological Survey data release, https://doi.org/10.5066/P9P4DHZE

- Simeone, C.E., Staub, L.E., Kolb, K.R., and Foks, S.S., 2024c, Results of benchmarking National Hydrologic Model application of PRMS simulations of streamflow drought: U.S. Geological Survey data release, https://doi.org/10.5066/P9YQSKC0

## License

See [LICENSE.md](LICENSE.md).
