"""
cohens_kappa.py

Compare model drought classification to observed and calculate Cohen's kappa.
Translated from R/site_cohens_kappas.R.
"""

import pandas as pd
from sklearn.metrics import cohen_kappa_score

from ._util import metric_rows
from .config import METRIC_COLS, PCT_TYPE_LABELS, THRESHOLDS


def site_cohens_kappa(df_bool: pd.DataFrame, site_name: str) -> pd.DataFrame:
    """
    Calculate Cohen's kappa and classification accuracy for modeled vs observed drought.

    Parameters
    ----------
    df_bool : pd.DataFrame
        Boolean drought timeseries from calculate_site_boolean_threshold_only().
        Columns: site, day, pct_type, threshold, drought.
    site_name : str
        Site identifier.

    Returns
    -------
    pd.DataFrame
        Long format (config.METRIC_COLS): site, pct_type ('variable'/'fixed'), threshold,
        metric ('kappa', 'classification_accuracy'), source ('mod'), value.
    """
    records = []

    for percent_type in ["jd", "site"]:
        df_type = df_bool[df_bool["pct_type"].str.contains(percent_type)].copy()

        for thresh in THRESHOLDS:
            try:
                df_thresh = df_type[df_type["threshold"] == thresh].copy()

                # Pivot: one column per pct_type
                df_wide = df_thresh.pivot_table(
                    index=["day"],
                    columns="pct_type",
                    values="drought",
                    aggfunc="first",
                ).reset_index()
                df_wide.columns.name = None

                obs_col = f"weibull_{percent_type}_obs"
                mod_col = f"weibull_{percent_type}_mod"

                if obs_col not in df_wide.columns or mod_col not in df_wide.columns:
                    raise ValueError(f"Missing columns: {obs_col} or {mod_col}")

                drought_obs = df_wide[obs_col].astype(bool)
                drought_mod = df_wide[mod_col].astype(bool)

                # Classification accuracy
                mod_ca = (drought_mod == drought_obs).mean()

                # Cohen's kappa
                kappa = cohen_kappa_score(drought_obs, drought_mod)

            except Exception:
                mod_ca = float("nan")
                kappa = float("nan")

            key = {"site": site_name, "pct_type": PCT_TYPE_LABELS[percent_type], "threshold": thresh}
            records += metric_rows(key, {"kappa": kappa, "classification_accuracy": mod_ca})

    return pd.DataFrame(records, columns=METRIC_COLS)
