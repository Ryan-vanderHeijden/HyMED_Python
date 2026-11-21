"""
Deterministic synthetic streamflow inputs for regression tests.

Each site is a paired obs/model daily series in the HyMED input format
(site, dt, q_cms_obs, q_cms_<model>, jd, year, month, wy, cy). The sites are
chosen to exercise distinct code paths rather than to look realistic:

  01000001  perennial    25 years of seasonal flow with persistent (AR1) anomalies
  01000002  intermittent long summer zero-flow runs -> CDPM adjustments
  01000003  gappy        perennial with missing observations (NaN q_cms_obs)
  01000004  short        three climate years only
"""

import os

import numpy as np
import pandas as pd

MODEL = "nwm"
SEED = 20261119


def _ar1(rng, n, phi=0.97, sigma=0.15):
    eps = rng.normal(0.0, sigma, n)
    x = np.empty(n)
    x[0] = eps[0]
    for i in range(1, n):
        x[i] = phi * x[i - 1] + eps[i]
    return x


def _seasonal(dt, base, amp):
    doy = dt.dt.dayofyear.to_numpy()
    # Peak in spring (~day 100), trough in late summer.
    return base * np.exp(amp * np.cos(2 * np.pi * (doy - 100) / 365.25))


def _frame(site, dt, q_obs, q_mod):
    df = pd.DataFrame({"site": site, "dt": dt.dt.strftime("%Y-%m-%d")})
    df["q_cms_obs"] = q_obs
    df[f"q_cms_{MODEL}"] = q_mod
    df["jd"] = dt.dt.dayofyear
    df["year"] = dt.dt.year
    df["month"] = dt.dt.month
    df["wy"] = np.where(df["month"] >= 10, df["year"] + 1, df["year"])
    df["cy"] = np.where(df["month"] >= 4, df["year"] + 1, df["year"])
    return df


def _dates(start, end):
    return pd.Series(pd.date_range(start, end, freq="D"))


def perennial(rng, years=25):
    # >= 19 years are needed for any day to reach the 5th jd ("variable") percentile,
    # since the smallest Weibull percentile per Julian day is 100 / (n_years + 1).
    dt = _dates("2000-04-01", f"{2000 + years}-03-31")
    n = len(dt)
    clim = _seasonal(dt, 5.0, 0.8)
    q_obs = clim * np.exp(_ar1(rng, n))
    # Model: biased, partially correlated anomalies.
    q_mod = 1.15 * clim * np.exp(0.7 * np.log(q_obs / clim) + _ar1(rng, n, sigma=0.08))
    return _frame("01000001", dt, q_obs.round(6), q_mod.round(6))


def intermittent(rng):
    dt = _dates("2000-04-01", "2010-03-31")
    n = len(dt)
    clim = _seasonal(dt, 0.4, 1.5)
    q_obs = clim * np.exp(_ar1(rng, n, sigma=0.25)) - 0.08
    q_mod = clim * np.exp(_ar1(rng, n, sigma=0.25)) - 0.06
    # Clip to exact zeros: produces multi-week zero runs in late summer.
    q_obs = np.clip(q_obs, 0.0, None)
    q_mod = np.clip(q_mod, 0.0, None)
    return _frame("01000002", dt, q_obs.round(6), q_mod.round(6))


def gappy(rng):
    df = perennial(rng, years=10)
    df["site"] = "01000003"
    q = df["q_cms_obs"].to_numpy().copy()
    # A 60-day block, a 10-day block, and scattered single missing days.
    q[400:460] = np.nan
    q[2000:2010] = np.nan
    q[rng.choice(len(q), 40, replace=False)] = np.nan
    df["q_cms_obs"] = q
    return df


def short(rng):
    dt = _dates("2005-04-01", "2008-03-31")
    n = len(dt)
    clim = _seasonal(dt, 2.0, 0.6)
    q_obs = clim * np.exp(_ar1(rng, n))
    q_mod = 0.9 * clim * np.exp(_ar1(rng, n))
    return _frame("01000004", dt, q_obs.round(6), q_mod.round(6))


SITES = [perennial, intermittent, gappy, short]


def write_inputs(input_dir: str) -> list:
    """Write one CSV per synthetic site into input_dir; return the site ids."""
    os.makedirs(input_dir, exist_ok=True)
    rng = np.random.default_rng(SEED)
    sites = []
    for make in SITES:
        df = make(rng)
        site = df["site"].iloc[0]
        df.to_csv(os.path.join(input_dir, f"{site}.csv"), index=False)
        sites.append(site)
    return sites
