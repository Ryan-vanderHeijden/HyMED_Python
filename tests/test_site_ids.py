"""USGS site ids keep their leading zeros through input reading and site selection."""

import os

import pandas as pd
import pytest

from hymed.pipeline import read_site_input, select_ref_sites


def test_site_id_comes_from_filename(tmp_path):
    """A site column written as a number (zeros lost) is replaced by the file's id."""
    os.makedirs(tmp_path / "input_data")
    pd.DataFrame({
        "site": [1011000, 1011000],
        "dt": ["2000-01-01", "2000-01-02"],
        "q_cms_obs": [1.0, 2.0],
        "q_cms_nwm": [1.5, 2.5],
    }).to_csv(tmp_path / "input_data" / "01011000.csv", index=False)

    df = read_site_input(str(tmp_path), "01011000", "nwm")
    assert df["site"].tolist() == ["01011000", "01011000"]


def test_ref_sites_match_without_leading_zeros(tmp_path):
    """Ref lists that stored ids as numbers still select the zero-led sites, any length."""
    path = tmp_path / "ref.csv"
    pd.DataFrame({
        "site": ["1011000", "01013500", "136230002", "394220106431500", "2000000"],
        "Ref": [1, 1, 1, 1, 0],
    }).to_csv(path, index=False)
    sites = ["01011000", "01013500", "0136230002", "394220106431500", "02000000", "03000000"]

    assert select_ref_sites(sites, str(path)) == [
        "01011000", "01013500", "0136230002", "394220106431500",
    ]


def _write(tmp_path, dts, q_obs):
    os.makedirs(tmp_path / "input_data", exist_ok=True)
    pd.DataFrame({"site": "01011000", "dt": dts, "q_cms_obs": q_obs,
                  "q_cms_nwm": [1.5] * len(dts)}).to_csv(
        tmp_path / "input_data" / "01011000.csv", index=False)


def test_exact_duplicate_days_are_dropped(tmp_path):
    _write(tmp_path, ["2000-01-01", "2000-01-01", "2000-01-02"], [1.0, 1.0, 2.0])
    with pytest.warns(UserWarning, match="dropped 1 duplicated"):
        df = read_site_input(str(tmp_path), "01011000", "nwm")
    assert df["dt"].dt.strftime("%Y-%m-%d").tolist() == ["2000-01-01", "2000-01-02"]


def test_conflicting_duplicate_days_raise(tmp_path):
    _write(tmp_path, ["2000-01-01", "2000-01-01", "2000-01-02"], [1.0, 3.0, 2.0])
    with pytest.raises(ValueError, match="conflicting flows.*2000-01-01"):
        read_site_input(str(tmp_path), "01011000", "nwm")
