"""USGS site ids keep their leading zeros through input reading and site selection."""

import os

import pandas as pd

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
