"""USGS site ids keep their leading zeros through input reading and site selection."""

import os

import pandas as pd

from hymed.pipeline import read_ref_sites, read_site_input


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


def test_ref_sites_restore_leading_zeros(tmp_path):
    path = tmp_path / "ref.csv"
    pd.DataFrame({
        "site": ["1011000", "01013500", "394220106431500", "2000000"],
        "Ref": [1, 1, 1, 0],
    }).to_csv(path, index=False)

    assert read_ref_sites(str(path)) == {"01011000", "01013500", "394220106431500"}
