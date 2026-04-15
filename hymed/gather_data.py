"""
gather_data.py

Helper function to gather all CSV files from a single folder into one dataframe.
Translated from R/gather_data.R.
"""

import os
import glob
import pandas as pd


def gather_data(folder_name: str, data_path: str) -> pd.DataFrame:
    """
    Gather all CSV files in a folder and concatenate into a single DataFrame.

    Parameters
    ----------
    folder_name : str
        Name of the subfolder containing the CSV files.
    data_path : str
        Path to the parent data directory.

    Returns
    -------
    pd.DataFrame
    """
    directory = os.path.join(data_path, folder_name)
    files = sorted(glob.glob(os.path.join(directory, "*.csv")))
    dfs = []
    for f in files:
        try:
            dfs.append(pd.read_csv(f, dtype={"site": str}))
        except pd.errors.EmptyDataError:
            pass
    return pd.concat(dfs, ignore_index=True)
