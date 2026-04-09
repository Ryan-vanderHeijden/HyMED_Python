"""
prepare_obs_input.py

Prepares observed-only input CSVs for HyMED evaluation.

Reads existing model input CSVs (default: nwm), duplicates q_cms_obs as
q_cms_obs_ref, drops the model flow column, and writes the result to
inst/extdata/obs/input_data/. Run this once before running run_single_site.py
with --model obs_ref --data-path inst/extdata/obs.

Usage:
    python prepare_obs_input.py
    python prepare_obs_input.py --source-model nwm
"""

import os
import glob
import argparse

import pandas as pd


def prepare_obs_inputs(source_model: str = "nwm") -> None:
    src_dir = os.path.join("inst", "extdata", source_model, "input_data")
    dst_dir = os.path.join("inst", "extdata", "obs", "input_data")
    os.makedirs(dst_dir, exist_ok=True)

    src_files = sorted(glob.glob(os.path.join(src_dir, "*.csv")))
    if not src_files:
        raise FileNotFoundError(f"No CSV files found in {src_dir}")

    model_col = f"q_cms_{source_model}"

    for fpath in src_files:
        site = os.path.basename(fpath)
        df = pd.read_csv(fpath, dtype={"site": str})

        if model_col not in df.columns:
            raise KeyError(f"{site}: expected column '{model_col}' not found")

        df["q_cms_obs_ref"] = df["q_cms_obs"]
        df = df.drop(columns=[model_col])

        df.to_csv(os.path.join(dst_dir, site), index=False)
        print(f"written: {site}")

    print(f"\nDone. {len(src_files)} files written to {dst_dir}")
    print("Run evaluation with:")
    print("  python run_single_site.py --model obs_ref --data-path inst/extdata/obs")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Prepare observed-only input CSVs for HyMED."
    )
    parser.add_argument(
        "--source-model",
        default="nwm",
        help="Model directory to source input files from (default: nwm)",
    )
    args = parser.parse_args()
    prepare_obs_inputs(source_model=args.source_model)
