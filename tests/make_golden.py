"""
Regenerate the frozen inputs and golden regression outputs in tests/golden/.

Only run this when an output change is intended; the refactor is supposed to
leave every golden file untouched. Layout:

  golden/input_data/<site>.csv.gz      synthetic inputs (tests/synthetic.py)
  golden/<scheme>/.../<file>.csv.gz    outputs of each workflow runner

Usage (from the repo root):
    python tests/make_golden.py            # outputs only, from the frozen inputs
    python tests/make_golden.py --inputs   # also regenerate the synthetic inputs
"""

import argparse
import glob
import gzip
import os
import shutil
import tempfile

from synthetic import write_inputs
from workflow import GOLDEN, GOLDEN_INPUTS, SCHEMES, output_files, run_workflow


def _gzip(src_path: str, dst_path: str) -> None:
    os.makedirs(os.path.dirname(dst_path), exist_ok=True)
    # mtime=0 and no filename keep the gzip bytes stable across regenerations.
    with open(src_path, "rb") as src, open(dst_path, "wb") as raw, \
            gzip.GzipFile(fileobj=raw, mode="wb", mtime=0, filename="") as gz:
        shutil.copyfileobj(src, gz)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[1])
    parser.add_argument("--inputs", action="store_true",
                        help="regenerate the synthetic inputs too")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory() as tmp:
        if args.inputs or not glob.glob(os.path.join(GOLDEN_INPUTS, "*.csv.gz")):
            shutil.rmtree(GOLDEN_INPUTS, ignore_errors=True)
            raw = os.path.join(tmp, "inputs")
            for site in write_inputs(raw):
                _gzip(os.path.join(raw, f"{site}.csv"),
                      os.path.join(GOLDEN_INPUTS, f"{site}.csv.gz"))
            print(f"inputs: regenerated in {GOLDEN_INPUTS}")

        for scheme in SCHEMES:
            shutil.rmtree(os.path.join(GOLDEN, scheme), ignore_errors=True)
            work = run_workflow(scheme, tmp)
            files = output_files(work)
            for rel in files:
                _gzip(os.path.join(work, rel), os.path.join(GOLDEN, scheme, rel + ".gz"))
            print(f"{scheme}: {len(files)} files")


if __name__ == "__main__":
    main()
