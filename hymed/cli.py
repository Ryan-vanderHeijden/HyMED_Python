"""
cli.py

Command-line interface: `hymed run ...` (or `python -m hymed run ...`).
"""

import argparse

from .pipeline import run_evaluation


def _add_run_parser(subparsers) -> None:
    p = subparsers.add_parser(
        "run",
        help="Evaluate modeled vs observed streamflow drought for every site.",
        description="Evaluate every site in <data-path>/input_data and write the combined "
                    "long-format outputs to <out-path>.",
    )
    p.add_argument("--data-path", required=True,
                   help="Directory containing input_data/<site>.csv (may be read-only).")
    p.add_argument("--out-path", required=True, help="Directory for the outputs.")
    p.add_argument("--model", required=True,
                   help="Model name; selects the q_cms_<model> input column (e.g. nwm).")
    p.add_argument("--universal", action="store_true",
                   help="Identify events at the 30th percentile and label each with a "
                        "severity class, instead of identifying events per threshold.")
    p.add_argument("--ref-sites", default=None,
                   help="CSV with columns 'site' and 'Ref'; only sites with Ref == 1 are evaluated.")
    p.add_argument("--skip-existing", action="store_true",
                   help="Resume: skip sites whose checkpoint in <out-path>/sites/ is complete.")
    p.add_argument("--write-percentiles", action="store_true",
                   help="Also write daily percentiles per site to <out-path>/percentiles/.")
    p.add_argument("--workers", type=int, default=None,
                   help="Number of parallel processes (default: $HYMED_WORKERS, else 16).")


def main(argv: list = None) -> None:
    parser = argparse.ArgumentParser(
        prog="hymed", description="HyMED: Hydrologic Model Evaluation for Drought."
    )
    subparsers = parser.add_subparsers(dest="command", required=True)
    _add_run_parser(subparsers)
    args = parser.parse_args(argv)

    if args.command == "run":
        run_evaluation(
            data_path=args.data_path,
            out_path=args.out_path,
            model=args.model,
            scheme="universal" if args.universal else "per_threshold",
            ref_sites=args.ref_sites,
            skip_existing=args.skip_existing,
            write_percentiles=args.write_percentiles,
            workers=args.workers,
        )
