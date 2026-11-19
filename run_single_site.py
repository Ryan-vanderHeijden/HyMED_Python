"""
run_single_site.py

Runs drought evaluation for each site in the input data directory, identifying
drought events independently at each percentile threshold (5/10/20/30).
The workflow itself lives in hymed/pipeline.py.

Usage:
    python run_single_site.py --model nwm --data-path <dir> --out-path <dir>

Set HYMED_WORKERS to control the number of parallel processes (default 16).
"""

from hymed.pipeline import main

if __name__ == "__main__":
    main(scheme="per_threshold")
