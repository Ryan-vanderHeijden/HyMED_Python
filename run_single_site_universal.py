"""
run_single_site_universal.py

Runs drought evaluation for each site in the input data directory using the
"universal" drought classification scheme (see hymed/calculate_properties_universal.py):
events are identified at the 30th percentile and each event is labeled with one
severity class (5/10/20/30) from the minimum percentile it reaches. The per-event
``threshold`` column holds that class. The workflow itself lives in hymed/pipeline.py.

Usage:
    python run_single_site_universal.py --model nwm --out-path <dir>

Output defaults to inst/extdata/<model>_universal. Set HYMED_WORKERS to control the
number of parallel processes (default 16).
"""

from hymed.pipeline import main

if __name__ == "__main__":
    main(scheme="universal")
