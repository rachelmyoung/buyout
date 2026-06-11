# RUN ALL: one-click replication entry point
#
# Run from the repository root:
#
#   Rscript run_all.R
#
# This regenerates the synthetic toy data, runs the main event-study
# analysis (Figure 3 + SI figures), and runs the new robustness-check
# figures. All outputs are written to ./cleandata/ and ./figures/.
#
# The Stata step (scripts/02_main_analysis/08_TreatedControl_post5year_regressions_figure4plots.do,
# which produces Figure 4) is NOT run by this script, since it requires a
# Stata license. See README.txt for instructions on running it separately.

dir.create("cleandata", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

message("=== Step 1/3: generating synthetic toy data ===")
source("scripts/00-create-toy-data-for-replication.R")

message("=== Step 2/3: main event-study regressions (Figure 3 + SI figures) ===")
source("scripts/02_main_analysis/07_main_regression_and_figure3plots.R")

message("=== Step 3/3: robustness checks (Robustness1-4 figures) ===")
source("scripts/02_main_analysis/09_robustness_checks.R")

message("=== Done. Figures written to ./figures/ ===")
message("Note: the Stata step (08_TreatedControl_post5year_regressions_figure4plots.do, Figure 4) is separate -- see README.txt.")
