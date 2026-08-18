# RUN ALL: one-click replication entry point
#
# Run from the repository root:
#
#   Rscript run_all.R
#
# This regenerates the synthetic toy data, runs the main event-study
# analysis (Figure 3 + SI figures), runs the robustness-check figures, and
# runs the benefit-cost analysis (BCA). All outputs are written to
# ./cleandata/, ./figures/, and ./output/.
#
# The BCA step (scripts/02_main_analysis/BCA.R, BCA_figures.R) does NOT
# depend on the synthetic toy data or the regression steps above -- it reads
# only ./rawdata/bca_inputs/ and a set of literal preferred/low/high
# assumptions in the script itself, so it reproduces the actual benefit-cost
# numbers reported in the manuscript, not placeholder results.
#
# The Stata step (scripts/02_main_analysis/08_TreatedControl_post5year_regressions_figure4plots.do,
# which produces Figure 4) is NOT run by this script, since it requires a
# Stata license. See README.txt for instructions on running it separately.

dir.create("cleandata", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

message("=== Step 1/4: generating synthetic toy data ===")
source("scripts/00-create-toy-data-for-replication.R")

message("=== Step 2/4: main event-study regressions (Figure 3 + SI figures) ===")
source("scripts/02_main_analysis/07_main_regression_and_figure3plots.R")

message("=== Step 3/4: robustness checks (Robustness1-4 figures) ===")
source("scripts/02_main_analysis/09_robustness_checks.R")

message("=== Step 4/4: benefit-cost analysis (BCA) ===")
source("scripts/02_main_analysis/BCA.R")
source("scripts/02_main_analysis/BCA_figures.R")

message("=== Done. Figures written to ./figures/, BCA tables to ./output/bca/ ===")
message("Note: the Stata step (08_TreatedControl_post5year_regressions_figure4plots.do, Figure 4) is separate -- see README.txt.")
