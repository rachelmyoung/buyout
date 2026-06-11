\\-----------------------------------------------------------------------------
README: Replication Package for "Does Government-Assisted Relocation Help Households"
Author: Rachel Young
Last Updated: 06-10-2026
Contact: rmyoung@umn.edu
\\-----------------------------------------------------------------------------



This repository contains all code and materials necessary to reproduce the results, figures, and tables presented in the paper “Does Government-Assisted Relocation Help Households”.

Because the analysis uses proprietary address-level data that cannot be publicly released, this archive provides:

1. A fully synthetic ("toy") dataset that mirrors the structure and variable names of the confidential data. All values (addresses, ZIP codes, flood-zone codes, incomes, outcomes, etc.) are randomly generated and have no relationship to any real household, property, or location -- they exist only so the code can be run end to end.
2. All analysis code used in the paper, beginning from the cleaned analysis dataset.
3. A description of the non-public data cleaning, matching, and control-group construction steps.


\\-----------------------------------------------------------------------------
\\ Quick Start (One-Click Run)
\\-----------------------------------------------------------------------------

From the repository root, run:

    Rscript run_all.R

This single command:
  1. Regenerates the synthetic toy data (scripts/00-create-toy-data-for-replication.R)
  2. Runs the main event-study regressions and produces Figure 3 + SI figures
     (scripts/02_main_analysis/07_main_regression_and_figure3plots.R)
  3. Runs the new robustness-check analyses and produces Robustness1-4 figures
     (scripts/02_main_analysis/09_robustness_checks.R)

All figures are written to /figures. No Stata is required for this step.

The post-5-year heterogeneity regressions and Figure 4
(scripts/02_main_analysis/08_TreatedControl_post5year_regressions_figure4plots.do)
require Stata and are run separately -- see "Replication Steps" below.

Because every input is synthetic random data, the resulting figures show no
meaningful patterns (estimates centered near zero with wide confidence
intervals) -- they confirm that the code runs, not that the paper's results
hold.


\\-----------------------------------------------------------------------------
\\ Set Up
\\-----------------------------------------------------------------------------

Scripts provided are written in Stata and R. Note that you will need a Stata license to fully replicate the analysis. Throughout this ReadMe, when indicating paths to code and data, it is assumed that you’ll execute scripts from the repo root directory.

To estimate run all the scripts, you will need several packages installed in Stata. To add them, launch Stata and run:

ssc install estout, replace
ssc install rsource, replace
ssc install outreg2, replace
net install grc1leg, from(http://www.stata.com/users/vwiggins/) 

You will also need several R packages. To add them, launch R and run:

install.packages(c(
  "dplyr",
  "haven",
  "tidyr",
  "ggplot2",
  "data.table",
  "stringr",
  "scales",
  "sandwich",
  "lmtest"
))

(Earlier versions of this package also loaded foreign, tidyverse, readr, DBI,
rlang, and fastLink, but none of these are actually used by the R scripts in
this package, so they are no longer required.)

\\-----------------------------------------------------------------------------
\\ File Structure
\\-----------------------------------------------------------------------------
/run_all.R				One-click entry point (see Quick Start above)
/cleandata 				Stores the data needed for the replication
/rawdata 				Stores the data that is publicly sharable
/scripts				Stores all the Stata and R scripts
/figures				All figures (Figure 3, SI, and robustness checks) go here
/output					Stata-only outputs (estimates, CSVs) from script 08 (requires Stata)


\\-----------------------------------------------------------------------------
\\ Data Documentation
\\-----------------------------------------------------------------------------

A detailed description of the data used in this analysis can be found in the Supplementary Information associated with the article. However, there are several datasets that cannot be made public. 

Public or Shareable Data (Included):
	- cleandata/TOY_TreatedControl_post5year.dta: synthetic post-5-year subset (EVENTTIME in {-1, 5}), used by script 08 (Stata)
	- cleandata/TOY_TreatedControlAddresses_forRegression_allyear.dta: synthetic dataset with all variables used in analysis, including:
		- fld_zone, zone_subty: synthetic raw FEMA flood-zone code and subtype, used to derive flood_group / flood_group_event0 (floodway, coastal_1pct, riverine_1pct, risk_0.2pct, levee, minimal, other) for the robustness checks
		- fld_zone_factor: synthetic numeric SFHA indicator (3 = Special Flood Hazard Area), used to construct the SFHA outcome in Figure 3
		- frac_employed_16plus_2000_TEST, frac_college_plus: synthetic destination-tract employment/education shares, used to construct the "more employed" / "more educated" outcomes in Figure 3
	Both .dta files are regenerated by scripts/00-create-toy-data-for-replication.R; every value in them is randomly generated (set.seed(123)) and is not derived from or linked to any real address, household, or location.
	- rawdata/femabuyouts.csv: FEMA buyout addresses (raw and the shape files are the geocoded version).
	- rawdata/uniqueAddress_20002017_GeoCoded_joinCensusTract2000_ACS2000_NFHLall.csv: geocoded addresses merged with NFHL (including infutor addresses, no ids, and fema buyout addresses)


Confidential Data (Not Included):
The main dataset is derived from Infutor consumer address histories and credit histories, which are merged to:
FEMA buyout addresses
Census tract characteristics (ACS)
Flood-zone classifications (SFHA, 100-yr / 500-yr floodplain)
These raw datasets contain personally identifiable and sensitive information and cannot be shared.



\\-----------------------------------------------------------------------------
\\ Replication Steps
\\-----------------------------------------------------------------------------

There are five stages to our analysis:

1. Data collection and processing
2. Regression model estimation
3. Figure creation 

\\-----------------------------------------------------------------------------

1. Data Collection and Preprocessing

All raw-data preparation scripts are located in:

    /scripts/01_preprocessing_CANNOTBERUN/

These scripts reproduce the full construction of the analysis dataset,
including address-history cleaning, geocoding merges, and treatment/control
cohort assembly. These files cannot be executed without licensed access
to Infutor Consumer Address Histories and other restricted datasets.

To enable replication, a toy dataset is provided that mirrors the structure
of the final processed data. This allows users to run all analysis scripts
and generate placeholder regression outputs and figures.


\\-----------------------------------------------------------------------------


2. Main Analysis (Runs with Toy Data)

All estimations and figure generation that are replicable without proprietary
data are stored in:

    /scripts/02_main_analysis/

The primary scripts are:

    07_main_regression_and_figure3plots.R
        - Runs event-study and summary regressions
        - Produces Figure 3 and related intermediate outputs

    08_TreatedControl_post5year_regressions_figure4plots.do
        - Runs the post-5-year treated vs. control regressions
        - Produces Figure 4 and all heterogeneous-effects coefficient plots

    09_robustness_checks.R
        - Runs four additional robustness checks on top of the Figure 3
          specification:
            1. Flow vs. cumulative mobility outcomes (does the effect on
               moved_address / MOVED_ZIP / MOVED_STATE come from a one-time
               "flow" move vs. an accumulating "cumulative ever moved"
               outcome?)
            2. The moved_address event study re-estimated separately within
               each baseline (event-time-0) flood-risk subgroup
               (flood_group_event0: floodway, coastal_1pct, riverine_1pct,
               risk_0.2pct, levee, minimal, other)
            3. Intent-to-treat (ITT, all treated households) vs.
               conditional-on-movers (ATE, treated households restricted to
               those observed moving at or after the buyout)
            4. The distribution of flood_group among treated movers at event
               time 0 vs. event time 5, and the event-0-to-event-5 transition
               matrix between flood groups
        - Produces figures/Robustness1_*.pdf .. Robustness4_*.pdf

07 and 09 are R scripts and are fully runnable using the provided toy
datasets located in:

    /cleandata/

08 is a Stata .do file and requires a Stata license (see "Set Up" above).

Two further robustness checks exist in the cleaned-up analysis code this
package was updated from (2026/figures/from_VM_1/new_clean_code) but are
NOT included here, because they require external data not part of this
package:
    - A heterogeneity check by destination-tract median home value, which
      requires merging in NHGIS census-tract data
    - A histogram of the distance moved (in miles), which requires
      household-level latitude/longitude and the geosphere package
If you have access to the underlying confidential data plus NHGIS / lat-lon
files, these checks can be reconstructed following the same pattern as
09_robustness_checks.R.

\\-----------------------------------------------------------------------------


3. Figure Creation

All figures in the paper—event-study trends, difference-in-means plots,
and heterogeneous-effects coefficient plots—are generated directly within
scripts 07 and 08. The robustness-check figures (Robustness1-4) are
generated by script 09.

Output figures are automatically saved into:

    /figures/
