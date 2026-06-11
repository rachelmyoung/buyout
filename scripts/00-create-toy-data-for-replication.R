library(dplyr)
library(haven)
library(tidyr)

#setwd("./replication_package")

set.seed(123)

########################################################
# 1. BASIC PANEL STRUCTURE
########################################################

N_pid          <- 4000          # number of unique households
EVENTTIME_vals <- -15:20        # 36 periods

PID <- 1:N_pid

toy <- expand.grid(
  PID       = PID,
  EVENTTIME = EVENTTIME_vals
)

# We'll give each row a calendar year (EVENTYEAR) later, independent of EVENTTIME.

########################################################
# 2. PID-LEVEL ATTRIBUTES (state, county, treatment)
########################################################

pid_level <- data.frame(
  PID      = PID,
  ADDRESS  = paste0("ADDR", PID),
  CITY     = sample(c("Aville", "Btown", "Ccity"), N_pid, replace = TRUE),
  STATE    = sample(c("TX","FL","NC","NJ","CA"), N_pid, replace = TRUE),
  statefp  = sample(1:5, N_pid, replace = TRUE),   # just 5 numeric states to match the 5 strings
  countyfp = sample(100:999, N_pid, replace = TRUE),
  Treated  = rbinom(N_pid, 1, 0.4)                 # treatment is constant within PID
)

toy <- toy %>%
  left_join(pid_level, by = "PID")

# Row-level ZIP (can vary over time, doesn't matter)
toy$ZIP <- sample(10000:99999, nrow(toy), replace = TRUE)

########################################################
# 3. CALENDAR YEAR & GROUP STRUCTURE FOR FE MODELS
########################################################

# Assign EVENTYEAR *at the row level*, independent of EVENTTIME.
# This avoids perfect collinearity between year FE and post_5yr.
toy$EVENTYEAR <- sample(1995:2010, nrow(toy), replace = TRUE)

########################################################
# 4. HETEROGENEITY DUMMIES & POST INDICATOR
#    (ensuring variation within statefp x EVENTYEAR cells)
########################################################

toy <- toy %>%
  group_by(statefp, EVENTYEAR) %>%
  mutate(
    # "Post" indicator (not tied mechanically to EVENTTIME – it's just a toy)
    post_5yr          = rbinom(n(), 1, 0.5),
    
    # Heterogeneity splits
    high_frac_black   = rbinom(n(), 1, 0.5),
    high_frac_black_75= rbinom(n(), 1, 0.5),
    treated_sfha_3    = rbinom(n(), 1, 0.5),
    high_income_percap = rbinom(n(), 1, 0.5),
    high_median_income = rbinom(n(), 1, 0.5),
  ) %>%
  ungroup()

# A continuous income and race share for Stata to build other splits from if needed
toy$fam_income_percap <- round(runif(nrow(toy), 5000, 100000))
toy$frac_black        <- runif(nrow(toy), 0, 1)

# Census-tract characteristics used to build the "richer / more employed /
# more educated" destination-tract outcomes in 07_main_regression_and_figure3plots.R
toy$frac_employed_16plus_2000_TEST <- runif(nrow(toy), 0, 1)
toy$frac_college_plus              <- runif(nrow(toy), 0, 1)

########################################################
# 5. OUTCOME VARIABLES (STRUCTURED BUT ARBITRARY)
########################################################

# We want all outcomes to vary within the same FE cells as above so
# the regressions don't hit degenerate cases.

toy <- toy %>%
  group_by(statefp, EVENTYEAR) %>%
  mutate(
    moved_address = rbinom(n(), 1, 0.15),
    MOVED_ZIP     = rbinom(n(), 1, 0.06),
    MOVED_STATE   = rbinom(n(), 1, 0.03),
    richer        = rbinom(n(), 1, 0.25)    # arbitrary "richer" dummy for outcome
  ) %>%
  ungroup()

# Make sure *treated* PIDs are not dropped by your Stata filter:
# has_moved_5yr = max(moved_address==1 & EVENTTIME>=0)
# drop if has_moved_5yr==0 & Treated==1
# → ensure each treated PID has *some* move at EVENTTIME >= 0.

toy <- toy %>%
  group_by(PID) %>%
  mutate(
    any_move_post = any(moved_address == 1 & EVENTTIME >= 0 & Treated == 1)
  ) %>%
  mutate(
    # If treated and we never moved post-0, force one move at the first post period
    moved_address = ifelse(
      Treated == 1 & EVENTTIME >= 0 & !any_move_post & row_number() == min(which(EVENTTIME >= 0)),
      1,
      moved_address
    )
  ) %>%
  ungroup() %>%
  select(-any_move_post)

########################################################
# 5b. FLOOD-ZONE CATEGORY (fld_zone / zone_subty / fld_zone_factor)
########################################################
# fld_zone / zone_subty are raw FEMA-style flood-zone fields. They are used
# downstream (07_main_regression_and_figure3plots.R and the robustness-check
# script) to derive flood_group / flood_group_event0 (floodway, coastal_1pct,
# riverine_1pct, risk_0.2pct, levee, minimal, other) and the SFHA indicator
# fld_zone_factor (3 = in the Special Flood Hazard Area).
#
# Each row gets an independent draw, so the same PID can show up in different
# flood groups across EVENTTIME (e.g. after a move) -- this gives the
# event-time-0-vs-5 transition matrix something to show.

flood_zone_lookup <- data.frame(
  fld_zone        = c("AE",   "VE",   "FLOODWAY", "X",    "X",    "X",      "D"),
  zone_subty      = c("none", "none", "none",
                       "0.2 PCT ANNUAL CHANCE FLOOD HAZARD",
                       "REDUCED FLOOD RISK DUE TO LEVEE",
                       "MINIMAL FLOOD HAZARD",
                       "none"),
  fld_zone_factor = c(3,      3,      4,          1,      1,      1,        1),
  prob            = c(0.20,   0.10,   0.05,       0.15,   0.10,   0.30,     0.10)
)
# fld_zone_factor == 3 corresponds to the "riverine_1pct"/"coastal_1pct"
# (AE/VE) flood groups, i.e. ~30% of rows -- matching the previous toy SFHA rate.

flood_draw <- flood_zone_lookup[
  sample.int(nrow(flood_zone_lookup), nrow(toy), replace = TRUE, prob = flood_zone_lookup$prob),
]

toy$fld_zone        <- flood_draw$fld_zone
toy$zone_subty      <- flood_draw$zone_subty
toy$fld_zone_factor <- flood_draw$fld_zone_factor

########################################################
# 6. HOUSE VALUES (USED IN OTHER SCRIPTS / HISTOGRAMS)
########################################################

toy$pricepaid <- rnorm(nrow(toy), mean = 50000, sd = 25000)

########################################################
# 7. SAVE FULL PANEL & POST5YEARS SUBSET
########################################################

# Full panel toy data for the big R script
write_dta(toy, "./cleandata/TOY_TreatedControlAddresses_forRegression_allyear.dta")

# Subset used by your Stata heterogeneity script:
# (EVENTTIME == -1 or 5) – matches your original post-5-year design.
data_sub <- toy %>%
  filter(EVENTTIME %in% c(-1, 5))

write_dta(data_sub, "cleandata/TOY_TreatedControl_post5year.dta")

message("Toy datasets created.")
