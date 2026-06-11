# ROBUSTNESS CHECKS
#
# Implements four robustness checks adapted from the cleaned-up analysis code
# in 2026/figures/from_VM_1/new_clean_code (not included in this package):
#
#   1. Flow vs. cumulative mobility outcomes
#      (moved_address vs. moved_address_flow, etc. -- new_clean_code/08_main_regression__full_analysis_cleanedup.R)
#   2. Event-study estimates by baseline (event-time-0) flood-risk group
#      (flood_group_event0 -- new_clean_code/07_final_cleaning_variables_for_regression.R,
#       new_clean_code/08_main_regression__full_analysis_cleanedup.R)
#   3. Intent-to-treat (ITT) vs. conditional-on-movers (ATE) estimates
#      (new_clean_code/12_main_regression__full_analysis_cleanedup_conditionalMoving.R)
#   4. Flood-risk-group distribution and transition matrix among treated
#      movers, event time 0 vs. 5
#      (new_clean_code/10_figure1_histogram_plots.R)
#
# The new_clean_code versions of checks 1-3 use fixest::feols(). To avoid
# adding fixest (and patchwork, used for combining figures) as a new package
# dependency, this script reimplements the same event-study specification
# with lm() + sandwich::vcovCL() + lmtest::coeftest()/coefci(), exactly as in
# 07_main_regression_and_figure3plots.R (clustering by STATE).
#
# Two checks present in new_clean_code were intentionally NOT ported because
# they require external data not available in this package (see README):
#   - A median-home-value heterogeneity check (merges NHGIS tract data)
#   - A distance-moved histogram (requires lat/lon + geosphere)
#
# Output: figures/Robustness1_*.pdf .. Robustness4_*.pdf

library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(sandwich)
library(lmtest)
library(haven)
library(stringr)
library(scales)

rm(list = ls())

dir.create("figures", showWarnings = FALSE)

#################################################
# Load Data
#################################################

data <- read_dta("./cleandata/TOY_TreatedControlAddresses_forRegression_allyear.dta")
data$EVENTTIME <- as.numeric(data$EVENTTIME)

#################################################
# Flood-risk category (flood_group / flood_group_event0)
# Ported from new_clean_code/07_final_cleaning_variables_for_regression.R
#################################################

data <- data %>%
  mutate(
    fld_zone = trimws(as.character(fld_zone)),
    zone_subty = trimws(as.character(zone_subty)),
    fld_zone = ifelse(is.na(fld_zone) | fld_zone == "", "missing", fld_zone),
    zone_subty = ifelse(is.na(zone_subty) | zone_subty == "", "none", zone_subty),
    flood_cat = paste0(fld_zone, "__", zone_subty),
    flood_cat = factor(flood_cat)
  )

setDT(data)
data[, flood_group := case_when(
  str_detect(flood_cat, "FLOODWAY") ~ "floodway",
  str_detect(flood_cat, "VE|COASTAL") ~ "coastal_1pct",
  str_detect(flood_cat, "AE|^A_|AO|AH") ~ "riverine_1pct",
  str_detect(flood_cat, "0.2 PCT") ~ "risk_0.2pct",
  str_detect(flood_cat, "REDUCED FLOOD RISK") ~ "levee",
  str_detect(flood_cat, "MINIMAL FLOOD HAZARD|^X$") ~ "minimal",
  TRUE ~ "other"
)]

base_flood <- data[EVENTTIME == 0, .(flood_group_event0 = first(na.omit(flood_group))), by = PID]
data <- merge(data, base_flood, by = "PID", all.x = TRUE)
data[, flood_group_event0 := factor(flood_group_event0)]

data <- as.data.frame(data)

#################################################
# Flow outcomes (vs. the cumulative outcomes already in the toy data)
# Ported from new_clean_code/08_main_regression__full_analysis_cleanedup.R
#################################################

data <- data %>%
  arrange(PID, EVENTTIME) %>%
  group_by(PID) %>%
  mutate(
    moved_address_flow = as.integer(moved_address == 1 & lag(moved_address, default = 0) == 0),
    moved_zip_flow     = as.integer(MOVED_ZIP == 1 & lag(MOVED_ZIP, default = 0) == 0),
    moved_state_flow   = as.integer(MOVED_STATE == 1 & lag(MOVED_STATE, default = 0) == 0)
  ) %>%
  ungroup() %>%
  as.data.frame()

#################################################
# EVENTTIME as factor (ref = -1), matching 07_main_regression_and_figure3plots.R
#################################################

data$EVENTTIME <- relevel(as.factor(data$EVENTTIME), ref = "-1")

#################################################
# SFHA control subset, same construction as 07_main_regression_and_figure3plots.R
#################################################

data_3 <- data %>%
  group_by(PID) %>%
  filter(0 %in% treated_sfha_3 | 1 %in% treated_sfha_3) %>%
  ungroup()

#################################################
# Helpers
#################################################

# Run the Fig3-style event-study regression for `outcome_var` and return the
# Treated:as.factor(EVENTTIME) coefficients with clustered (by STATE) CIs,
# in the same form as m1coeffs_move / m1cis_plot in 07_main_regression_and_figure3plots.R.
run_event_study <- function(outcome_var, data) {

  data$EVENTTIME <- droplevels(data$EVENTTIME)

  fmla <- as.formula(paste0(
    outcome_var,
    " ~ as.factor(EVENTYEAR) + as.factor(STATE) + as.factor(EVENTYEAR):as.factor(STATE) + ",
    "Treated + as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME)"
  ))

  m <- lm(fmla, data = data)

  m_coefs <- data.frame(summary(m)$coefficients)
  coi <- which(startsWith(rownames(m_coefs), "Treated:as.factor(EVENTTIME)"))

  event_times <- as.numeric(sub("^Treated:as\\.factor\\(EVENTTIME\\)", "", rownames(m_coefs)[coi]))

  cl_vcov <- vcovCL(m, cluster = ~STATE)
  cis <- as.data.frame(coefci(m, parm = coi, vcov = vcovCL, cluster = ~STATE))

  coefs <- data.frame(
    EVENTTIME = event_times,
    Estimate  = m_coefs[coi, "Estimate"],
    ci_lower  = cis[, 1],
    ci_upper  = cis[, 2]
  )

  # add the omitted reference period (EVENTTIME == -1) back in as zero
  coefs <- rbind(coefs, data.frame(EVENTTIME = -1, Estimate = 0, ci_lower = 0, ci_upper = 0))
  coefs <- coefs[order(coefs$EVENTTIME), ]
  rownames(coefs) <- NULL

  list(model = m, coefs = coefs)
}

# Overlay multiple event-study coefficient series (named list of data.frames
# from run_event_study()$coefs) on one plot.
plot_event_study_comparison <- function(coefs_list, title, ylab, xlim = c(-10, 15)) {
  combined <- bind_rows(lapply(names(coefs_list), function(nm) {
    df <- coefs_list[[nm]]
    df$series <- nm
    df
  }))
  combined$series <- factor(combined$series, levels = names(coefs_list))

  ggplot(combined, aes(x = EVENTTIME, y = Estimate, colour = series, fill = series)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, colour = NA) +
    geom_line() +
    labs(x = "Event Time (Years)", y = ylab, title = title, colour = NULL, fill = NULL) +
    theme_classic() +
    theme(panel.grid = element_blank(), legend.position = "bottom") +
    scale_x_continuous(limits = xlim) +
    geom_vline(xintercept = 0, col = "red") +
    geom_hline(yintercept = 0)
}

#################################################
# Robustness check 1: Flow vs. cumulative mobility outcomes
#################################################
# Compares the cumulative "ever moved by event time t" outcomes used in
# Figure 3 to "flow" outcomes that are 1 only in the period the household
# first moves. Run on the same SFHA-control sample (data_3) as Fig3a/b.

flow_vs_cumulative <- list(
  "Moved address"  = list(cum = "moved_address",  flow = "moved_address_flow",  ylab = "Likelihood of moving to a new address"),
  "Moved zip code" = list(cum = "MOVED_ZIP",       flow = "moved_zip_flow",      ylab = "Likelihood of moving to a new zip code"),
  "Moved state"    = list(cum = "MOVED_STATE",     flow = "moved_state_flow",    ylab = "Likelihood of moving to a new state")
)

pdf("figures/Robustness1_flow_vs_cumulative.pdf", width = 8, height = 6)
for (nm in names(flow_vs_cumulative)) {
  spec <- flow_vs_cumulative[[nm]]
  es_cum  <- run_event_study(spec$cum,  data_3)
  es_flow <- run_event_study(spec$flow, data_3)

  p <- plot_event_study_comparison(
    list("Cumulative (ever by t)" = es_cum$coefs,
         "Flow (in year t)"       = es_flow$coefs),
    title = paste0(nm, ": cumulative vs. flow outcome"),
    ylab  = spec$ylab
  )
  print(p)
}
dev.off()

#################################################
# Robustness check 2: Subgroups by baseline (event time 0) flood-risk group
#################################################
# Re-estimates the moved_address event study (Fig3a specification) separately
# within each baseline flood_group_event0 category, on the full sample
# (`data`, not the SFHA-control subset data_3).

flood_levels <- levels(data$flood_group_event0)

subgroup_coefs <- list()
for (fg in flood_levels) {
  sub_data <- data[!is.na(data$flood_group_event0) & data$flood_group_event0 == fg, ]
  if (length(unique(sub_data$Treated)) < 2) next
  es <- run_event_study("moved_address", sub_data)
  subgroup_coefs[[fg]] <- es$coefs
}

p2 <- plot_event_study_comparison(
  subgroup_coefs,
  title = "Likelihood of moving to a new address, by baseline flood-risk group",
  ylab  = "Likelihood of moving to a new address",
  xlim  = c(-15, 20)
)

pdf("figures/Robustness2_subgroup_by_baseline_floodgroup.pdf", width = 9, height = 6)
print(p2)
dev.off()

#################################################
# Robustness check 3: ITT vs. conditional-on-movers (ATE)
#################################################
# ITT uses the full SFHA-control sample (data_3, same as Fig3a). The ATE
# sample restricts treated households to those observed moving at some
# point at or after the buyout (EVENTTIME >= 0); control households are
# unchanged.
#
# NOTE: in this synthetic toy data, 00-create-toy-data-for-replication.R
# guarantees that *every* treated household has at least one post-buyout
# move (so that the Stata post-5-year script's has_moved_5yr filter does not
# drop treated PIDs). As a result, the ATE sample below is identical to the
# ITT sample and the two series in this figure will overlap exactly. On real
# data, where not all treated households move, the two series would differ.

es_itt <- run_event_study("moved_address", data_3)

movers_post <- data_3 %>%
  filter(Treated == 1, as.numeric(as.character(EVENTTIME)) >= 0, moved_address == 1) %>%
  distinct(PID) %>%
  pull(PID)

data_3_ate <- data_3 %>%
  filter(Treated == 0 | PID %in% movers_post)

es_ate <- run_event_study("moved_address", data_3_ate)

p3 <- plot_event_study_comparison(
  list("ITT (all treated)" = es_itt$coefs,
       "ATE (conditional on movers)" = es_ate$coefs),
  title = "Likelihood of moving to a new address: ITT vs. conditional-on-movers (ATE)",
  ylab  = "Likelihood of moving to a new address"
)

pdf("figures/Robustness3_ITT_vs_ATE_movers.pdf")
print(p3)
dev.off()

#################################################
# Robustness check 4: Flood-group distribution & transitions among treated movers
# Ported from new_clean_code/10_figure1_histogram_plots.R
#################################################
# Among treated households that moved at or after the buyout, shows the
# distribution of flood_group at event time 0 vs. event time 5, and the
# event-0-to-event-5 transition matrix.

movers_post_pids <- data %>%
  filter(Treated == 1, as.numeric(as.character(EVENTTIME)) >= 0, moved_address == 1) %>%
  distinct(PID) %>%
  pull(PID)

mover_flood <- data %>%
  filter(PID %in% movers_post_pids,
         as.numeric(as.character(EVENTTIME)) %in% c(0, 5),
         !is.na(flood_group)) %>%
  distinct(PID, EVENTTIME, flood_group) %>%
  mutate(
    EVENTTIME_NUM = as.numeric(as.character(EVENTTIME)),
    event_period = factor(EVENTTIME_NUM, levels = c(0, 5),
                           labels = c("Event time = 0", "Event time = 5")),
    flood_group = factor(flood_group)
  )

# -- distribution --
flood_counts <- mover_flood %>%
  count(event_period, flood_group, name = "n") %>%
  group_by(event_period) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

p4a <- ggplot(flood_counts, aes(x = flood_group, y = share, fill = event_period)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
  labs(x = "Flood group", y = "Share of treated movers",
       title = "Flood-risk group among treated movers: event time 0 vs. 5",
       fill = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Event time = 0" = "#D2B48C", "Event time = 5" = "#8B4513")) +
  theme_classic() +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

pdf("figures/Robustness4_floodgroup_distribution_event0_event5.pdf", width = 8, height = 5)
print(p4a)
dev.off()

# -- transition matrix --
transitions <- mover_flood %>%
  select(PID, EVENTTIME_NUM, flood_group) %>%
  pivot_wider(id_cols = PID, names_from = EVENTTIME_NUM, values_from = flood_group,
              names_prefix = "event_") %>%
  filter(!is.na(event_0), !is.na(event_5)) %>%
  transmute(flood_group_t0 = event_0, flood_group_t5 = event_5)

transition_counts <- transitions %>%
  count(flood_group_t0, flood_group_t5, name = "n") %>%
  group_by(flood_group_t0) %>%
  mutate(row_total = sum(n), share = n / row_total) %>%
  ungroup()

all_groups <- sort(unique(c(as.character(transition_counts$flood_group_t0),
                             as.character(transition_counts$flood_group_t5))))

transition_counts <- transition_counts %>%
  mutate(
    flood_group_t0 = factor(flood_group_t0, levels = all_groups),
    flood_group_t5 = factor(flood_group_t5, levels = all_groups)
  ) %>%
  complete(flood_group_t0, flood_group_t5, fill = list(n = 0, row_total = 0, share = 0))

p4b <- ggplot(transition_counts, aes(x = flood_group_t5, y = flood_group_t0, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(share > 0, paste0(round(100 * share), "%\n(n=", n, ")"), "")), size = 3) +
  scale_fill_gradient(low = "#F5E6D3", high = "#8B4513",
                       labels = scales::percent_format(accuracy = 1), name = "Row share") +
  labs(x = "Flood group at event time 5", y = "Flood group at event time 0",
       title = "Flood-group transitions among treated mover properties",
       subtitle = "Rows sum to 100% within event-time-0 flood group") +
  theme_classic() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

pdf("figures/Robustness4_floodgroup_transition_event0_to_event5.pdf", width = 8, height = 6)
print(p4b)
dev.off()

message("Robustness-check figures written to figures/Robustness1_*.pdf .. Robustness4_*.pdf")
