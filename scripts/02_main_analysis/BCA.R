############################################################
# Buyout Benefit-Cost Analysis with Monte Carlo Sensitivity
# Purpose: Fiscal/programmatic BCA aligned with estimated effects,
#          realistic program costs, administrative costs,
#          per-property and aggregate estimates, and uncertainty analysis.
#
# Author: Rachel Young
# Date: 2026-05-11
############################################################

# -----------------------------
# 0. Packages
# -----------------------------

library(data.table)
library(ggplot2)

# -----------------------------
# 1. User inputs
# -----------------------------

# Project directories
setwd("/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram")
root_dir   <- "2026"
out_dir    <- file.path(root_dir, "data/intermediate/bca_outputs")
fig_dir    <- file.path(root_dir, "figures", "bca")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Total number of buyout properties in the analysis/program universe.
# Used only to scale per-property estimates into aggregate program totals.
n_buyout_properties <- 44000

# Monte Carlo settings
set.seed(12345)
nsim <- 50000

# Discount rates for deterministic sensitivity
real_discount_rates <- c(0.01, 0.02, 0.03, 0.05, 0.07)
preferred_discount_rate <- 0.03

# Analysis horizon after buyout.
horizons <- c(10, 20, 30)
preferred_horizon <- 30

# -----------------------------
# 2. Core BCA assumption table
# -----------------------------

# Notes:
# - This is a fiscal/programmatic BCA, not a full welfare analysis.
# - Costs are one-time program costs per treated buyout property.
# - Benefits are annual expected avoided public costs per treated buyout property.
# - NFIP benefits are adjusted for incomplete insurance take-up.
# - Uninsured/private damages and displacement welfare costs are intentionally excluded.

assumptions <- data.table(
  parameter = c(
    "property_acquisition_cost",
    "relocation_assistance",
    "demolition_site_restoration",
    "admin_transaction_cost_share",
    "annual_expected_nfip_claims",
    "annual_expected_disaster_assistance",
    "nfip_participation_rate",
    "amenity_open_space_annual",
    "deadweight_cost_public_funds"
  ),
  preferred = c(
    87160,  # observed median of positive acquisition costs, excluding zeros
    0,
    35000,
    0.15,
    16518, # annual expected NFIP claims if insured; verify this is annualized
    9623,  # annual expected FEMA IA / other public disaster assistance; verify annualized
    0.30,
    250,
    0.00
  ),
  low = c(
    7776,
    0,
    15000,
    0.10,
    1474,
    0,
    0.15,
    0,
    0.00
  ),
  high = c(
    325583,
    5000,
    80000,
    0.25,
    72699,
    18595,
    0.60,
    2000,
    0.30
  ),
  distribution = rep("beta_scaled", 9),
  notes = c(
    "Acquisition-only cost. Preferred value is observed median of positive acquisition costs, omitting zeros. Distribution is right-skewed. Prefer empirical resampling from observed positive acquisition costs if available.",
    "Relocation assistance if paid as a program expenditure. Set conservatively to zero in the baseline and vary in sensitivity analysis.",
    "Production costs: demolition, asbestos/environmental remediation, grading, and site restoration. Keep separate from administrative/transaction costs.",
    "Administrative and transaction costs: grant writing, planning, public engagement, appraisals, BCA preparation, environmental/historic review, reporting, intergovernmental coordination, legal/title/closing, and project management. Baseline 15%; sensitivity 10-25%.",
    "Annual expected NFIP claims per property conditional on being insured. If this is a claim/event average rather than an annual expected value, it must be annualized before use.",
    "Annual expected FEMA IA / other public disaster assistance per property. If this is conditional on disaster receipt rather than annual expected value, it must be annualized before use.",
    "Share of relevant properties expected to have NFIP coverage; scales expected NFIP claim benefits to account for incomplete insurance take-up.",
    "Optional open-space amenity co-benefit; include conservatively or as sensitivity only.",
    "Optional marginal cost of public funds. Baseline zero; sensitivity only."
  )
)

fwrite(assumptions, file.path(out_dir, "bca_assumptions.csv"))

# -----------------------------
# 3. Helper functions
# -----------------------------

# Present value of annual flow over horizon with real discount rate r.
pv_phased_benefits <- function(annual_benefit, r, horizon, realization_profile) {
  years <- seq_len(horizon)
  profile <- ifelse(
    years <= length(realization_profile),
    realization_profile[years],
    tail(realization_profile, 1)
  )
  discounted_benefits <- annual_benefit * profile / ((1 + r) ^ years)
  sum(discounted_benefits)
}

# Draw from approximate distributions using low/preferred/high.
# This keeps assumptions transparent and easy to explain.
draw_beta_scaled <- function(n, low, mode, high, shape = 8) {
  # Simple beta-PERT-like draw bounded by low/high.
  # Larger shape means tighter around preferred value.
  if (high <= low) return(rep(low, n))
  m <- (mode - low) / (high - low)
  m <- min(max(m, 0.001), 0.999)
  alpha <- 1 + shape * m
  beta  <- 1 + shape * (1 - m)
  low + (high - low) * rbeta(n, alpha, beta)
}

draw_parameter <- function(row, n) {
  dist <- row$distribution[[1]]
  low  <- row$low[[1]]
  pref <- row$preferred[[1]]
  high <- row$high[[1]]
  
  if (dist == "beta_scaled") {
    return(draw_beta_scaled(n, low, pref, high))
  }
  stop(paste("Unknown distribution:", dist))
}

make_draws <- function(assumptions, nsim) {
  draws <- data.table(sim = 1:nsim)
  for (p in assumptions$parameter) {
    row <- assumptions[parameter == p]
    draws[, (p) := draw_parameter(row, nsim)]
  }
  draws
}

# Add aggregate totals to a result table with per-property estimates.
add_aggregate_totals <- function(dt, n_properties) {
  out <- copy(dt)
  out[, `:=`(
    n_buyout_properties = n_properties,
    aggregate_direct_program_cost = direct_program_cost * n_properties,
    aggregate_admin_transaction_cost = admin_transaction_cost * n_properties,
    aggregate_total_cost = total_cost * n_properties,
    aggregate_pv_benefits = pv_benefits * n_properties,
    aggregate_net_benefit = net_benefit * n_properties
  )]
  out
}

# -----------------------------
# 4. Scenario definitions
# -----------------------------

bca_scenarios <- data.table(
  scenario = c(
    "Scenario 1: Full relocation",
    "Scenario 2: Estimated move effect",
    "Scenario 3: Estimated SFHA exit effect"
  ),
  scenario_short = c(
    "Full relocation",
    "Move effect",
    "SFHA exit effect"
  ),
  risk_reduction_effect = c(
    1.00,  # upper-bound benchmark
    0.77,  # based on estimated cumulative move effect among treated movers controling for flood group
    0.65   # based on estimated not-living-in-SFHA effect
  ),
  risk_reduction_effect_sd = c(
    0.03,
    0.0088,
    0.025
  )
)

benefit_realization_profile <- c(
  0.05, # year 1
  0.30, # year 2
  0.55, # year 3
  0.75, # year 4
  0.90, # year 5
  1.00, # year 6
  1.00, # year 7
  1.00, # year 8
  1.00, # year 9
  1.00  # year 10+
)

# -----------------------------
# 5. BCA engine
# -----------------------------

run_bca_once <- function(params,
                         risk_reduction_effect,
                         discount_rate = 0.03,
                         horizon = 30) {
  
  # One-time program costs per treated buyout property.
  direct_program_cost <- params$property_acquisition_cost +
    params$relocation_assistance +
    params$demolition_site_restoration
  
  admin_transaction_cost <- params$admin_transaction_cost_share * direct_program_cost
  
  resource_cost_before_mcpf <- direct_program_cost +
    admin_transaction_cost
  
  total_cost <- resource_cost_before_mcpf *
    (1 + params$deadweight_cost_public_funds)
  
  # Annual expected avoided public costs per treated buyout property.
  # NFIP savings are scaled by insurance participation because uninsured properties
  # do not generate NFIP claim payments.
  annual_avoided_nfip_claims <- risk_reduction_effect *
    params$nfip_participation_rate *
    params$annual_expected_nfip_claims
  
  annual_avoided_disaster_assistance <- risk_reduction_effect *
    params$annual_expected_disaster_assistance
  
  annual_public_avoided_cost <- annual_avoided_nfip_claims +
    annual_avoided_disaster_assistance
  
  annual_amenity <- params$amenity_open_space_annual
  
  annual_benefit <- annual_public_avoided_cost +
    annual_amenity
  
  pv_benefits <- pv_phased_benefits(
    annual_benefit = annual_benefit,
    r = discount_rate,
    horizon = horizon,
    realization_profile = benefit_realization_profile
  )
  
  net_benefit <- pv_benefits - total_cost
  benefit_cost_ratio <- pv_benefits / total_cost
  
  data.table(
    discount_rate = discount_rate,
    horizon = horizon,
    risk_reduction_effect = risk_reduction_effect,
    direct_program_cost = direct_program_cost,
    admin_transaction_cost = admin_transaction_cost,
    total_cost = total_cost,
    annual_avoided_nfip_claims = annual_avoided_nfip_claims,
    annual_avoided_disaster_assistance = annual_avoided_disaster_assistance,
    annual_public_avoided_cost = annual_public_avoided_cost,
    annual_amenity = annual_amenity,
    annual_benefit = annual_benefit,
    pv_benefits = pv_benefits,
    net_benefit = net_benefit,
    benefit_cost_ratio = benefit_cost_ratio
  )
}

preferred_params <- as.list(setNames(assumptions$preferred, assumptions$parameter))

# -----------------------------
# 6. Deterministic BCA table
# -----------------------------

deterministic_results <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  
  rbindlist(lapply(real_discount_rates, function(r) {
    rbindlist(lapply(horizons, function(h) {
      out <- run_bca_once(
        params = preferred_params,
        risk_reduction_effect = scen$risk_reduction_effect,
        discount_rate = r,
        horizon = h
      )
      out[, `:=`(
        scenario = scen$scenario,
        scenario_short = scen$scenario_short
      )]
      out
    }))
  }))
}))

deterministic_results <- add_aggregate_totals(
  deterministic_results,
  n_buyout_properties
)

fwrite(deterministic_results, file.path(out_dir, "deterministic_bca_results.csv"))

# -----------------------------
# 7. Monte Carlo BCA
# -----------------------------

mc_results <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  
  param_draws <- make_draws(assumptions, nsim)
  
  param_draws[, risk_reduction_effect := rnorm(
    .N,
    mean = scen$risk_reduction_effect,
    sd = scen$risk_reduction_effect_sd
  )]
  
  param_draws[, risk_reduction_effect := pmin(pmax(risk_reduction_effect, 0), 1)]
  
  param_draws[, discount_rate := draw_beta_scaled(.N, 0.01, preferred_discount_rate, 0.07)]
  param_draws[, horizon := sample(horizons, .N, replace = TRUE, prob = c(0.2, 0.3, 0.5))]
  
  out <- param_draws[, {
    params <- as.list(.SD)
    run_bca_once(
      params = params,
      risk_reduction_effect = risk_reduction_effect,
      discount_rate = discount_rate,
      horizon = horizon
    )
  }, by = sim, .SDcols = assumptions$parameter]
  
  out[, `:=`(
    scenario = scen$scenario,
    scenario_short = scen$scenario_short
  )]
  
  out
}))

mc_results <- add_aggregate_totals(
  mc_results,
  n_buyout_properties
)

fwrite(mc_results, file.path(out_dir, "monte_carlo_bca_results.csv"))

mc_summary <- mc_results[, .(
  n = .N,
  
  # Per-property values
  mean_pv_benefits = mean(pv_benefits, na.rm = TRUE),
  median_pv_benefits = median(pv_benefits, na.rm = TRUE),
  p05_pv_benefits = quantile(pv_benefits, 0.05, na.rm = TRUE),
  p95_pv_benefits = quantile(pv_benefits, 0.95, na.rm = TRUE),
  
  mean_total_cost = mean(total_cost, na.rm = TRUE),
  median_total_cost = median(total_cost, na.rm = TRUE),
  p05_total_cost = quantile(total_cost, 0.05, na.rm = TRUE),
  p95_total_cost = quantile(total_cost, 0.95, na.rm = TRUE),
  
  mean_net_benefit = mean(net_benefit, na.rm = TRUE),
  median_net_benefit = median(net_benefit, na.rm = TRUE),
  p05_net_benefit = quantile(net_benefit, 0.05, na.rm = TRUE),
  p95_net_benefit = quantile(net_benefit, 0.95, na.rm = TRUE),
  
  mean_bcr = mean(benefit_cost_ratio, na.rm = TRUE),
  median_bcr = median(benefit_cost_ratio, na.rm = TRUE),
  p05_bcr = quantile(benefit_cost_ratio, 0.05, na.rm = TRUE),
  p95_bcr = quantile(benefit_cost_ratio, 0.95, na.rm = TRUE),
  
  pr_net_benefit_positive = mean(net_benefit > 0, na.rm = TRUE),
  pr_bcr_above_one = mean(benefit_cost_ratio > 1, na.rm = TRUE),
  
  # Aggregate values across 44,000 buyouts
  n_buyout_properties = first(n_buyout_properties),
  mean_aggregate_pv_benefits = mean(aggregate_pv_benefits, na.rm = TRUE),
  median_aggregate_pv_benefits = median(aggregate_pv_benefits, na.rm = TRUE),
  p05_aggregate_pv_benefits = quantile(aggregate_pv_benefits, 0.05, na.rm = TRUE),
  p95_aggregate_pv_benefits = quantile(aggregate_pv_benefits, 0.95, na.rm = TRUE),
  
  mean_aggregate_total_cost = mean(aggregate_total_cost, na.rm = TRUE),
  median_aggregate_total_cost = median(aggregate_total_cost, na.rm = TRUE),
  p05_aggregate_total_cost = quantile(aggregate_total_cost, 0.05, na.rm = TRUE),
  p95_aggregate_total_cost = quantile(aggregate_total_cost, 0.95, na.rm = TRUE),
  
  mean_aggregate_net_benefit = mean(aggregate_net_benefit, na.rm = TRUE),
  median_aggregate_net_benefit = median(aggregate_net_benefit, na.rm = TRUE),
  p05_aggregate_net_benefit = quantile(aggregate_net_benefit, 0.05, na.rm = TRUE),
  p95_aggregate_net_benefit = quantile(aggregate_net_benefit, 0.95, na.rm = TRUE)
), by = .(scenario, scenario_short)]

fwrite(mc_summary, file.path(out_dir, "monte_carlo_bca_summary.csv"))

# -----------------------------
# 8. Sensitivity / tornado-style analysis by scenario
# -----------------------------

oat_sensitivity <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  
  rbindlist(lapply(assumptions$parameter, function(p) {
    low_params <- preferred_params
    high_params <- preferred_params
    
    low_params[[p]] <- assumptions[parameter == p, low]
    high_params[[p]] <- assumptions[parameter == p, high]
    
    low_res <- run_bca_once(
      params = low_params,
      risk_reduction_effect = scen$risk_reduction_effect,
      discount_rate = preferred_discount_rate,
      horizon = preferred_horizon
    )
    
    high_res <- run_bca_once(
      params = high_params,
      risk_reduction_effect = scen$risk_reduction_effect,
      discount_rate = preferred_discount_rate,
      horizon = preferred_horizon
    )
    
    low_res <- add_aggregate_totals(low_res, n_buyout_properties)
    high_res <- add_aggregate_totals(high_res, n_buyout_properties)
    
    data.table(
      scenario = scen$scenario,
      scenario_short = scen$scenario_short,
      parameter = p,
      low_net_benefit = low_res$net_benefit,
      high_net_benefit = high_res$net_benefit,
      low_aggregate_net_benefit = low_res$aggregate_net_benefit,
      high_aggregate_net_benefit = high_res$aggregate_net_benefit,
      low_bcr = low_res$benefit_cost_ratio,
      high_bcr = high_res$benefit_cost_ratio,
      range_net_benefit = abs(high_res$net_benefit - low_res$net_benefit),
      range_aggregate_net_benefit = abs(high_res$aggregate_net_benefit - low_res$aggregate_net_benefit)
    )
  }))
}))

oat_sensitivity <- oat_sensitivity[
  order(scenario_short, -range_net_benefit)
]

fwrite(oat_sensitivity, file.path(out_dir, "one_at_a_time_sensitivity_by_scenario.csv"))

# -----------------------------
# 9. Paper/SI summary tables
# -----------------------------

# Scenario-specific paper-ready summary.
paper_table <- mc_summary[, .(
  scenario,
  scenario_short,
  n_buyout_properties,
  preferred_discount_rate = preferred_discount_rate,
  preferred_horizon = preferred_horizon,
  median_pv_benefits_per_property = median_pv_benefits,
  median_total_cost_per_property = median_total_cost,
  median_net_benefit_per_property = median_net_benefit,
  median_bcr = median_bcr,
  pr_net_benefit_positive = pr_net_benefit_positive,
  pr_bcr_above_one = pr_bcr_above_one,
  median_aggregate_pv_benefits = median_aggregate_pv_benefits,
  median_aggregate_total_cost = median_aggregate_total_cost,
  median_aggregate_net_benefit = median_aggregate_net_benefit
)]

fwrite(paper_table, file.path(out_dir, "paper_ready_bca_summary.csv"))

print(mc_summary)
print(paper_table)

# -----------------------------
# 10. Figures
# -----------------------------

# Tornado plot: per-property net benefits, base R style.
plot_sens <- copy(oat_sensitivity)

param_order <- plot_sens[, .(
  avg_range = mean(range_net_benefit, na.rm = TRUE)
), by = parameter][order(-avg_range), parameter]

plot_sens[, parameter_label := parameter]
plot_sens[, parameter_label := gsub("_", " ", parameter_label)]
plot_sens[, parameter_label := tools::toTitleCase(parameter_label)]
plot_sens[, parameter_label := factor(parameter_label, levels = tools::toTitleCase(gsub("_", " ", param_order)))]
plot_sens[, min_nb := pmin(low_net_benefit, high_net_benefit)]
plot_sens[, max_nb := pmax(low_net_benefit, high_net_benefit)]

preferred_lines <- deterministic_results[
  discount_rate == preferred_discount_rate &
    horizon == preferred_horizon,
  .(scenario_short, preferred_net_benefit = net_benefit)
]

plot_sens <- merge(
  plot_sens,
  preferred_lines,
  by = "scenario_short",
  all.x = TRUE
)

scenario_cols <- c(
  "Full relocation" = "#0072B2",
  "Move effect" = "forestgreen",
  "SFHA exit effect" = "goldenrod2"
)

pdf(
  file = file.path(fig_dir, "bca_tornado_net_benefits_by_scenario.pdf"),
  width = 11,
  height = 7,
  useDingbats = FALSE
)

par(
  mfrow = c(1, 3),
  mar = c(5, 9, 3, 1),
  las = 1,
  bty = "n"
)

for (s in c("Full relocation", "Move effect", "SFHA exit effect")) {
  dt_s <- plot_sens[scenario_short == s]
  col_s <- scenario_cols[s]
  dt_s <- dt_s[order(parameter_label)]
  dt_s[, y := seq_len(.N)]
  
  xlim_s <- range(c(dt_s$min_nb, dt_s$max_nb, dt_s$preferred_net_benefit, 0), na.rm = TRUE)
  xpad <- diff(xlim_s) * 0.08
  xlim_s <- c(xlim_s[1] - xpad, xlim_s[2] + xpad)
  
  plot(
    NA,
    xlim = xlim_s,
    ylim = c(0.5, nrow(dt_s) + 0.5),
    yaxt = "n",
    xlab = "Net benefit per property",
    ylab = "",
    main = s,
    cex.lab = 1.05,
    cex.main = 1.05,
    cex.axis = 0.9,
    xaxt = "n"
  )
  
  axis(
    side = 1,
    at = pretty(xlim_s),
    labels = format(pretty(xlim_s), big.mark = ",", scientific = FALSE, trim = TRUE),
    cex.axis = 0.85
  )
  
  axis(
    side = 2,
    at = dt_s$y,
    labels = as.character(dt_s$parameter_label),
    tick = FALSE,
    cex.axis = 0.8
  )
  
  abline(v = 0, lty = 3, lwd = 1)
  abline(v = unique(dt_s$preferred_net_benefit), lty = 2, lwd = 1.5)
  
  for (i in seq_len(nrow(dt_s))) {
    row <- dt_s[i]
    segments(
      x0 = row$min_nb,
      y0 = row$y,
      x1 = row$max_nb,
      y1 = row$y,
      lwd = 3,
      col = col_s
    )
    points(row$min_nb, row$y, pch = 16, cex = 0.7, col = col_s)
    points(row$max_nb, row$y, pch = 16, cex = 0.7, col = col_s)
  }
}

dev.off()

# Discount-rate and horizon sensitivity, base R style.
det_plot_dt <- copy(deterministic_results)
det_plot_dt[, horizon := factor(horizon, levels = horizons)]

pdf(
  file = file.path(fig_dir, "bca_discount_horizon_sensitivity.pdf"),
  width = 11,
  height = 4.5,
  useDingbats = FALSE
)

par(
  mfrow = c(1, 3),
  mar = c(5, 5, 3, 1),
  las = 1,
  bty = "n"
)

line_types <- c("10" = 1, "20" = 2, "30" = 3)
point_types <- c("10" = 16, "20" = 17, "30" = 15)

for (s in c("Full relocation", "Move effect", "SFHA exit effect")) {
  dt_s <- det_plot_dt[scenario_short == s]
  col_s <- scenario_cols[s]
  
  ylim_s <- range(c(dt_s$benefit_cost_ratio, 1), na.rm = TRUE)
  ypad <- diff(ylim_s) * 0.12
  ylim_s <- c(max(0, ylim_s[1] - ypad), ylim_s[2] + ypad)
  
  plot(
    NA,
    xlim = range(real_discount_rates),
    ylim = ylim_s,
    xlab = "Real discount rate",
    ylab = "Benefit-cost ratio",
    main = s,
    cex.lab = 1.05,
    cex.main = 1.05,
    cex.axis = 0.95,
    xaxt = "n"
  )
  
  axis(
    side = 1,
    at = real_discount_rates,
    labels = paste0(real_discount_rates * 100, "%"),
    cex.axis = 0.9
  )
  
  abline(h = 1, lty = 2, lwd = 1.5)
  
  for (h in as.character(horizons)) {
    dt_h <- dt_s[as.character(horizon) == h][order(discount_rate)]
    lines(
      dt_h$discount_rate,
      dt_h$benefit_cost_ratio,
      lty = line_types[h],
      lwd = 2,
      col = col_s
    )
    points(
      dt_h$discount_rate,
      dt_h$benefit_cost_ratio,
      pch = point_types[h],
      cex = 1,
      col = col_s
    )
  }
  
  legend(
    "topright",
    legend = paste0(horizons, " years"),
    lty = line_types[as.character(horizons)],
    pch = point_types[as.character(horizons)],
    lwd = 2,
    bty = "n",
    cex = 0.85,
    title = "Horizon"
  )
}

dev.off()

# -------------------------------------------------
# Benefit-cost ratio uncertainty plot by scenario
# -------------------------------------------------

bcr_plot_dt <- mc_results[, .(
  p05 = quantile(benefit_cost_ratio, 0.05, na.rm = TRUE),
  p25 = quantile(benefit_cost_ratio, 0.25, na.rm = TRUE),
  median = median(benefit_cost_ratio, na.rm = TRUE),
  p75 = quantile(benefit_cost_ratio, 0.75, na.rm = TRUE),
  p95 = quantile(benefit_cost_ratio, 0.95, na.rm = TRUE)
), by = scenario_short]

bcr_plot_dt[, scenario_short := factor(
  scenario_short,
  levels = c(
    "Full relocation",
    "Move effect",
    "SFHA exit effect"
  )
)]

bcr_plot_dt <- bcr_plot_dt[order(scenario_short)]

scenario_cols <- c(
  "Full relocation" = "#0072B2",
  "Move effect" = "forestgreen",
  "SFHA exit effect" = "goldenrod2"
)

pdf(
  file = file.path(fig_dir, "bca_ratio_uncertainty_plot.pdf"),
  width = 7,
  height = 4.5,
  useDingbats = FALSE
)

par(mar = c(5, 7, 2, 1), las = 1, bty = "n")

plot(
  NA,
  xlim = range(c(bcr_plot_dt$p05, bcr_plot_dt$p95, 1), na.rm = TRUE),
  ylim = c(0.5, nrow(bcr_plot_dt) + 0.5),
  yaxt = "n",
  ylab = "",
  xlab = "Benefit-cost ratio",
  main = "",
  cex.lab = 1.2,
  cex.axis = 1.1
)

abline(v = 1, lty = 2, lwd = 1.5)

axis(
  side = 2,
  at = seq_len(nrow(bcr_plot_dt)),
  labels = rev(as.character(bcr_plot_dt$scenario_short)),
  tick = FALSE,
  cex.axis = 1.1
)

plot_dt <- copy(bcr_plot_dt)
plot_dt[, y := rev(seq_len(.N))]

for (i in seq_len(nrow(plot_dt))) {
  row <- plot_dt[i]
  col_i <- scenario_cols[as.character(row$scenario_short)]
  
  segments(row$p05, row$y, row$p95, row$y, lwd = 2, col = col_i)
  rect(
    xleft = row$p25,
    ybottom = row$y - 0.18,
    xright = row$p75,
    ytop = row$y + 0.18,
    col = adjustcolor(col_i, alpha.f = 0.35),
    border = col_i,
    lwd = 2
  )
  points(row$median, row$y, pch = 16, cex = 1.3, col = col_i)
}

mtext(
  "Monte Carlo uncertainty intervals",
  side = 3,
  line = 0.2,
  adj = 0,
  cex = 1
)

dev.off()

# -------------------------------------------------
# SI Figure: Monte Carlo distributions
# Panel A: Net benefits per property
# Panel B: Benefit-cost ratios
# -------------------------------------------------

net_xlim <- quantile(mc_results$net_benefit, c(0.01, 0.99), na.rm = TRUE)

dens_net <- lapply(names(scenario_cols), function(s) {
  density(
    mc_results[
      scenario_short == s &
        net_benefit >= net_xlim[1] &
        net_benefit <= net_xlim[2],
      net_benefit
    ],
    na.rm = TRUE
  )
})

names(dens_net) <- names(scenario_cols)
ymax_net <- max(sapply(dens_net, function(d) max(d$y, na.rm = TRUE)))

bcr_xlim <- quantile(mc_results$benefit_cost_ratio, c(0.01, 0.99), na.rm = TRUE)

dens_bcr <- lapply(names(scenario_cols), function(s) {
  density(
    mc_results[
      scenario_short == s &
        benefit_cost_ratio >= bcr_xlim[1] &
        benefit_cost_ratio <= bcr_xlim[2],
      benefit_cost_ratio
    ],
    na.rm = TRUE
  )
})

names(dens_bcr) <- names(scenario_cols)
ymax_bcr <- max(sapply(dens_bcr, function(d) max(d$y, na.rm = TRUE)))

pdf(
  file = file.path(fig_dir, "si_monte_carlo_distributions.pdf"),
  width = 8,
  height = 9,
  useDingbats = FALSE
)

par(mfrow = c(2, 1), mar = c(5, 5, 2, 1), las = 1, bty = "n")

# Panel A: Net benefits
plot(
  NA,
  xlim = net_xlim,
  ylim = c(0, ymax_net * 1.12),
  xlab = "Net benefit per treated property/household",
  ylab = "Density",
  main = "A. Monte Carlo distribution of net benefits",
  cex.lab = 1.2,
  cex.main = 1.1,
  cex.axis = 1,
  xaxt = "n"
)

axis(
  side = 1,
  at = pretty(net_xlim),
  labels = format(pretty(net_xlim), big.mark = ",", scientific = FALSE)
)

abline(v = 0, lty = 2, lwd = 1.5)

for (s in names(scenario_cols)) {
  d <- dens_net[[s]]
  polygon(
    c(d$x, rev(d$x)),
    c(d$y, rep(0, length(d$y))),
    col = adjustcolor(scenario_cols[s], alpha.f = 0.20),
    border = NA
  )
  lines(d$x, d$y, col = scenario_cols[s], lwd = 2.5)
}

legend(
  "topright",
  legend = names(scenario_cols),
  col = scenario_cols,
  lwd = 2.5,
  bty = "n",
  cex = 0.95
)

# Panel B: Benefit-cost ratios
plot(
  NA,
  xlim = bcr_xlim,
  ylim = c(0, ymax_bcr * 1.12),
  xlab = "Benefit-cost ratio",
  ylab = "Density",
  main = "B. Monte Carlo distribution of benefit-cost ratios",
  cex.lab = 1.2,
  cex.main = 1.1,
  cex.axis = 1,
  xaxt = "n"
)

axis(
  side = 1,
  at = pretty(bcr_xlim),
  labels = format(pretty(bcr_xlim), scientific = FALSE, trim = TRUE)
)

abline(v = 1, lty = 2, lwd = 1.5)

for (s in names(scenario_cols)) {
  d <- dens_bcr[[s]]
  polygon(
    c(d$x, rev(d$x)),
    c(d$y, rep(0, length(d$y))),
    col = adjustcolor(scenario_cols[s], alpha.f = 0.20),
    border = NA
  )
  lines(d$x, d$y, col = scenario_cols[s], lwd = 2.5)
}

legend(
  "topright",
  legend = names(scenario_cols),
  col = scenario_cols,
  lwd = 2.5,
  bty = "n",
  cex = 0.95
)

dev.off()
