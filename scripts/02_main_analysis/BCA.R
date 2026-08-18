############################################################
# BENEFIT-COST ANALYSIS (BCA) -- official pipeline
#
# AnnualBenefit_t = [RiskReductionEffect_s x Recurrence x ConditionalBuildingContentsLoss
#                    + RiskReductionEffect_s x Recurrence x ConditionalDisasterAssistance (IA)
#                    + AmenityOpenSpace] x RealizationProfile_t
#
# Benefits combine three components, all defined per treated property:
#  1. Avoided flood-related building and contents damage: a recurrence-scaled conditional
#     severity estimate (D-bar) derived from paid NFIP claims (N=494,010, buyout-ZIP/A-V-
#     zone matched -- see ../bca_revisions/conditional_severity_reproduction.json for
#     provenance). NFIP participation rate is NOT applied, since D-bar is already an
#     unconditional-on-programmatic-payout severity estimate.
#  2. Avoided FEMA Individual and Households Program (IHP) disaster assistance, scaled by
#     BOTH recurrence and the risk-reduction effect -- IA only accrues in years a damaging
#     flood actually occurs. Preferred value $4,811 (OpenFEMA, IHP-only) -- see note at the
#     benefit_assumptions table below.
#  3. A fixed open-space amenity co-benefit, not scaled by recurrence or the risk-reduction
#     effect, since it accrues regardless of whether a flood occurs.
# The conditional building-and-contents loss (D-bar) is Monte Carlo-varied, drawn from the
# same empirical NFIP-claims distribution its preferred value comes from (p25/p75 of
# N=494,010 paid claims), not held fixed -- unlike recurrence, which IS held fixed as a
# named-benchmark sensitivity dimension (see below), D-bar has substantial empirical
# variance (sd=$82,351) with no reason to treat it as a scenario choice rather than an
# uncertain quantity.
#
# Parameters are split into cost_assumptions (true one-time program costs: acquisition,
# relocation, demolition/site restoration, admin/transaction share, deadweight cost of
# public funds) and benefit_assumptions (IA, open-space amenity, and the conditional
# building-and-contents loss) -- the latter three are benefit-side inputs, not costs.
#
# Recurrence is a SENSITIVITY DIMENSION (3 named benchmarks -- lower/repetitive-loss/upper),
# not a sampled distribution -- held fixed within each Monte Carlo run below.
#
# Reads ../bca_revisions/recurrence_benchmarks.json for the flood-hazard-side parameters
# (D-bar preferred value, recurrence benchmarks); cost/IA/amenity preferred-low-high
# assumptions are hardcoded in the benefit_assumptions/cost_assumptions tables below.
#
# History: superseded an earlier NFIP-claims x participation-rate specification (archived
# at ../scripts/archive/BCA_original_NFIP_based_pre2026-08-18.R) and an intermediate
# recurrence-only, no-IA/amenity parallel spec (archived at
# ../bca_revisions/archive/recurrence_bca.R) during methodology revisions in August 2026.
############################################################
library(data.table)

ROOT <- "/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram/2026/bca_revisions"
OUT <- file.path(ROOT, "full_bca_recurrence_outputs")
FIG <- file.path(ROOT, "figures_full_recurrence")
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)
if (!dir.exists(FIG)) dir.create(FIG, recursive = TRUE)

rb <- jsonlite::fromJSON(file.path(ROOT, "recurrence_benchmarks.json"))
cond_loss <- rb$conditional_bc_loss  # $45,917
p_lower <- rb$lower_recurrence_nfip_zone_weighted_EXACT   # 0.0153
p_rl    <- rb$repetitive_loss_benchmark                    # 0.20
p_upper <- rb$upper_recurrence_county_disaster_declaration  # 0.2328

cat(sprintf("Conditional B+C loss: $%.0f\n", cond_loss))
cat(sprintf("Recurrence benchmarks: lower=%.4f, RL=%.4f, upper=%.4f\n", p_lower, p_rl, p_upper))

n_buyout_properties <- 44000
nsim <- 50000
real_discount_rates <- c(0.01, 0.02, 0.03, 0.05, 0.07)
preferred_discount_rate <- 0.03
horizons <- c(10, 20, 30)
preferred_horizon <- 30

# True one-time program costs.
cost_assumptions <- data.table(
  parameter = c("property_acquisition_cost","relocation_assistance","demolition_site_restoration",
                "admin_transaction_cost_share","deadweight_cost_public_funds"),
  preferred = c(87160, 0, 35000, 0.15, 0.00),
  low       = c(7776,  0, 15000, 0.10, 0.00),
  high      = c(325583,5000,80000,0.25,0.30)
)

# Benefit-side assumptions (annual_expected_nfip_claims and nfip_participation_rate omitted
# -- replaced by the recurrence x conditional_bc_loss term).
#
# IA: annual_expected_disaster_assistance preferred/low/high are the mean/min/max of 21
# annual per-property IHP-only payout values from OpenFEMA (user-supplied, 2026-08-17) --
# mean $4,811.41, range $2,130.75-$9,298.51 -- resolving both the earlier scope flag
# (IHP-only, not IHP+HA+ONA) and the low/high bounds (previously a proportional guess off
# the old $9,623/$18,595 figures inherited from ../scripts/BCA.R; the low bound in
# particular is no longer $0, since no year in this 21-value panel is anywhere near zero).
# Preferred/low/high are TREATED BELOW as conditional on a damaging flood occurring
# (recurrence is applied to this term in run_bcaR, same as conditional_bc_loss), consistent
# with each value being a per-year OpenFEMA figure rather than an already-frequency-
# weighted one.
#
# conditional_bc_loss: preferred = $45,917 (mean of N=494,010 paid NFIP claims, buyout-ZIP/
# A-V-zone matched -- see conditional_severity_reproduction.json). low/high = the empirical
# p25/p75 of that same claims distribution ($4,910.99/$55,628.98, from
# conditional_severity_reproduction.json), user-selected 2026-08-18 over the alternative of
# reusing the +/-25% bound from the one-at-a-time sensitivity analysis. Now drawn per
# Monte Carlo simulation via draw_cost_parameter(), same as every other benefit_assumptions
# row -- previously held fixed at $45,917 in every simulation.
benefit_assumptions <- data.table(
  parameter = c("annual_expected_disaster_assistance","amenity_open_space_annual","conditional_bc_loss"),
  preferred = c(4811, 250, cond_loss),
  low       = c(2131, 0,   4911),
  high      = c(9299, 2000,55629)
)

all_assumptions <- rbindlist(list(cost_assumptions, benefit_assumptions))
realization_profile <- c(0.05,0.30,0.55,0.75,0.90,1.00,1.00,1.00,1.00,1.00)
pv_phased_benefits <- function(annual_benefit, r, horizon, profile) {
  years <- seq_len(horizon)
  p <- ifelse(years <= length(profile), profile[years], tail(profile,1))
  sum(annual_benefit * p / (1+r)^years)
}
draw_beta_scaled <- function(n, low, mode, high, shape = 8) {
  if (high <= low) return(rep(low, n))
  m <- min(max((mode - low) / (high - low), 0.001), 0.999)
  alpha <- 1 + shape * m; beta <- 1 + shape * (1 - m)
  low + (high - low) * rbeta(n, alpha, beta)
}
draw_cost_parameter <- function(row, n) draw_beta_scaled(n, row$low[[1]], row$preferred[[1]], row$high[[1]])

bca_scenarios <- data.table(
  scenario_short = c("Full relocation","Move effect","SFHA exit effect"),
  risk_reduction_effect = c(1.00, 0.77, 0.65),
  risk_reduction_effect_sd = c(0.03, 0.0088, 0.025)
)

run_bcaR <- function(cost_params, rre, recurrence, r=0.03, h=30) {
  direct_cost <- cost_params$property_acquisition_cost + cost_params$relocation_assistance + cost_params$demolition_site_restoration
  admin_cost <- cost_params$admin_transaction_cost_share * direct_cost
  total_cost <- (direct_cost + admin_cost) * (1 + cost_params$deadweight_cost_public_funds)

  annual_expected_damage <- recurrence * cost_params$conditional_bc_loss
  annual_avoided_flood_damage <- rre * annual_expected_damage          # replaces NFIP term
  # IA is conditional on a damaging flood occurring, exactly like the flood-damage term --
  # apply recurrence here too, not just RRE, or benefits are overstated (IA would accrue
  # every year regardless of whether a flood happened that year).
  annual_avoided_disaster_assistance <- rre * recurrence * cost_params$annual_expected_disaster_assistance
  annual_amenity <- cost_params$amenity_open_space_annual              # unchanged, not RRE-scaled, not recurrence-scaled

  annual_benefit <- annual_avoided_flood_damage + annual_avoided_disaster_assistance + annual_amenity
  pv <- pv_phased_benefits(annual_benefit, r, h, realization_profile)

  data.table(discount_rate=r, horizon=h, risk_reduction_effect=rre, recurrence=recurrence,
             cond_loss=cost_params$conditional_bc_loss,
             annual_expected_damage=annual_expected_damage,
             annual_avoided_flood_damage=annual_avoided_flood_damage,
             annual_avoided_disaster_assistance=annual_avoided_disaster_assistance,
             annual_amenity=annual_amenity,
             annual_benefit=annual_benefit,
             total_cost=total_cost, pv_benefits=pv, net_benefit=pv-total_cost, benefit_cost_ratio=pv/total_cost)
}
preferred_cost_params <- as.list(setNames(all_assumptions$preferred, all_assumptions$parameter))

############################################################
# 1. Deterministic across full recurrence grid x scenarios x horizons (r=3% for grid;
#    full discount-rate grid also computed at preferred recurrence points for the
#    discount/horizon sensitivity figure)
############################################################
# Extended beyond p_upper so the diagnostic figure can show the SFHA-exit break-even point
# (23.9%, from breakeven_recurrence.csv), which exceeds the upper recurrence benchmark.
recurrence_grid_max <- 0.26
recurrence_grid <- sort(unique(c(seq(p_lower, recurrence_grid_max, length.out = 30), p_lower, p_rl, p_upper)))

det_grid <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  rbindlist(lapply(recurrence_grid, function(p) {
    out <- run_bcaR(preferred_cost_params, scen$risk_reduction_effect, p, preferred_discount_rate, preferred_horizon)
    out[, scenario_short := scen$scenario_short]
    out
  }))
}))
fwrite(det_grid, file.path(OUT, "deterministic_recurrence_grid.csv"))

# break-even recurrence per scenario (closed form).
# IA now scales with recurrence too (see run_bcaR), so it combines additively with
# cond_loss inside the recurrence-scaled bracket:
# total_cost = PVfactor(r,h) x [rre*p*(cond_loss+IA) + amenity]
# => p* = (total_cost/PVfactor - amenity) / (rre*(cond_loss+IA))
pv_factor_at <- function(r, h) {
  years <- seq_len(h)
  p <- ifelse(years <= length(realization_profile), realization_profile[years], tail(realization_profile,1))
  sum(p/(1+r)^years)
}
pvf <- pv_factor_at(preferred_discount_rate, preferred_horizon)
base_cost <- (87160+35000)*1.15
ia_pref <- preferred_cost_params$annual_expected_disaster_assistance
amenity_pref <- preferred_cost_params$amenity_open_space_annual
cond_loss_pref <- preferred_cost_params$conditional_bc_loss
breakeven <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  rre <- scen$risk_reduction_effect
  pstar <- (base_cost/pvf - amenity_pref) / (rre * (cond_loss_pref + ia_pref))
  data.table(scenario_short=scen$scenario_short, risk_reduction_effect=rre, breakeven_recurrence=pstar)
}))
fwrite(breakeven, file.path(OUT, "breakeven_recurrence.csv"))
cat("\n=== BREAK-EVEN RECURRENCE (p* such that BCR=1), h=30, r=3%, IA+amenity included ===\n")
print(breakeven)

# discount/horizon sensitivity at the three named benchmarks
det_benchmarks_full <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  rbindlist(lapply(c(p_lower, p_rl, p_upper), function(p) {
    rbindlist(lapply(real_discount_rates, function(r) {
      rbindlist(lapply(horizons, function(h) {
        out <- run_bcaR(preferred_cost_params, scen$risk_reduction_effect, p, r, h)
        out[, scenario_short := scen$scenario_short]
        out
      }))
    }))
  }))
}))
fwrite(det_benchmarks_full, file.path(OUT, "deterministic_benchmarks_full_grid.csv"))

cat("\n=== DETERMINISTIC at 3 named benchmarks (h=30, r=3%) ===\n")
print(det_benchmarks_full[horizon==30 & discount_rate==0.03, .(scenario_short, recurrence=round(recurrence,4), annual_benefit=round(annual_benefit), net_benefit=round(net_benefit), bcr=round(benefit_cost_ratio,3))])

############################################################
# 2. Monte Carlo WITHIN each of the 3 named recurrence scenarios (recurrence held fixed
#    per scenario -- a sensitivity dimension, not sampled; cost + IA + amenity +
#    conditional_bc_loss + treatment-effect uncertainty propagated)
############################################################
set.seed(12345)
param_names <- all_assumptions$parameter
run_mc_at_recurrence <- function(p, label) {
  rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
    scen <- bca_scenarios[s]
    draws <- data.table(sim = 1:nsim)
    for (cp in param_names) draws[, (cp) := draw_cost_parameter(all_assumptions[parameter == cp], nsim)]
    draws[, risk_reduction_effect := rnorm(.N, mean = scen$risk_reduction_effect, sd = scen$risk_reduction_effect_sd)]
    draws[, risk_reduction_effect := pmin(pmax(risk_reduction_effect, 0), 1)]
    draws[, discount_rate := draw_beta_scaled(.N, 0.01, preferred_discount_rate, 0.07)]
    draws[, horizon := sample(horizons, .N, replace = TRUE, prob = c(0.2, 0.3, 0.5))]
    out <- draws[, {
      cost_params <- as.list(.SD[, ..param_names])
      run_bcaR(cost_params, risk_reduction_effect, p, discount_rate, horizon)
    }, by = sim, .SDcols = c(param_names, "risk_reduction_effect","discount_rate","horizon")]
    out[, `:=`(scenario_short = scen$scenario_short, recurrence_label = label)]
    out
  }))
}
mc_lower <- run_mc_at_recurrence(p_lower, "Lower (1.53%)")
mc_rl    <- run_mc_at_recurrence(p_rl, "Repetitive-loss (20%)")
mc_upper <- run_mc_at_recurrence(p_upper, "Upper (23.28%)")
mc_all <- rbindlist(list(mc_lower, mc_rl, mc_upper))
fwrite(mc_all, file.path(OUT, "monte_carlo_by_recurrence_benchmark.csv"))

mc_summary <- mc_all[, .(
  median_net_benefit=median(net_benefit), p05_net_benefit=quantile(net_benefit,0.05), p95_net_benefit=quantile(net_benefit,0.95),
  median_bcr=median(benefit_cost_ratio), p05_bcr=quantile(benefit_cost_ratio,0.05), p95_bcr=quantile(benefit_cost_ratio,0.95),
  pr_bcr_above_one=mean(benefit_cost_ratio>1)
), by=.(recurrence_label, scenario_short)]
fwrite(mc_summary, file.path(OUT, "monte_carlo_summary_by_recurrence_benchmark.csv"))
cat("\n=== MC SUMMARY by recurrence benchmark x scenario ===\n")
print(mc_summary)

cat("\nDONE full recurrence-based BCA (with IA, amenity, deadweight cost).\n")
