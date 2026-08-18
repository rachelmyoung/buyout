############################################################
# BCA FIGURES -- official pipeline. Reads outputs written by BCA.R (run that first).
# Produces Figure 4B analogues, SI Monte Carlo distributions (2x3 grid + single-benchmark
# RL20 version), SI one-at-a-time tornado, discount/horizon sensitivity, and the
# BCR-vs-recurrence diagnostic. See BCA.R's header for methodology/history notes.
############################################################
library(data.table)
ROOT <- "/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram/2026/bca_revisions"
OUT <- file.path(ROOT, "full_bca_recurrence_outputs")
FIG <- file.path(ROOT, "figures_full_recurrence")

rb <- jsonlite::fromJSON(file.path(ROOT, "recurrence_benchmarks.json"))
p_lower <- rb$lower_recurrence_nfip_zone_weighted_EXACT
p_rl <- rb$repetitive_loss_benchmark
p_upper <- rb$upper_recurrence_county_disaster_declaration
cond_loss <- rb$conditional_bc_loss

det_grid <- fread(file.path(OUT, "deterministic_recurrence_grid.csv"))
mc_all <- fread(file.path(OUT, "monte_carlo_by_recurrence_benchmark.csv"))
breakeven <- fread(file.path(OUT, "breakeven_recurrence.csv"))
scenario_cols <- c("Full relocation" = "#0072B2", "Move effect" = "forestgreen", "SFHA exit effect" = "goldenrod2")
scenario_order <- c("Full relocation","Move effect","SFHA exit effect")
benchmark_to_recurrence <- c("Lower (1.53%)" = p_lower, "Repetitive-loss (20%)" = p_rl, "Upper (23.28%)" = p_upper)

############################################################
# FIGURE 4B PARALLEL: MC BCR uncertainty (box/whisker) at each of the 3 named
# recurrence benchmarks -- mirrors bca_ratio_uncertainty_plot.pdf / Fig 4B style.
############################################################
for (bm in c("Lower (1.53%)","Repetitive-loss (20%)","Upper (23.28%)")) {
  bm_file <- gsub("[^A-Za-z0-9]","_", bm)
  d <- mc_all[recurrence_label==bm]
  bcr_summary <- d[, .(p05=quantile(benefit_cost_ratio,0.05), p25=quantile(benefit_cost_ratio,0.25),
                        median=median(benefit_cost_ratio), p75=quantile(benefit_cost_ratio,0.75),
                        p95=quantile(benefit_cost_ratio,0.95)), by=scenario_short]

  # deterministic point estimate (preferred costs/IA/amenity, h=30, r=3%) at this benchmark's
  # exact recurrence value, for direct comparison against the MC distribution
  det_bm <- det_grid[abs(recurrence - benchmark_to_recurrence[[bm]]) < 1e-9, .(scenario_short, deterministic_bcr = benefit_cost_ratio)]
  bcr_summary <- merge(bcr_summary, det_bm, by = "scenario_short")

  bcr_summary[, scenario_short := factor(scenario_short, levels=scenario_order)]
  bcr_summary <- bcr_summary[order(scenario_short)]

  xlim_raw_ind <- range(c(bcr_summary$p05,bcr_summary$p95,bcr_summary$deterministic_bcr,1))
  xpad_ind <- diff(xlim_raw_ind) * 0.12
  xlim_ind <- c(xlim_raw_ind[1]-xpad_ind, xlim_raw_ind[2]+xpad_ind)

  pdf(file.path(FIG, paste0("fullBCA_recurrence_Figure4B_", bm_file, ".pdf")), width=6.4, height=4.1, useDingbats=FALSE)
  par(mar=c(5,9,3,1), las=1, bty="n")
  plot(NA, xlim=xlim_ind, ylim=c(0.5,4.05), yaxt="n",
       xlab="Benefit-cost ratio", ylab="", main=paste0("Full BCA (+IA, +amenity) -- recurrence = ", bm), cex.main=0.85)
  abline(v=1, lty=2, lwd=1.5)
  axis(2, at=1:3, labels=rev(scenario_order), tick=FALSE)
  plot_dt <- copy(bcr_summary); plot_dt[, y := rev(seq_len(.N))]
  for (i in seq_len(nrow(plot_dt))) {
    row <- plot_dt[i]; col_i <- scenario_cols[as.character(row$scenario_short)]
    segments(row$p05, row$y, row$p95, row$y, lwd=2, col=col_i)
    rect(row$p25, row$y-0.18, row$p75, row$y+0.18, col=adjustcolor(col_i,alpha.f=0.35), border=col_i, lwd=2)
    points(row$median, row$y, pch=16, cex=1.3, col=col_i)
    text(row$median, row$y+0.32, labels=sprintf("%.2f", row$median), cex=0.78, col=col_i, font=2)
    text(row$p05, row$y-0.24, labels=sprintf("%.2f", row$p05), cex=0.62, col=col_i, pos=2)
    text(row$p95, row$y-0.24, labels=sprintf("%.2f", row$p95), cex=0.62, col=col_i, pos=4)
  }
  # deterministic markers drawn in a second pass, after all boxes/whiskers, so they are
  # never occluded; black outline + white fill for maximum contrast against any scenario color
  for (i in seq_len(nrow(plot_dt))) {
    row <- plot_dt[i]
    points(row$deterministic_bcr, row$y, pch=23, cex=1.7, bg="white", col="black", lwd=2.5)
    text(row$deterministic_bcr, row$y+0.50, labels=sprintf("%.2f", row$deterministic_bcr), cex=0.68, col="black", font=2)
  }
  legend("bottomright", legend=c("Monte Carlo median","Deterministic (preferred)"),
         pch=c(16,23), pt.bg=c(NA,"white"), col=c("grey30","black"), pt.cex=c(1.1,1.4), bty="n", cex=0.7)
  dev.off()
}
cat("saved 3x fullBCA_recurrence_Figure4B_*.pdf\n")

############################################################
# FIGURE 4B COMBINED: all 3 recurrence benchmarks together in one plot
############################################################
recurrence_bar_order <- c("Lower (1.53%)","Repetitive-loss (20%)","Upper (23.28%)")
bcr_summary_all <- mc_all[, .(p05=quantile(benefit_cost_ratio,0.05), p25=quantile(benefit_cost_ratio,0.25),
                               median=median(benefit_cost_ratio), p75=quantile(benefit_cost_ratio,0.75),
                               p95=quantile(benefit_cost_ratio,0.95)), by=.(scenario_short, recurrence_label)]

block_order <- rev(scenario_order)
y_lookup <- rbindlist(lapply(seq_along(block_order), function(b) {
  base_y <- (b - 1) * 4 + 1
  data.table(scenario_short = block_order[b], recurrence_label = recurrence_bar_order,
             y = base_y + seq_along(recurrence_bar_order) - 1)
}))
bcr_summary_all <- merge(bcr_summary_all, y_lookup, by = c("scenario_short","recurrence_label"))

# deterministic point estimate at each (scenario, benchmark) cell, same source as the
# individual-panel figures above
det_by_benchmark <- rbindlist(lapply(names(benchmark_to_recurrence), function(bm) {
  det_grid[abs(recurrence - benchmark_to_recurrence[[bm]]) < 1e-9,
           .(scenario_short, recurrence_label = bm, deterministic_bcr = benefit_cost_ratio)]
}))
bcr_summary_all <- merge(bcr_summary_all, det_by_benchmark, by = c("scenario_short","recurrence_label"))
bcr_summary_all <- bcr_summary_all[order(y)]
group_centers <- sapply(block_order, function(s) mean(y_lookup[scenario_short == s, y]))

xlim_raw <- range(c(bcr_summary_all$p05, bcr_summary_all$p95, bcr_summary_all$deterministic_bcr, 1))
xpad_right <- diff(xlim_raw) * 0.20
xlim_s <- c(xlim_raw[1], xlim_raw[2] + xpad_right)

pdf(file.path(FIG, "fullBCA_recurrence_Figure4B_combined.pdf"), width=7.5, height=6.5, useDingbats=FALSE)
par(mar=c(5,10,3,1), las=1, bty="n")
plot(NA, xlim=xlim_s, ylim=c(min(y_lookup$y)-0.7, max(y_lookup$y)+0.7), yaxt="n",
     xlab="Benefit-cost ratio", ylab="", main="Full BCA (+IA, +amenity) -- all recurrence benchmarks", cex.main=0.9)
abline(v=1, lty=2, lwd=1.5)
axis(2, at=group_centers, labels=block_order, tick=FALSE)
for (i in seq_len(nrow(bcr_summary_all))) {
  row <- bcr_summary_all[i]
  col_i <- scenario_cols[as.character(row$scenario_short)]
  segments(row$p05, row$y, row$p95, row$y, lwd=2, col=col_i)
  rect(row$p25, row$y-0.30, row$p75, row$y+0.30, col=adjustcolor(col_i,alpha.f=0.35), border=col_i, lwd=2)
  points(row$median, row$y, pch=16, cex=1.1, col=col_i)
  text(row$median, row$y+0.42, labels=sprintf("%.2f", row$median), cex=0.62, col=col_i, font=2)
  text(row$p05, row$y-0.36, labels=sprintf("%.2f", row$p05), cex=0.48, col=col_i, pos=2)
  text(row$p95, row$y-0.36, labels=sprintf("%.2f", row$p95), cex=0.48, col=col_i, pos=4)
  pct_label <- sub("^.*\\(","",row$recurrence_label); pct_label <- sub("\\)$","",pct_label)
  text(xlim_s[2], row$y, labels=pct_label, pos=2, cex=0.68, col=col_i)
}
# deterministic markers drawn in a second pass, after all boxes/whiskers, so they are never
# occluded; black outline + white fill for maximum contrast against any scenario color
for (i in seq_len(nrow(bcr_summary_all))) {
  row <- bcr_summary_all[i]
  points(row$deterministic_bcr, row$y, pch=23, cex=1.5, bg="white", col="black", lwd=2.2)
  text(row$deterministic_bcr, row$y+0.30, labels=sprintf("%.2f", row$deterministic_bcr), cex=0.46, col="black", font=2)
}
for (b in seq_len(length(block_order)-1)) abline(h=(b-1)*4 + 3.5, lty=1, col="grey85")
legend("bottomright", legend=c("Monte Carlo median","Deterministic (preferred)"),
       pch=c(16,23), pt.bg=c(NA,"white"), col=c("grey30","black"), pt.cex=c(1.0,1.2), bty="n", cex=0.62)
dev.off()
cat("saved fullBCA_recurrence_Figure4B_combined.pdf\n")

############################################################
# SI ANALOGUE: Monte Carlo net-benefit + BCR density, styled to match the official
# si_monte_carlo_distributions.pdf (filled, semi-transparent density curves per scenario,
# quantile-trimmed shared x-axis, dashed reference line) -- extended to a 2x3 grid (rows =
# net benefit / BCR, columns = the 3 recurrence benchmarks) since recurrence has no
# analogue in the official (non-recurrence-based) pipeline.
############################################################
benchmark_order <- c("Lower (1.53%)","Repetitive-loss (20%)","Upper (23.28%)")

pdf(file.path(FIG, "fullBCA_recurrence_SI_monte_carlo_distributions.pdf"), width=13, height=8, useDingbats=FALSE)
par(mfrow=c(2,3), mar=c(5,5,3,1), las=1, bty="n")

# Row A: net benefits
for (bm in benchmark_order) {
  d <- mc_all[recurrence_label==bm]
  net_xlim <- quantile(d$net_benefit, c(0.01,0.99))
  dens_net <- lapply(scenario_order, function(s)
    density(d[scenario_short==s & net_benefit>=net_xlim[1] & net_benefit<=net_xlim[2], net_benefit]))
  ymax_net <- max(sapply(dens_net, function(dd) max(dd$y, na.rm=TRUE)))

  plot(NA, xlim=net_xlim, ylim=c(0, ymax_net*1.12),
       xlab="Net benefit per treated property", ylab="Density",
       main=paste0("A. Net benefits -- ", bm), cex.lab=1.05, cex.main=0.95, cex.axis=0.95, xaxt="n")
  axis(side=1, at=pretty(net_xlim), labels=format(pretty(net_xlim), big.mark=",", scientific=FALSE), cex.axis=0.85)
  abline(v=0, lty=2, lwd=1.5)

  for (i in seq_along(scenario_order)) {
    dd <- dens_net[[i]]
    polygon(c(dd$x, rev(dd$x)), c(dd$y, rep(0, length(dd$y))),
            col=adjustcolor(scenario_cols[scenario_order[i]], alpha.f=0.20), border=NA)
    lines(dd$x, dd$y, col=scenario_cols[scenario_order[i]], lwd=2.5)
  }
  if (bm=="Lower (1.53%)") legend("topright", legend=scenario_order, col=scenario_cols, lwd=2.5, bty="n", cex=0.8)
}

# Row B: benefit-cost ratios
for (bm in benchmark_order) {
  d <- mc_all[recurrence_label==bm]
  bcr_xlim <- quantile(d$benefit_cost_ratio, c(0.01,0.99))
  dens_bcr <- lapply(scenario_order, function(s)
    density(d[scenario_short==s & benefit_cost_ratio>=bcr_xlim[1] & benefit_cost_ratio<=bcr_xlim[2], benefit_cost_ratio]))
  ymax_bcr <- max(sapply(dens_bcr, function(dd) max(dd$y, na.rm=TRUE)))

  plot(NA, xlim=bcr_xlim, ylim=c(0, ymax_bcr*1.12),
       xlab="Benefit-cost ratio", ylab="Density",
       main=paste0("B. Benefit-cost ratio -- ", bm), cex.lab=1.05, cex.main=0.95, cex.axis=0.95, xaxt="n")
  axis(side=1, at=pretty(bcr_xlim), labels=format(pretty(bcr_xlim), scientific=FALSE, trim=TRUE), cex.axis=0.85)
  abline(v=1, lty=2, lwd=1.5)

  for (i in seq_along(scenario_order)) {
    dd <- dens_bcr[[i]]
    polygon(c(dd$x, rev(dd$x)), c(dd$y, rep(0, length(dd$y))),
            col=adjustcolor(scenario_cols[scenario_order[i]], alpha.f=0.20), border=NA)
    lines(dd$x, dd$y, col=scenario_cols[scenario_order[i]], lwd=2.5)
  }
}
dev.off()
cat("saved fullBCA_recurrence_SI_monte_carlo_distributions.pdf\n")

############################################################
# SI ANALOGUE, SINGLE BENCHMARK: same net-benefit + BCR density panels as above, restricted
# to the repetitive-loss (20%) recurrence benchmark only -- a direct 2x1 structural analogue
# of the official si_monte_carlo_distributions.pdf (no recurrence dimension to grid over).
############################################################
bm_single <- "Repetitive-loss (20%)"
d_single <- mc_all[recurrence_label==bm_single]

pdf(file.path(FIG, "fullBCA_recurrence_SI_monte_carlo_distributions_RL20.pdf"), width=8, height=9, useDingbats=FALSE)
par(mfrow=c(2,1), mar=c(5,5,3,1), las=1, bty="n")

# Panel A: net benefits
net_xlim <- quantile(d_single$net_benefit, c(0.01,0.99))
dens_net <- lapply(scenario_order, function(s)
  density(d_single[scenario_short==s & net_benefit>=net_xlim[1] & net_benefit<=net_xlim[2], net_benefit]))
ymax_net <- max(sapply(dens_net, function(dd) max(dd$y, na.rm=TRUE)))

plot(NA, xlim=net_xlim, ylim=c(0, ymax_net*1.12),
     xlab="Net benefit per treated property", ylab="Density",
     main="A. Monte Carlo distribution of net benefits (repetitive-loss, 20% recurrence)",
     cex.lab=1.2, cex.main=1.0, cex.axis=1, xaxt="n")
axis(side=1, at=pretty(net_xlim), labels=format(pretty(net_xlim), big.mark=",", scientific=FALSE))
abline(v=0, lty=2, lwd=1.5)
for (i in seq_along(scenario_order)) {
  dd <- dens_net[[i]]
  polygon(c(dd$x, rev(dd$x)), c(dd$y, rep(0, length(dd$y))),
          col=adjustcolor(scenario_cols[scenario_order[i]], alpha.f=0.20), border=NA)
  lines(dd$x, dd$y, col=scenario_cols[scenario_order[i]], lwd=2.5)
}
legend("topright", legend=scenario_order, col=scenario_cols, lwd=2.5, bty="n", cex=0.95)

# Panel B: benefit-cost ratios
bcr_xlim <- quantile(d_single$benefit_cost_ratio, c(0.01,0.99))
dens_bcr <- lapply(scenario_order, function(s)
  density(d_single[scenario_short==s & benefit_cost_ratio>=bcr_xlim[1] & benefit_cost_ratio<=bcr_xlim[2], benefit_cost_ratio]))
ymax_bcr <- max(sapply(dens_bcr, function(dd) max(dd$y, na.rm=TRUE)))

plot(NA, xlim=bcr_xlim, ylim=c(0, ymax_bcr*1.12),
     xlab="Benefit-cost ratio", ylab="Density",
     main="B. Monte Carlo distribution of benefit-cost ratios (repetitive-loss, 20% recurrence)",
     cex.lab=1.2, cex.main=1.0, cex.axis=1, xaxt="n")
axis(side=1, at=pretty(bcr_xlim), labels=format(pretty(bcr_xlim), scientific=FALSE, trim=TRUE))
abline(v=1, lty=2, lwd=1.5)
for (i in seq_along(scenario_order)) {
  dd <- dens_bcr[[i]]
  polygon(c(dd$x, rev(dd$x)), c(dd$y, rep(0, length(dd$y))),
          col=adjustcolor(scenario_cols[scenario_order[i]], alpha.f=0.20), border=NA)
  lines(dd$x, dd$y, col=scenario_cols[scenario_order[i]], lwd=2.5)
}
dev.off()
cat("saved fullBCA_recurrence_SI_monte_carlo_distributions_RL20.pdf\n")

############################################################
# SI SENSITIVITY (TORNADO) ANALOGUE, BY SCENARIO -- adds IA and Amenity as explicit
# tornado bars alongside the recurrence-based flood-damage drivers.
############################################################
realization_profile <- c(0.05,0.30,0.55,0.75,0.90,1.00,1.00,1.00,1.00,1.00)
pv_of <- function(annual, r=0.03, h=30) sum(annual*ifelse(seq_len(h)<=length(realization_profile),realization_profile[seq_len(h)],1)/(1+r)^seq_len(h))
base_cost <- (87160+35000)*1.15
ia_pref <- 4811  # OpenFEMA, IHP-only (see BCA.R benefit_assumptions note)
amenity_pref <- 250

# IA is now recurrence-scaled, same as cond_loss (see BCA.R run_bcaR).
annual_pref <- function(rre, p) rre*p*(cond_loss+ia_pref) + amenity_pref

bca_scenarios <- data.table(scenario_short = scenario_order, risk_reduction_effect = c(1.00, 0.77, 0.65),
                             risk_reduction_effect_sd = c(0.03, 0.0088, 0.025))
oat_by_scenario <- rbindlist(lapply(seq_len(nrow(bca_scenarios)), function(s) {
  scen <- bca_scenarios[s]
  rre <- scen$risk_reduction_effect
  rows <- list()
  # RRE low/high = +/-1.96 SD (95% CI), truncated to [0,1] -- same truncation the Monte
  # Carlo applies to its rnorm(mean=rre, sd=risk_reduction_effect_sd) draws.
  rre_lo <- pmax(rre - 1.96*scen$risk_reduction_effect_sd, 0)
  rre_hi <- pmin(rre + 1.96*scen$risk_reduction_effect_sd, 1)
  for (val in c(rre_lo,rre_hi)) rows[[length(rows)+1]] <- data.table(param="Risk-Reduction Effect", net_benefit=pv_of(annual_pref(val,p_rl))-base_cost)
  for (val in c(7776,325583)) rows[[length(rows)+1]] <- data.table(param="Acquisition Cost", net_benefit=pv_of(annual_pref(rre,p_rl))-(val+35000)*1.15)
  for (val in c(15000,80000)) rows[[length(rows)+1]] <- data.table(param="Demolition", net_benefit=pv_of(annual_pref(rre,p_rl))-(87160+val)*1.15)
  # admin_transaction_cost_share, applied to acquisition+demolition (relocation_assistance
  # preferred = $0, so omitted here as in base_cost elsewhere in this script)
  for (val in c(0.10,0.25)) rows[[length(rows)+1]] <- data.table(param="Implementation Cost", net_benefit=pv_of(annual_pref(rre,p_rl))-(87160+35000)*(1+val))
  for (val in c(p_lower,p_upper)) rows[[length(rows)+1]] <- data.table(param="Damaging-Flood Recurrence", net_benefit=pv_of(annual_pref(rre,val))-base_cost)
  for (val in c(4911,55629)) rows[[length(rows)+1]] <- data.table(param="Conditional Bldg+Contents Loss", net_benefit=pv_of(rre*p_rl*(val+ia_pref) + amenity_pref)-base_cost)
  for (val in c(2131,9299)) rows[[length(rows)+1]] <- data.table(param="Disaster Assistance (IA)", net_benefit=pv_of(rre*p_rl*(cond_loss+val) + amenity_pref)-base_cost)
  for (val in c(0,2000)) rows[[length(rows)+1]] <- data.table(param="Open-Space Amenity", net_benefit=pv_of(rre*p_rl*(cond_loss+ia_pref) + val)-base_cost)
  for (val in c(0.01,0.07)) rows[[length(rows)+1]] <- data.table(param="Discount Rate", net_benefit=pv_of(annual_pref(rre,p_rl), r=val)-base_cost)
  for (val in c(10,30)) rows[[length(rows)+1]] <- data.table(param="Horizon", net_benefit=pv_of(annual_pref(rre,p_rl), h=val)-base_cost)
  out <- rbindlist(rows)
  out[, scenario_short := scen$scenario_short]
  out
}))

oat_range_by_scenario <- oat_by_scenario[, .(
  lo=min(net_benefit), hi=max(net_benefit), range=max(net_benefit)-min(net_benefit)
), by=.(scenario_short, param)]
fwrite(oat_range_by_scenario, file.path(OUT, "oat_sensitivity_full_recurrence.csv"))

param_order <- oat_range_by_scenario[, .(avg_range=mean(range)), by=param][order(avg_range), param]

preferred_nb_by_scenario <- setNames(
  sapply(bca_scenarios$risk_reduction_effect, function(rre) pv_of(annual_pref(rre,p_rl)) - base_cost),
  bca_scenarios$scenario_short
)

pdf(file.path(FIG, "fullBCA_recurrence_SI_tornado.pdf"), width=11, height=6.5, useDingbats=FALSE)
par(mfrow=c(1,3), mar=c(5,17,3,1), las=1, bty="n")
for (s in scenario_order) {
  dt_s <- oat_range_by_scenario[scenario_short==s][match(param_order, param)]
  xlim_s <- range(c(dt_s$lo, dt_s$hi, preferred_nb_by_scenario[s], 0))
  xpad <- diff(xlim_s)*0.08; xlim_s <- c(xlim_s[1]-xpad, xlim_s[2]+xpad)
  plot(NA, xlim=xlim_s, ylim=c(0.5,nrow(dt_s)+0.5), yaxt="n", xlab="Net benefit per property (h=30, r=3%, at RL 20% recurrence)",
       ylab="", main=s, cex.main=1.0, xaxt="n")
  axis(1, at=pretty(xlim_s), labels=format(pretty(xlim_s),big.mark=",",scientific=FALSE), cex.axis=0.8)
  axis(2, at=seq_len(nrow(dt_s)), labels=dt_s$param, tick=FALSE, cex.axis=0.75)
  abline(v=0, lty=3)
  abline(v=preferred_nb_by_scenario[s], lty=2, lwd=1.5)
  for (i in seq_len(nrow(dt_s))) {
    segments(dt_s$lo[i], i, dt_s$hi[i], i, lwd=4, col=scenario_cols[s])
    points(c(dt_s$lo[i],dt_s$hi[i]), c(i,i), pch=16, col=scenario_cols[s])
  }
}
dev.off()
cat("saved fullBCA_recurrence_SI_tornado.pdf\n")

############################################################
# RECURRENCE-VS-BCR DIAGNOSTIC
############################################################
pdf(file.path(FIG, "fullBCA_recurrence_vs_BCR_diagnostic.pdf"), width=7.3, height=5.5, useDingbats=FALSE)
par(mar=c(5,5,3,8), bty="n")
plot(NA, xlim=range(det_grid$recurrence), ylim=c(0, max(det_grid$benefit_cost_ratio)*1.05),
     xlab="Annual damaging-flood recurrence", ylab="Benefit-cost ratio",
     main="Full BCA (+IA, +amenity): BCR vs. assumed damaging-flood recurrence\n(conditional loss = $45,917)", cex.main=0.8)
abline(h=1, lty=2, lwd=1.5)
for (s in scenario_order) {
  dd <- det_grid[scenario_short==s][order(recurrence)]
  lines(dd$recurrence, dd$benefit_cost_ratio, col=scenario_cols[s], lwd=2.5)
}
abline(v=p_lower, lty=3, col="grey40"); abline(v=p_rl, lty=3, col="grey40"); abline(v=p_upper, lty=3, col="grey40")
text(p_lower, max(det_grid$benefit_cost_ratio)*1.0, "NFIP\nlower", cex=0.65, col="grey30")
text(p_rl, max(det_grid$benefit_cost_ratio)*1.0, "RL\n20%", cex=0.65, col="grey30")
text(p_upper, max(det_grid$benefit_cost_ratio)*1.0, "County-\ndisaster\nupper", cex=0.65, col="grey30")
legend(x=max(det_grid$recurrence)*1.02, y=max(det_grid$benefit_cost_ratio)*0.9, xpd=NA, legend=scenario_order, col=scenario_cols, lwd=2.5, bty="n", cex=0.75)
for (s in scenario_order) {
  pstar <- breakeven[scenario_short==s, breakeven_recurrence]
  if (pstar >= min(det_grid$recurrence) && pstar <= max(det_grid$recurrence)) {
    points(pstar, 1, pch=17, cex=1.3, col=scenario_cols[s])
  }
}
dev.off()
cat("saved fullBCA_recurrence_vs_BCR_diagnostic.pdf\n")

############################################################
# DISCOUNT-RATE / HORIZON SENSITIVITY, one panel set per named recurrence benchmark
############################################################
det_full <- fread(file.path(OUT, "deterministic_benchmarks_full_grid.csv"))
real_discount_rates <- sort(unique(det_full$discount_rate))
horizons <- sort(unique(det_full$horizon))
line_types <- setNames(c(1,2,3), as.character(horizons))
point_types <- setNames(c(16,17,15), as.character(horizons))

recurrence_to_label <- c("Lower (1.53%)" = p_lower, "Repetitive-loss (20%)" = p_rl, "Upper (23.28%)" = p_upper)

for (bm_label in names(recurrence_to_label)) {
  bm_file <- gsub("[^A-Za-z0-9]","_", bm_label)
  p_val <- recurrence_to_label[[bm_label]]
  dt_bm <- det_full[abs(recurrence - p_val) < 1e-9]
  dt_bm[, horizon := factor(horizon, levels = horizons)]

  pdf(file.path(FIG, paste0("fullBCA_recurrence_discount_horizon_sensitivity_", bm_file, ".pdf")),
      width = 11, height = 4.7, useDingbats = FALSE)
  par(mfrow = c(1,3), mar = c(5,5,3,1), oma = c(0,0,2,0), las = 1, bty = "n")

  for (s in scenario_order) {
    dt_s <- dt_bm[scenario_short == s]
    col_s <- scenario_cols[s]

    ylim_s <- range(c(dt_s$benefit_cost_ratio, 1), na.rm = TRUE)
    ypad <- diff(ylim_s) * 0.12
    ylim_s <- c(max(0, ylim_s[1] - ypad), ylim_s[2] + ypad)

    plot(NA, xlim = range(real_discount_rates), ylim = ylim_s,
         xlab = "Real discount rate", ylab = "Benefit-cost ratio", main = s,
         cex.lab = 1.05, cex.main = 1.05, cex.axis = 0.95, xaxt = "n")
    axis(side = 1, at = real_discount_rates, labels = paste0(real_discount_rates * 100, "%"), cex.axis = 0.9)
    abline(h = 1, lty = 2, lwd = 1.5)

    for (h in as.character(horizons)) {
      dt_h <- dt_s[as.character(horizon) == h][order(discount_rate)]
      lines(dt_h$discount_rate, dt_h$benefit_cost_ratio, lty = line_types[h], lwd = 2, col = col_s)
      points(dt_h$discount_rate, dt_h$benefit_cost_ratio, pch = point_types[h], cex = 1, col = col_s)
    }
    legend("topright", legend = paste0(horizons, " years"), lty = line_types[as.character(horizons)],
           pch = point_types[as.character(horizons)], lwd = 2, bty = "n", cex = 0.85, title = "Horizon")
  }
  mtext(paste0("Full recurrence-based BCA (+IA, +amenity) -- recurrence = ", bm_label),
        side = 3, line = -1.3, outer = TRUE, cex = 0.85, font = 2)
  dev.off()
}
cat("saved 3x fullBCA_recurrence_discount_horizon_sensitivity_*.pdf\n")

cat("\nALL FULL-BCA RECURRENCE FIGURES DONE.\n")
