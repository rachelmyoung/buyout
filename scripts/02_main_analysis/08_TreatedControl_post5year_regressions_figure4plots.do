cd "/Users/rachelyoung/Dropbox/Princeton/research/buyoutprogram/2025/TOSHARE/replication_package"
set scheme s1color

local outest "output/estimates"


* read in the data
use "./cleandata/TOY_TreatedControl_post5year.dta", clear

set scheme s1color

encode STATE, gen(STATE_factor)

* Loop over the two versions of the treatment group (ATE and ITT)
foreach v in 0/1 {
	
	if `v'==1 {
		di "running version conditional on moving (ATE)"
		drop if post_5yr==1 & moved_address==0 & Treated==1
	} 
	else {
		di "running ITT version"
	}

loc outcome "moved_address MOVED_ZIP MOVED_STATE SFHA richer"
foreach y in `outcome' {

* Model 1
xi: reg `y' Treated##post_5yr , cluster(STATE)
estimate store m1
est save "`outest'/m1_`y'", replace

	
* Model 2
xi: reg `y' i.EVENTYEAR i.STATE Treated##post_5yr , cluster(STATE)
estimate store m2
est save "`outest'/m2_`y'", replace


* Model 3 
reg `y' i.EVENTYEAR##i.statefp Treated##post_5yr , cluster(statefp)
estimate store m3
est save "`outest'/m3_`y'", replace


* Model 4 
reg `y' i.countyfp i.EVENTYEAR##i.statefp Treated##post_5yr , cluster(statefp)
estimate store m4
est save "`outest'/m4_`y'", replace


* Model census tract 50% black
xi: reg `y' i.EVENTYEAR i.STATE high_frac_black##Treated##post_5yr , cluster(STATE)
estimate store m2_black50
est save "`outest'/m2_black50_`y'", replace


* Model census tract 20% black (75th percentile)
xi: reg `y' i.EVENTYEAR i.STATE high_frac_black_75##Treated##post_5yr , cluster(STATE)
estimate store m2_black20
est save "`outest'/m2_black20_`y'", replace


* Model SFHA
preserve
keep if treated_sfha_3==1 | treated_sfha_3==0 
*xi: reg `y' i.EVENTYEAR i.STATE fld_zone_factor##Treated##post_5yr , cluster(STATE)
xi: reg `y' i.EVENTYEAR i.STATE Treated##post_5yr , cluster(STATE)
estimate store m2_sfha
est save "`outest'/m2_sfha_`y'", replace
restore


* Model high income per cap
xi: reg `y' i.EVENTYEAR i.STATE high_income_percap##Treated##post_5yr , cluster(STATE)
estimate store m2_incomepercap
est save "`outest'/m2_incomepercap_`y'", replace


* model high median income
xi: reg `y' i.EVENTYEAR i.STATE high_median_income##Treated##post_5yr , cluster(STATE)
estimate store m2_incomemedian
est save "`outest'/m2_incomemedian_`y'", replace


estout * using output/treatedControl_post5year_regression_`y'.csv , cells(b se(par)) stats(N vce) replace


}


/*--------------------------------------------------- Make the plots */


loc outcome "moved_address MOVED_ZIP MOVED_STATE SFHA richer"
foreach y in `outcome' {

* Make coefficeint plot for each outcome

clear
set obs 11
gen xaxis=_n 
replace xaxis = xaxis-4 if xaxis>=8
gen model = ""
sort xaxis
gen subgroup = _n
gen b = .
gen se =.
gen b_percent = .
gen se_percent = .


label define model_lbl 1 "Zip Code (no FE)"
label define model_lbl 2 "Zip Code", add
label define model_lbl 3 "SFHA", add
label define model_lbl 4 "50% Black", add
label define model_lbl 5 "20% Black", add
label define model_lbl 6 "High Income per Capita", add
label define model_lbl 7 "High Median Income", add
label values xaxis model_lbl


label define coeff_lbl 1 "Zip Code (no FE)"
label define coeff_lbl 2 "Zip Code", add
label define coeff_lbl 3 "SFHA", add
label define coeff_lbl 4 "<50% Black", add
label define coeff_lbl 5 ">=50% Black", add
label define coeff_lbl 6 "<20% Black", add
label define coeff_lbl 7 ">=20% Black", add
label define coeff_lbl 8 "Low Income per Capita", add
label define coeff_lbl 9 "High Income per Capita", add
label define coeff_lbl 10 "Low Median Income", add
label define coeff_lbl 11 "High Median Income", add
label values subgroup coeff_lbl

local mList "m1 m2 m2_sfha m2_black50 m2_black20 m2_incomepercap"

local count = 1

foreach m in `mList' {
di "`m'"

replace model = "`m'" if xaxis==`count'

est use "`outest'/`m'_`y'.ster"

if "`m'" == "m1" | "`m'"== "m2" | "`m'"== "m2_sfha" {
replace b = _b[1.Treated#1.post_5yr] if model == "`m'"
replace se = _se[1.Treated#1.post_5yr] if model == "`m'"

nlcom (_b[1.Treated#1.post_5yr] / _b[1.post_5yr]) -1
replace b_percent = el(e(b),1,1) if model == "`m'"
replace se_percent = sqrt(el(e(V),1,1)) if model == "`m'"
}

if "`m'" == "m2_black50"  {
replace b = _b[1.Treated#1.post_5yr] if model == "`m'"  & subgroup==4
replace se = _se[1.Treated#1.post_5yr] if model == "`m'" & subgroup==4


lincom _b[1.high_frac_black#1.Treated#1.post_5yr] + _b[1.Treated#1.post_5yr] , level(95)
replace b = r(estimate) if model == "m2_black50" & subgroup==5
replace se = r(se) if model == "`m'" & subgroup==5

* percent increase from treatmenet within group
nlcom ((_b[1.post_5yr] +_b[1.Treated#1.post_5yr]) / (_b[1.post_5yr] ) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==4
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==4


nlcom ((_b[1.post_5yr]+_b[1.Treated#1.post_5yr] +_b[1.high_frac_black#1.post_5yr] + _b[1.high_frac_black#1.Treated#1.post_5yr]) / (_b[1.post_5yr] + _b[1.high_frac_black#1.post_5yr]) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==5
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==5
}

if "`m'"=="m2_black20" {
*est use "`outest'/m2_black20_moved_address.ster"
	
replace b = _b[1.Treated#1.post_5yr] if model == "`m'" & subgroup==6
replace se = _se[1.Treated#1.post_5yr] if model == "`m'" & subgroup==6

lincom _b[1.high_frac_black_75#1.Treated#1.post_5yr] + _b[1.Treated#1.post_5yr], level(95)
replace b = r(estimate) if model == "`m'" & subgroup==7
replace se = r(se) if model == "`m'" & subgroup==7


* percent increase from treatmenet within group
nlcom ((_b[1.post_5yr] +_b[1.Treated#1.post_5yr] ) / (_b[1.post_5yr] ) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==6
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==6

nlcom ((_b[1.post_5yr]+_b[1.Treated#1.post_5yr] +_b[1.high_frac_black_75#1.post_5yr] + _b[1.high_frac_black_75#1.Treated#1.post_5yr]) / (_b[1.post_5yr] + _b[1.high_frac_black_75#1.post_5yr]) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==7
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==7
}
if "`m'"=="m2_incomepercap" {
replace b = _b[1.Treated#1.post_5yr] if model == "`m'" & subgroup==8
replace se = _se[1.Treated#1.post_5yr] if model == "`m'" & subgroup==8

lincom _b[1.high_income_percap#1.Treated#1.post_5yr] + _b[1.Treated#1.post_5yr], level(95)
replace b = r(estimate) if model == "`m'" & subgroup==9
replace se = r(se) if model == "`m'" & subgroup==9

* percent increase from treatmenet within group
nlcom ((_b[1.post_5yr] + _b[1.Treated#1.post_5yr] ) / (_b[1.post_5yr] ) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==8
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==8

nlcom ((_b[1.post_5yr]+_b[1.Treated#1.post_5yr] +_b[1.high_income_percap#1.post_5yr] + _b[1.high_income_percap#1.Treated#1.post_5yr]) / (_b[1.post_5yr] + _b[1.high_income_percap#1.post_5yr]) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==9
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==9
}

if "`m'"=="m2_incomemedian" {
replace b = _b[1.Treated#1.post_5yr] if model == "`m'" & subgroup==10
replace se = _se[1.Treated#1.post_5yr] if model == "`m'" & subgroup==10

lincom _b[1.high_median_income#1.Treated#1.post_5yr] + _b[1.Treated#1.post_5yr], level(95)
replace b = r(estimate) if model == "`m'" & subgroup==11
replace se = r(se) if model == "`m'" & subgroup==11

* percent increase from treatmenet within group
nlcom ((_b[1.post_5yr] +_b[1.Treated#1.post_5yr] ) / (_b[1.post_5yr] ) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==10
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==10


nlcom ((_b[1.post_5yr]+_b[1.Treated#1.post_5yr] +_b[1.high_median_income#1.post_5yr] + _b[1.high_median_income#1.Treated#1.post_5yr]) / (_b[1.post_5yr] + _b[1.high_median_income#1.post_5yr]) ) -1
replace b_percent = el(r(b),1,1) if model == "`m'"  & subgroup==11
replace se_percent = sqrt(el(r(V),1,1)) if model == "`m'"  & subgroup==11
}


local count = `count' + 1
}

gen ci1=b-se*1.96
gen ci2=b+se*1.96 
gen ci1_percent=b_percent-se_percent*1.96
gen ci2_percent=b_percent+se_percent*1.96 

replace b_percent = b_percent*100
replace ci1_percent=ci1_percent*100
replace ci2_percent=ci2_percent*100

if "`y'"=="moved_address" {
tw (scatter subgroup b ,  mcolor(forest_green)) ///
(rcap  ci1 ci2 subgroup,  horizontal lcolor(forest_green%75)), ///
xline(0) ylabel(1(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("likelihood 5 years after buyout") title("Moved to a new address") ///
legend(off)

graph save figures/est_allmodels_`y'.gph, replace

preserve
drop if xaxis<4
tw (scatter subgroup b_percent ,  mcolor(forest_green)) ///
(rcap  ci1_percent ci2_percent subgroup,  horizontal lcolor(forest_green%75)), ///
xline(0) ylabel(4(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("percent change 5 years after") title("Moved to a new address") ///
legend(off)

graph save figures/est_allmodels_`y'_percent.gph, replace
restore
}

if "`y'"=="MOVED_ZIP" {
tw (scatter subgroup b , mcolor(cranberry)) ///
(rcap  ci1 ci2 subgroup,  horizontal lcolor(cranberry%75)), ///
xline(0) ylabel(1(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("likelihood 5 years after buyout") title("New zip code") ///
legend(off)

graph save figures/est_allmodels_`y'.gph, replace

preserve
drop if xaxis<4
tw (scatter subgroup b_percent , mcolor(cranberry)) ///
(rcap  ci1_percent ci2_percent subgroup,  horizontal lcolor(cranberry%75)), ///
xline(0) ylabel(4(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("percent change 5 years after") title("New zip code") ///
legend(off)

graph save figures/est_allmodels_`y'_percent.gph, replace
restore 
}

if "`y'"=="MOVED_STATE" {
tw (scatter subgroup b ,  mcolor(edkblue)) ///
(rcap  ci1 ci2 subgroup,  horizontal lcolor(edkblue%75)), ///
xline(0) ylabel(1(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("likelihood 5 years after buyout") title("New state") ///
legend(off)

graph save figures/est_allmodels_`y'.gph, replace

preserve
drop if xaxis<4
tw (scatter subgroup b_percent ,  mcolor(edkblue)) ///
(rcap  ci1_percent ci2_percent subgroup,  horizontal lcolor(edkblue%75)), ///
xline(0) ylabel(4(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("percent change 5 years after") title("New state") ///
legend(off)

graph save figures/est_allmodels_`y'_percent.gph, replace
restore
}

if "`y'"=="richer" {
tw (scatter subgroup b ,  mcolor(maroon)) ///
(rcap  ci1 ci2 subgroup,  horizontal lcolor(maroon%75)), ///
xline(0) ylabel(1(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("likelihood 5 years after buyout") title("Richer census tract") ///
legend(off)

graph save figures/est_allmodels_`y'.gph, replace

preserve 
drop if xaxis<4
tw (scatter subgroup b_percent ,  mcolor(maroon)) ///
(rcap  ci1_percent ci2_percent subgroup,  horizontal lcolor(maroon%75)), ///
xline(0) ylabel(4(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("percent change 5 years after") title("Richer census tract") ///
legend(off)

graph save figures/est_allmodels_`y'_percent.gph, replace
restore 
}
if "`y'"=="SFHA" {
tw (scatter subgroup b ,  mcolor(orange)) ///
(rcap  ci1 ci2 subgroup,  horizontal lcolor(orange%75)), ///
xline(0) ylabel(1(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("likelihood 5 years after buyout") title("SFHA") ///
legend(off)

graph save figures/est_allmodels_`y'.gph, replace

preserve
drop if xaxis<4
tw (scatter subgroup b_percent ,  mcolor(orange)) ///
(rcap  ci1_percent ci2_percent subgroup,  horizontal lcolor(orange%75)), ///
xline(0) ylabel(4(1)9, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("percent change 5 years after") title("SFHA") ///
legend(off)

graph save figures/est_allmodels_`y'_percent.gph, replace
restore
}

if "`y'"=="median_value_owner_occ_spec" {
tw (scatter subgroup b if xaxis<6,  mcolor(blue)) ///
(rcap  ci1 ci2 subgroup if xaxis<6,  horizontal lcolor(blue%75)), ///
xline(0) ylabel(1(1)7, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("likelihood 5 years after buyout") title("Median owner occupied value") ///
legend(off)

graph save figures/est_allmodels_`y'.gph, replace

preserve
drop if xaxis<4
tw (scatter subgroup b_percent if xaxis<6,  mcolor(blue)) ///
(rcap  ci1_percent ci2_percent subgroup if xaxis<6,  horizontal lcolor(blue%75)), ///
xline(0) ylabel(4(1)7, valuelabel format(%9.0f) labs(small) angle(45)) xsize(4) ysize(8) ///
ytitle("") xtitle("percent change 5 years after") title("Median owner occupied value") ///
legend(off)

graph save figures/est_allmodels_`y'_percent.gph, replace
restore
}	
}

**** Combine the plots
graph combine figures/est_allmodels_MOVED_ZIP.gph figures/est_allmodels_MOVED_STATE.gph  figures/est_allmodels_SFHA.gph figures/est_allmodels_richer.gph, rows(2) cols(2)  
graph save figures/combine_est_allmodels_4.gph, replace

graph combine figures/est_allmodels_moved_address.gph figures/combine_est_allmodels_4.gph, col(2) 
graph export figures/est_allmodels_combined.pdf, replace


graph combine figures/est_allmodels_moved_address.gph  figures/est_allmodels_SFHA.gph figures/est_allmodels_richer.gph, col(3) xsize(6) ysize(2)

}
