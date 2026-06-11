cd "~/project/buyout"
set scheme s1color

* read in nhgis nominal file to get tracts
import delim using ./rawdata/nhgis0001_ts_nominal_tract.csv, clear

tempfile nhgis
save `nhgis'

* read in decennial income file and make gjoin2000
use "./rawdata/DECENNIALDPSF32000.DP3_data_with_overlays_2021-05-23T183709.dta", clear
generate splitat = ustrpos(geo_id,"US")
list geo_id if splitat == 0

/*This is not working correctly need to fix*/
generate str1 defendant = ""
replace defendant = usubstr(geo_id,splitat + 2,.)

generate str1 statefp  = ""
replace statefp = usubstr(defendant,1,2)
generate str1 countyfp = ""
replace countyfp = usubstr(defendant,3,3)
generate str1 tracta= ""
replace tracta = usubstr(defendant,6,.)

destring statefp countyfp tracta, replace

drop defendant
drop dp3_e*

tempfile decennial
save `decennial'


forvalue j = 2000/2017 {
	
if "`j'"=="2000" {
	use "./output/fullsample_expanded_2000_geocoded_ACS.dta", clear /*These files were created in NEW4-geocoded_acs_plots*/
	gen flood_sum = .
}
else {
import delimited using "./output/NEW9-uniqueaddress-event-year`j'.csv", clear /*These files were created in NEW9*/

rename pid year fname lname addrid gender effdate address city state zip eventtime  moved_state moved_zip buyout_zip buyout_state buyout_address eventyear  lastmonth firstmonth lastyear firstyear, upper

replace statefp = "" if statefp=="NA"
replace statenh = "" if statenh=="NA"
replace countyfp = "" if countyfp=="NA"
replace countynh = "" if countynh=="NA"
replace tracta = "" if tracta=="NA"

destring statefp statenh countyfp countynh tracta, replace
destring b* a* c*, replace ignore("NA")
rename treated Treated
}


********* Remove Duplicates and People that are PO Boxes *********
sort PID YEAR ADDRESS
by PID: drop if strpos(ADDRESS, "PO BOX")

duplicates drop YEAR PID , force 



merge m:1 nhgiscode gjoin1970 gjoin1980 gjoin1990 gjoin2000 gjoin2010 gjoin2012  statefp statenh county countyfp countynh tracta name1970 name1980 name1990 name2000 name2010 name2012 using `nhgis', keep(1 3) force

********* Make and clean up some variables for analysis *********
* population
rename av0aa2000 total_pop_2000
label variable total_pop_2000 "total population 2000"
gen l_pop_2000 = log(total_pop_2000)
label variable l_pop_2000 "log total population 2000"

* Age
gen frac_under18 = (ax8aa2000 + ax8ab2000 + ax8ac2000 + ax8ad2000)/total_pop_2000
label variable frac_under18 "fraction under 18"
gen frac_over65 = (ax8ar2000 + ax8as2000 + ax8at2000 + ax8au2000 + ax8av2000)/total_pop_2000
label variable frac_over65 "fraction over 65"


* Flood Zone as a factor variable
label define floodlabel 3 "1-percent annual chance flood" 2 "moderate or 0.2 percent annual chance flood" 1 "minimal flood hazard"
gen fld_zone_factor = .
replace fld_zone_factor = 3 if inlist(fld_zone, "A","AE","AH","AO","AR", "V", "VE", "V*", "A*")
replace fld_zone_factor = 2 if inlist(fld_zone, "B", "X")
replace fld_zone_factor = 1 if inlist(fld_zone, "C")
label values fld_zone_factor floodlabel
label variable fld_zone_factor "Property Flood Factor"

gen BUYOUT_CENSUS_TRACT_temp = tracta if YEAR==EVENTYEAR
sort PID YEAR
bysort PID : egen BUYOUT_CENSUS_TRACT = min(BUYOUT_CENSUS_TRACT_temp)
drop BUYOUT_CENSUS_TRACT_temp

gen BUYOUT_FLOOD_ZONE_temp = fld_zone_factor if YEAR==EVENTYEAR
sort PID YEAR
bysort PID : egen  BUYOUT_FLOOD_ZONE = min(BUYOUT_FLOOD_ZONE_temp)
drop BUYOUT_FLOOD_ZONE_temp
gen LOWER_FLOOD_ZONE = 0
replace LOWER_FLOOD_ZONE=1 if BUYOUT_FLOOD_ZONE<fld_zone_factor

** Created a 1% flood risk control group
gen treated_sfha_3 = .
replace treated_sfha_3 = 0 if BUYOUT_FLOOD_ZONE==3
replace treated_sfha_3 = 1 if Treated==1
** Created a 1% or 0.2%flood risk control group
gen treated_sfha_23 = .
replace treated_sfha_23 = 0 if BUYOUT_FLOOD_ZONE==3 | BUYOUT_FLOOD_ZONE==2
replace treated_sfha_23 = 1 if Treated==1

* Family income 
merge m:1 statefp countyfp tracta using `decennial', keep(1 3) nogen force

rename dp3_c151 fam_income_percap
label variable fam_income_percap "family income per capita (dollars)"

rename dp3_c150 fam_income_median
label variable fam_income_median "Median family income"

*** Higher income? (census tract)
gen BUYOUT_fam_income_median_temp = fam_income_median if YEAR==EVENTYEAR
sort PID YEAR
bysort PID : egen BUYOUT_fam_income_median = min(BUYOUT_fam_income_median_temp)
drop BUYOUT_fam_income_median_temp

gen HIGHER_MEDIAN_FAM_INCOME = 0
replace HIGHER_MEDIAN_FAM_INCOME = 1 if fam_income_median>BUYOUT_fam_income_median

*destring s1901_2010_c01_012e , gen(HH_income_mean) force
*egen HH_income_mean_bin = cut(HH_income_mean), at(10000 14999 24999 34999 49999 74999 99999 149999 199999 250000)

* employment 
gen frac_employed_16plus_2000 = dp3_c42 / total_pop_2000
label variable frac_employed_16plus_2000  "fraction 16+ and employed (nhgis)"

gen frac_employed_16plus_2000_TEST =  b84aa2000 / total_pop_2000
label variable frac_employed_16plus_2000_TEST "fraction 16+ and employed (decennial)"

gen frac_primeworkage_2000 = (b57ai2000 + b57aj2000 + b57ak2000 + b57al2000 + b57am2000 + b57an2000) / total_pop_2000
label variable frac_primeworkage_2000  "fraction 16+"

* fraction black, white, etc.
gen frac_black = b18ab2000/total_pop_2000
label variable frac_black "fraction black"
gen frac_white = b18aa2000/total_pop_2000
label variable frac_white "fraction white"

* educational attainment
gen frac_less_than_HS = b69aa2000/total_pop_2000
label variable frac_less_than_HS "fraction less than HS"

gen frac_some_college = b69ab2000/total_pop_2000
label variable frac_some_college "fraction HS and some college"

gen frac_college_plus = b69ac2000/total_pop_2000
label variable frac_college_plus "fraction college degree +"


* keep the variables needed for the anaylsis:

keep PID YEAR FNAME LNAME ADDRID GENDER EFFDATE ADDRESS CITY STATE ZIP EVENTTIME moved_address MOVED_STATE MOVED_ZIP BUYOUT_ZIP BUYOUT_STATE BUYOUT_ADDRESS LOWER_FLOOD_ZONE BUYOUT_FLOOD_ZONE EVENTYEAR Treated LASTMONTH FIRSTMONTH LASTYEAR FIRSTYEAR gjoin2000 fld_zone fld_zone_factor sfha_tf static_bfe ymax ymin xmax xmin statefp countyfp tracta moved_address frac_white frac_black l_pop_2000 frac_primeworkage_2000 fam_income_percap fam_income_median total_pop_2000 frac_employed_16plus_2000_TEST frac_under18 frac_over65 frac_college_plus frac_some_college frac_less_than_HS treated_sfha_23 treated_sfha_3 flood_sum


save output/TreatedControlAddresses_forRegression_`j'_sfha_floodevent.dta, replace

export delim output/TreatedControlAddresses_forRegression_`j'_sfha_floodevent.csv, replace


* collapse census tract 

}

* Append Data

use output/TreatedControlAddresses_forRegression_2001_sfha_floodevent.dta, clear

forvalue y = 2002/2017 {
	append using output/TreatedControlAddresses_forRegression_`y'_sfha_floodevent.dta
}


append using output/TreatedControlAddresses_forRegression_2000_sfha_floodevent.dta, force

gen flooded = .
replace flooded = 0 if flood_sum==0
replace flooded = 1 if flood_sum>0 & flood_sum<=4

save output/TreatedControlAddresses_forRegression_allyear_sfha_floodevent.dta, replace


