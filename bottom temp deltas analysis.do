
cd "\\net.nefsc.noaa.gov\aharris\DisMap data"


***After running the R-script to get the state biomass area in which each temp value occurred, import the data here
import excel using "bottom_temp_deltas40_1.xlsx", clear  first 
tempfile bottom_temps1
save `bottom_temps1', replace 

import excel using "bottom_temp_deltas40_2.xlsx", clear  first 
tempfile bottom_temps2
save `bottom_temps2', replace 

import excel using "bottom_temp_deltas40_3.xlsx", clear  first 
tempfile bottom_temps3
save `bottom_temps3', replace 

import excel using "bottom_temp_deltas40_4.xlsx", clear  first 
tempfile bottom_temps4
save `bottom_temps4', replace 

import excel using "bottom_temp_deltas40_5.xlsx", clear  first 
tempfile bottom_temps5
save `bottom_temps5', replace 

import excel using "bottom_temp_deltas80_1.xlsx", clear  first 
tempfile bottom_temps6
save `bottom_temps6', replace 

import excel using "bottom_temp_deltas80_2.xlsx", clear  first 
tempfile bottom_temps7
save `bottom_temps7', replace 

import excel using "bottom_temp_deltas80_3.xlsx", clear  first 
tempfile bottom_temps8
save `bottom_temps8', replace 

import excel using "bottom_temp_deltas80_4.xlsx", clear  first 
tempfile bottom_temps9
save `bottom_temps9', replace 

import excel using "bottom_temp_deltas80_5.xlsx", clear  first 

append using `bottom_temps1'
append using `bottom_temps2'
append using `bottom_temps3'
append using `bottom_temps4'
append using `bottom_temps5'
append using `bottom_temps6'
append using `bottom_temps7'
append using `bottom_temps8'
append using `bottom_temps9'

drop year
gen year=.

local y=1
forv i=0(12)960{

	local x = `i'+12
	replace year=`y' if month >`i' & month<=`x'
	local ++y
}

*Quick graph of all tmps by month 
preserve
collapse (mean) tmp, by(month)
tsset  month

gen ma = (tmp+l12.tmp+l24.tmp+l36.tmp+l48.tmp+l60.tmp+l72.tmp+l84.tmp+l96.tmp+l108.tmp)/10

*tssmooth ma smooth=tmp, window(10 1 0)
*twoway (tsline sm1) (lfit sm1 month)
twoway (tsline ma)
*twoway (tsline tmp) (tsline smooth, lcol(red))

restore


*Drop points that do not fall within any biomass access areas
tab state
drop if state_name=="character(0)"


replace state_name = `"c("MA")"' if state_name=="MA"
replace state_name = `"c("RI")"' if state_name=="RI"
replace state_name = `"c("CT")"' if state_name=="CT"
replace state_name = `"c("NY")"' if state_name=="NY"
replace state_name = `"c("NJ")"' if state_name=="NJ"
replace state_name = `"c("DE")"' if state_name=="DE"
replace state_name = `"c("MD")"' if state_name=="MD"
replace state_name = `"c("VA")"' if state_name=="VA"
replace state_name = `"c("NC")"' if state_name=="NC"

split state_name, parse("c(", ")") 
drop state_name1

save "bt_tmp_deltas_raw.dta", replace 
u "bt_tmp_deltas_raw.dta", clear 

sort state_name month
replace row_num=_n
*Many points lie in multiple areas
*Duplicate these point and assign to each state sep

*Keep hauls with only one state
preserve
keep if state_name3==""
gen state_name_new=state_name2
drop state_name2 state_name3 state_name4 state_name5
tempfile one_state
save `one_state', replace 
restore

drop  if state_name3==""

*Keep hauls with two state
preserve
keep if state_name4==""
expand 2, gen(dup)
gen state_name_new=state_name2 if dup==0
replace state_name_new= state_name3 if dup==1
sort row
drop state_name2 state_name3 state_name4 state_name5
drop dup
tempfile two_state
save `two_state', replace 
restore

drop if state_name4==""

*Keep hauls with three state
preserve
keep if state_name5==""
expand 3
bysort row_num: gen n=_n
gen state_name_new=state_name2 if n==1
replace state_name_new= state_name3 if n==2
replace state_name_new= state_name4 if n==3
drop state_name2 state_name3 state_name4 state_name5
tempfile three_state
save `three_state', replace 
restore 


drop if state_name5==""
expand 4
bysort row_num: gen n=_n
gen state_name_new=state_name2 if n==1
replace state_name_new= state_name3 if n==2
replace state_name_new= state_name4 if n==3
replace state_name_new= state_name5 if n==4

drop state_name2 state_name3 state_name4 state_name5

append using `three_state'
append using `two_state'
append using `one_state'

split state_name_new, parse(`"""')
drop state_name_new state_name_new1 state_name
rename state_name_new state
sort state month 
drop n geo row_num

/*
gen month1=1 if month=="tmpJan"
replace month1=2 if month=="tmpFeb"
replace month1=3 if month=="tmpMar"
replace month1=4 if month=="tmpApr"
replace month1=5 if month=="tmpMay"
replace month1=6 if month=="tmpJun"
replace month1=7 if month=="tmpJul"
replace month1=8 if month=="tmpAug"
replace month1=9 if month=="tmpSep"
replace month1=10 if month=="tmpOct"
replace month1=11 if month=="tmpNov"
replace month1=12 if month=="tmpDec"
*/


preserve
collapse (mean) tmp, by(month)
tsset  month
twoway (tsline tmp) (lfit tmp month)
restore

*Collapse to obatin the mean bottom temp by state year month1
collapse (mean) tmp, by(state month)

gen year=.
local y=1
forv i=0(12)960{

	local x = `i'+12
	replace year=`y' if month >`i' & month<=`x'
	local ++y
}



/*
gen yr_mnth=ym(year,month)
format yr_mnth %tm
*/
encode state, gen(st1)

xtset st1 month

levelsof state, local(sts)
foreach s of local sts{
twoway (tsline tmp if state=="`s'") (lfit tmp month if state=="`s'" , ///
				 ylabel(, angle(45) labsize(small) glcolor(gs15))  ytitle("")  ///
				 	cmissing(n) legend(off) ///
				title("`s'", size(small)) $graphoptions name(bt_`s', replace))
}				
gr combine bt_MA  bt_RI bt_CT bt_NY bt_NJ bt_DE bt_MD bt_VA bt_NC, ycommon  ///
		cols(3) title("Mean bottom temperature deltas", size(medium)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))

order year month st* tmp
rename tmp tmp_delta
export excel using "mean bottom temp deltas.xlsx", replace firstrow(variables) 