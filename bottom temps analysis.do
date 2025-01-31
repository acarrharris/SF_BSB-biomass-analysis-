
cd "\\net.nefsc.noaa.gov\aharris\DisMap data"


***After running the R-script to get the state biomass area in which each temp value occurred, import the data here
import excel using "bottom_temps_new1.xlsx", clear  first 
tempfile bottom_temps1
save `bottom_temps1', replace 

import excel using "bottom_temps_new2.xlsx", clear  first 
tempfile bottom_temps2
save `bottom_temps2', replace 

import excel using "bottom_temps_new3.xlsx", clear  first 

*tempfile bottom_temps3
*save `bottom_temps3', replace 

*import excel using "bottom_temps_new4.xlsx", clear  first 

append using `bottom_temps1'
append using `bottom_temps2'
*append using `bottom_temps3'




*Drop points that do not fall within any biomass access areas
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
drop state_name 


replace row_num=_n

*Many points lie in multiple areas
*Duplicate these point and assign to each state sep

*Keep hauls with only one state
preserve
keep if state_name3==""
gen state_name=state_name2
drop state_name2 state_name3 state_name4 state_name5
tempfile one_state
save `one_state', replace 
restore

drop  if state_name3==""

*Keep hauls with two state
preserve
keep if state_name4==""
expand 2, gen(dup)
gen state_name=state_name2 if dup==0
replace state_name= state_name3 if dup==1
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
gen state_name=state_name2 if n==1
replace state_name= state_name3 if n==2
replace state_name= state_name4 if n==3
drop state_name2 state_name3 state_name4 state_name5
tempfile three_state
save `three_state', replace 
restore 


drop if state_name5==""
expand 4
bysort row_num: gen n=_n
gen state_name=state_name2 if n==1
replace state_name= state_name3 if n==2
replace state_name= state_name4 if n==3
replace state_name= state_name5 if n==4

drop state_name2 state_name3 state_name4 state_name5

append using `three_state'
append using `two_state'
append using `one_state'

split state_name, parse(`"""')
drop state_name state_name1
rename state_name2 state
drop n

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
destring month, replace 
drop geometry
save "historical_bottom_temps_by_state_new.dta", replace


*Make box plots by state of mean bt_tmps
u "historical_bottom_temps_by_state_new.dta", clear

gen yr_mnth=ym(year,month)
format yr_mnth %tm

encode state, gen(st1)
*graph box bt_tmp if state=="NJ", over(year) noout





*Collapse to obatin the mean bottom temp by state year month1
collapse (mean) bt_tmp, by(state year month)




encode state, gen(st1)
gen yr_mnth=ym(year,month)
format yr_mnth %tm
xtset st1 yr_mnth

drop if state=="NC"

levelsof state, local(sts)
foreach s of local sts{
twoway (tsline bt_tmp if state=="`s'") (lfit bt_tmp yr_mnth if state=="`s'" , ///
				 ylabel(, angle(horizontal) labsize(small) glcolor(gs15))  ytitle("")   ///
				 	cmissing(n) legend(off) ///
				title("`s'", size(small)) $graphoptions name(bt_`s', replace))
}				
gr combine bt_MA  bt_RI bt_CT bt_NY bt_NJ bt_DE bt_MD bt_VA ,  ycommon  ///
		cols(4) title("Mean bottom temperatures", size(medium))  graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))


		
*Now make graph for projected distirbtuions using geret transofmred dtata 		
import delimited using "decadal_bottom_temp_distributions_levels_03-3-2023.csv", clear 
encode state, gen(st1)
gen year=decade*10 
replace year=2010+year

gen yr_mnth=ym(year,month)
format yr_mnth %tm
xtset st1 yr_mnth
sort st1 yr_mnth
tsline average if state=="NJ", cmissing(n)

graph box average if state=="NJ", over(year) 

	
	
	
levelsof state, local(sts)
foreach s of local sts{
graph box average if state=="`s'", over(decade)  ///
				 ylab(,labsize(small) glcolor(gs15) angle(horizontal))  ytitle("")  ///
				 	 legend(off)  ///
				title("`s'", size(small)) $graphoptions name(bt_`s', replace)
}				
gr combine bt_MA  bt_RI bt_CT bt_NY bt_NJ bt_DE bt_MD bt_VA , ycommon  ///
		cols(4) title("", size(medium)) graphregion(fcolor(white) margin(tiny) lcolor(white)) plotregion(fcolor(white) lcolor(white) )
		

		
		
		
		
levelsof state, local(sts)
foreach s of local sts{
graph box bt_tmp if state=="`s'", over(month) ///
				title("`s'", size(small)) $graphoptions name(bt_`s', replace)
}				
gr combine bt_MA  bt_RI bt_CT bt_NY bt_NJ bt_DE bt_MD bt_VA bt_NC, ycommon  ///
		cols(3) title("Monthly temperatures", size(medium)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))

		
		
export excel using "historical mean bottom temps updated.xlsx", replace firstrow(variables) 