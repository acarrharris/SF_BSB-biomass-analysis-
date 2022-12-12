

cd "\\net.nefsc.noaa.gov\home2\aharris\DisMap data"


***After running the R-script to get the state biomass area in which each haul occurred, import the data here
**Fall survey 
import excel using "Fall trawl haul locations.xlsx", clear  first 

*Drop points that do not fall within any biomass access areas
drop if state_name=="character(0)"
replace state_name = `"c("MA")"' if state_name=="MA"
replace state_name = `"c("NJ")"' if state_name=="NJ"
replace state_name = `"c("NY")"' if state_name=="NY"
replace state_name = `"c("RI")"' if state_name=="RI"
replace state_name = `"c("VA")"' if state_name=="VA"

split state_name, parse("c(", ")") 
drop state_name1
drop state_name 



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
bysort haulid: gen n=_n
gen state_name=state_name2 if n==1
replace state_name= state_name3 if n==2
replace state_name= state_name4 if n==3
drop state_name2 state_name3 state_name4 state_name5
tempfile three_state
save `three_state', replace 
restore 


drop if state_name5==""
expand 4
bysort haulid: gen n=_n
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

save "fall_hauls_by_state.dta", replace

*ktau bsb_wtcpue sf_wtcpue if state=="CT"

*Loop through the years and state_name

global ktaub

levelsof state, local(sts)
tempfile new
save `new', replace
foreach s of local sts{
	u `new', clear
	keep if state=="`s'"
	
	tempfile new2
	save `new2', replace
	
	levelsof year, local(yrs)
	foreach y of local yrs{
		u `new2', clear 
		keep if year==`y'
		
		ktau sf_wtcpue bsb_wtcpue 
		return list
		local tau`s'`y'=`r(tau_b)'
		local tau_p`s'`y'=`r(p)'
		
		spearman sf_wtcpue bsb_wtcpue
		return list
		local spear`s'`y'=`r(rho)'
		local spear_p`s'`y'=`r(p)'
		
		clear 
		set obs 1
		gen state="`s'"
		gen year=`y'
		gen ktau=`tau`s'`y''
		gen ktau_p=`tau_p`s'`y''
		
		gen sp_rho=`spear`s'`y''
		gen sp_rho_p=`spear_p`s'`y''
		
		tempfile ktaub`s'`y'
		save `ktaub`s'`y'', replace
		global ktaub "$ktaub "`ktaub`s'`y''" " 
	}

	
}
clear
dsconcat $ktaub

encode state, gen(st)
xtset st year

tsline ktau if state=="NJ"

tsfill, full		
decode st, gen(st1)
replace state=st1 if state==""
gen ns_tau = "ns" if ktau_p>0.05
gen ns_spear = "ns" if sp_rho_p>0.05


levelsof state, local(sts)
foreach s of local sts{
twoway (scatter ktau year if state=="`s'" , ///
				connect(direct) lcol(black)  mcolor(black) mlabel(ns_tau) mlabpos(6)   lpat(solid) msymbol(o)msize(vsmall) ///
				xlabel(1974(4)2019, angle(45) labsize(small) )  yline(0) ylabel(, angle(45) labsize(small) glcolor(gs15))  ytitle("Kendall's tau-b", size(small))  ///
				xtitle("year", size(small) ) 	cmissing(n) ///
				title("`s'", size(small)) $graphoptions name(ktau_`s', replace))
}				
gr combine ktau_MA  ktau_RI ktau_CT ktau_NY ktau_NJ ktau_DE ktau_MD ktau_VA ktau_NC, ycommon note("ns signifies p-values>0.05") ///
		cols(3) title("Correlation (kendall's tau-b) between fluke and black sea bass" "CPUE on NMFS Fall survey", size(medium)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))


levelsof state, local(sts)
foreach s of local sts{
twoway (scatter sp_rho year if state=="`s'" , ///
				connect(direct) lcol(black)  mcolor(black) mlabel(ns_spear) mlabpos(6)  lpat(solid) msymbol(o)msize(vsmall) ///
				xlabel(1974(4)2019, angle(45) labsize(small) )  yline(0) ylabel(, angle(45) labsize(small) glcolor(gs15))  ytitle("Spearman's rho'", size(small))  ///
				xtitle("year", size(small) ) 	cmissing(n) ///
				title("`s'", size(small)) $graphoptions name(ktau_`s', replace))
}				
gr combine ktau_MA  ktau_RI ktau_CT ktau_NY ktau_NJ ktau_DE ktau_MD ktau_VA ktau_NC, ycommon  note("ns signifies p-values>0.05") ///
		cols(3) title("Correlation (spearman's rho) between fluke and black sea bass" "CPUE on NMFS Fall survey", size(medium)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))
		
export excel using "sf bsb trawl survey correlations fall.xlsx", replace firstrow(variables) 

import excel using "sf bsb trawl survey correlations fall.xlsx", clear  first











**Spring survey 
import excel using "Spring trawl haul locations.xlsx", clear  first 

*Drop points that do not fall within any biomass access areas
drop if state_name=="character(0)"
replace state_name = `"c("MA")"' if state_name=="MA"
replace state_name = `"c("NJ")"' if state_name=="NJ"
replace state_name = `"c("NY")"' if state_name=="NY"
replace state_name = `"c("RI")"' if state_name=="RI"
replace state_name = `"c("VA")"' if state_name=="VA"

split state_name, parse("c(", ")") 
drop state_name1
drop state_name 



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
bysort haulid: gen n=_n
gen state_name=state_name2 if n==1
replace state_name= state_name3 if n==2
replace state_name= state_name4 if n==3
drop state_name2 state_name3 state_name4 state_name5
tempfile three_state
save `three_state', replace 
restore 


drop if state_name5==""
expand 4
bysort haulid: gen n=_n
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

save "spring_hauls_by_state.dta", replace

*ktau bsb_wtcpue sf_wtcpue if state=="CT"

*Loop through the years and state_name

mvencode bsb_wtcpue sf_wtcpue, mv(0) override
global ktaub

levelsof state, local(sts)
tempfile new
save `new', replace
foreach s of local sts{
	u `new', clear
	keep if state=="`s'"
	
	tempfile new2
	save `new2', replace
	
	levelsof year, local(yrs)
	foreach y of local yrs{
		u `new2', clear 
		keep if year==`y'
		
		ktau sf_wtcpue bsb_wtcpue 
		return list
		local tau`s'`y'=`r(tau_b)'
		local tau_p`s'`y'=`r(p)'
		
		spearman sf_wtcpue bsb_wtcpue
		return list
		local spear`s'`y'=`r(rho)'
		local spear_p`s'`y'=`r(p)'
		
		clear 
		set obs 1
		gen state="`s'"
		gen year=`y'
		gen ktau=`tau`s'`y''
		gen ktau_p=`tau_p`s'`y''
		
		gen sp_rho=`spear`s'`y''
		gen sp_rho_p=`spear_p`s'`y''
		
		tempfile ktaub`s'`y'
		save `ktaub`s'`y'', replace
		global ktaub "$ktaub "`ktaub`s'`y''" " 
	}

	
}
clear
dsconcat $ktaub

encode state, gen(st)
xtset st year


tsfill, full		
decode st, gen(st1)
replace state=st1 if state==""
gen ns_tau = "ns" if ktau_p>0.05
gen ns_spear = "ns" if sp_rho_p>0.05


levelsof state, local(sts)
foreach s of local sts{
twoway (scatter ktau year if state=="`s'" , ///
				connect(direct) lcol(black)  mcolor(black) mlabel(ns_tau) mlabpos(6) lwidth(medthick)  lpat(solid) msymbol(o)msize(vsmall) ///
				xlabel(1974(4)2019, angle(45) labsize(small) )  yline(0) ylabel(, angle(45) labsize(small) glcolor(gs15))  ytitle("Kendall's tau-b", size(small))  ///
				xtitle("year", size(small) ) 	cmissing(n) ///
				title("`s'", size(small)) $graphoptions name(ktau_`s', replace))
}				
gr combine ktau_MA  ktau_RI ktau_CT ktau_NY ktau_NJ ktau_DE ktau_MD ktau_VA ktau_NC, ycommon note("ns signifies p-values>0.05") ///
		cols(3) title("Correlation (kendall's tau-b) between fluke and black sea bass" "CPUE on NMFS Spring survey", size(medium)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))


levelsof state, local(sts)
foreach s of local sts{
twoway (scatter sp_rho year if state=="`s'" , ///
				connect(direct) lcol(black)  mcolor(black) mlabel(ns_spear) mlabpos(6) lwidth(medthick)  lpat(solid) msymbol(o)msize(vsmall) ///
				xlabel(1974(4)2019, angle(45) labsize(small) )  yline(0) ylabel(, angle(45) labsize(small) glcolor(gs15))  ytitle("Spearman's rho'", size(small))  ///
				xtitle("year", size(small) ) 	cmissing(n) ///
				title("`s'", size(small)) $graphoptions name(ktau_`s', replace))
}				
gr combine ktau_MA  ktau_RI ktau_CT ktau_NY ktau_NJ ktau_DE ktau_MD ktau_VA ktau_NC, ycommon  note("ns signifies p-values>0.05") ///
		cols(3) title("Correlation (spearman's rho) between fluke and black sea bass" "CPUE on Spring Fall survey", size(medium)) graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white))
		
export excel using "sf bsb trawl survey correlations fall.xlsx", replace firstrow(variables) 