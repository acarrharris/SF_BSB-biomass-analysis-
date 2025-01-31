
***This code creates trip cost distributions based on the Sabrina's 2017 trip expenditure survey data

*Enter a directory with the expenditure survey data 
*cd "C:\Users\andrew.carr-harris\Desktop\Fluke_MSE\MRIP_data"
cd "\\net.nefsc.noaa.gov\aharris\fluke_MSE\input_data"
u "atl_states_2017_expsurvey.dta", clear
renvarlab *, lower


*keep only the states we need (ME-NC) 
keep if inlist(st, 23, 33, 25, 44, 9, 36, 34, 10, 24, 51, 37)

egen n_missing_cats=rowmiss(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
drop if n_missing==16

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
egen total_exp2=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

*check to see if we get the same results as Sabrina
svy: mean lodgexp if st==34 & mode_fx=="3"
svy: mean restexp if st==34 & mode_fx=="3"

gen mode1="sh" if inlist(mode_fx, "3")
replace mode1="bt" if inlist(mode_fx, "4", "5", "7")
tostring st, gen(st2)
gen st_mode=mode1+"_" +st2
encode st_mode, gen(st_mode2)
svy: mean total_exp2 , over(st_mode2)


*^ this produdes the exact same results as in the report. I tested several exp. categories and they all mateched. 

gen state="CT" if st2=="09" 
replace state="DE" if st2=="10"
replace state="ME" if st2=="23"
replace state="MD" if st2=="24"
replace state="MA" if st2=="25"
replace state="NJ" if st2=="34"
replace state="NY" if st2=="36"
replace state="NC" if st2=="37"
replace state="RI" if st2=="44"
replace state="VA" if st2=="51"
replace state="NH" if st2=="33"

*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/


*Now generate total expenditures each state/mode combination

global costs

*Adjust for inflation (https://www.bls.gov/data/inflation_calculator.htm)
replace total_exp = total_exp*1.08

tempfile new
save `new', replace

levelsof state, local(states)
foreach s of local states{
u `new', clear

keep if state=="`s'"
di "`s'"
tempfile new2
save `new2', replace

levelsof mode1 if strat_id!="", local(modes)
foreach m of local modes{

u `new2', clear 
svy: mean total_exp if mode1=="`m'"

mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
gen state="`s'"
gen mode="`m'"


tempfile costs`s'`m'
save `costs`s'`m'', replace
global costs "$costs "`costs`s'`m''" " 
}
}
clear
dsconcat $costs

/*
replace mode="sh" if mode=="3"
replace mode="he" if mode=="4"
replace mode="ch" if mode=="5"
replace mode="pr" if mode=="7"
*/
rename mode mode1
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\trip_costs_state_summary.csv", replace


*For simulated data for each state/mode combo, do the following:
/*
u "survey_trip_costs.dta", clear 
gen id=state+"_"+mode
global costs

tempfile new
save `new', replace 
levelsof id, local(ids)
foreach i of local ids{

u `new', clear 
su mean if id=="`i'"
local mean=`r(mean)'
su st_error if id=="`i'"
local sd=`r(mean)'

clear
set obs 20000
gen cost=rnormal(`mean', `sd')
replace cost=round(cost, .01)
gen id="`i'"
split id, parse("_")
rename id1 state
rename id2 mode
drop id

tempfile costs`i'
save `costs`i'', replace
global costs "$costs "`costs`i''" " 
}
clear
dsconcat $costs

export excel using "trip_costs_NE.xlsx", firstrow(variables) replace 
*/

*To estimate the parameters of the distirbution for all modes within a single state, do the following:


 cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\hcr\"
 u "directed_trips_by_mode_2021.dta", clear 
 replace mode1="3"  if mode=="sh"
replace mode1="4" if mode=="he"
replace mode1="5" if mode=="ch"
replace mode1= "7" if mode=="pr"
rename mode1 mode1

cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\fluke_mse\simulation_R_code"
merge 1:1 state mode1 using "survey_trip_costs.dta"
*tab state if _merge==1
*drop if _merge==1
tab state
drop if state=="ME"
drop if state=="NH"

*keep if year==2019
sort state year mode


global costs

tempfile new
save `new', replace 

levelsof state, local(sts)
foreach s of local sts{

u `new', clear 
keep if state=="`s'"

tempfile new1
save `new1', replace

	levelsof mode, local(mds)
	foreach m of local mds{
		
		u `new1', clear
		keep if mode=="`m'"
		
		su mean 
		local mean=`r(mean)'
		su st_error
		local sd=`r(mean)'
		
		expand dtrip
		gen cost= max(0, rnormal(`mean',`sd'))
		replace cost=round(cost, .01)
		
		tempfile costs`s'`m'
		save `costs`s'`m'', replace
		global costs "$costs "`costs`s'`m''" " 
	}


}
clear
dsconcat $costs

tabstat cost, by(state) statistics(mean sd)
