
cd "\\net.nefsc.noaa.gov\home2\aharris\DisMap data"
import excel using "sf survey-points-data fall.xlsx", clear  first 
gen survey="fall"
rename wtcpue sf_wtcpue
renvarlab, lower 

tempfile sf_fall
save `sf_fall', replace 

import excel using "bsb survey-points-data fall.xlsx",  clear  first   
gen survey="fall"
rename wtcpue bsb_wtcpue
renvarlab, lower 
merge 1:1 haulid   using `sf_fall'
drop _merge 

export excel using "sf bsb survey-points-data fall.xlsx", replace firstrow(variables) 


**Spring 
import excel using "sf survey-points-data spring.xlsx", clear  first 
gen survey="spring"
rename wtcpue sf_wtcpue
renvarlab, lower 

tempfile sf_spring
save `sf_spring', replace 

import excel using "bsb survey-points-data spring.xlsx",  clear  first   
gen survey="spring"
rename wtcpue bsb_wtcpue
renvarlab, lower 

merge 1:1 haulid   using `sf_spring'
drop _merge 


export excel using "sf bsb survey-points-data spring.xlsx", replace firstrow(variables) 
