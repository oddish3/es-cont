
*re-analysis of Bartik, Currie, Greenstone, Knittel fracking paper
global data "/Users/pedrosantanna/Documents/GitHub/Event-Studies-with-a-Continuous-Treatment/data"
* Load the match_long.dta data from their paper (available from https://www.aeaweb.org/journals/dataset?id=10.1257/app.20170487)
use "$data/match_long.dta", clear
gen sampleCty = (statefips!="36" & any_drb!=1 & shale_basin1!=301 & shale_basin1!=311 & sampleGroup==1) | (sampleGroup==2  & statefips!="36" & any_drb!=1 & shale_basin1!=301 & shale_basin1!=311)
// tab year sampleCty
keep if sampleCty
keep if sampleGroup==1


*To match Figure 5 of Bartik, Currie, Greenstone, Knittel 
* Outcome of log total county employment
gen y = ln(emp_tot)
* Treatment is prospectivity score 
gen d = valScoreM1
recode d (.=0)

*qui sum d if d>0, det

gen G = playyear1
gen t = year
*its actually fipsXplay, so the next lines keep only a county's first shale play
gen i = real(fips)
keep y d G t i shale_basin1
duplicates drop i t G d, force
* Keep only eventually-treated Shales
keep if G<.

save "$data/processed/bcgk_replication", replace
