*************************************************************************************************
*Diversity and Performance

*UNILATERAL Placebo

*dataset construction do-file



clear all

cd "C:\Users\silviap\Dropbox\Diversity_and_performance\AUGUST2020"
use UNILATERAL_replication.dta

*****************************************************************
ge  Nation = Team
replace yearWC=year
drop _merge
replace Nation ="Ireland" if regexm(Nation,"Ireland")
replace Nation ="Macedonia" if regexm(Nation,"Macedonia")

replace Nation ="Great Britain" if inlist(Nation,"Northern Ireland", "England", "Wales", "Scotland")
*ADD ATHLETICS
joinby Nation yearWC  using "Athletics1966_2018.dta", _merge(_merge) unmatched(both)

drop if _merge==2
****************************************************************
****************************************************************
*re-label variables for the table
label variable dist2herf_size "Diversity" 
label variable dist2herf_w_size "Diversity, SW" 
label variable dist2herfAPP_size "Diversity, apperance"
label variable App_Min "Stand. dev. appearances" 
label variable age_dev "Stand. dev. squad age" 
label variable age "Squad age" 
label variable age_squared "Squad age, squared" 

label variable Str_faced  "Adversary's strength" 
label variable Div_faced  "Adversary's diversity" 

label variable IV_France "IV, 18y lag"
label variable Score "Elo score" 
label variable Score1 "Elo score, computed" 
label variable Score0 "Elo's inital levels, computed" 

label variable limmig "Log immig. stocks, 18y lag" 
label variable pop_o "Population (mln)"  
label variable team_size  "Squad size"
label variable lgdpcap_UN  "Log of GDP/capita" 
*coach
label variable  foreign_coach  "Foreign coach"
label variable  Age_approx_coach  "Coach age"

forvalues i = 1978(2)2018 {
generate yr`i' =1*(yr==`i')
}

**********************************************************************

drop if regexm(Team, "Gibraltar|Faroe")
global controls1 age age_dev age_squared 
global controls2 age age_dev age_squared App_Min 
global controls3 age age_dev age_squared lgdpcap_UN pop_o App_Min
global controls4 age age_dev age_squared lgdpcap_UN limmig  App_Min  pop_o
global instrument IV_France
global DIVER  dist2herf_size
global depvar Total
xtset ID yr, delta(2)


replace Total = 0 if missing(Total)
replace Gold = 0 if missing(Gold)


************************************************************************
*Placebo  
foreach Depvar in Total Gold  {
qui eststo IV`Depvar'0: xi:ivreghdfe   `Depvar'  $controls3  ($DIVER = $instrument) ,  bw(2) a(finals Team yr) robust savefirst 
estadd local TeamFE "Yes": IV`Depvar'0
estadd local YrFE "Yes": IV`Depvar'0
estadd local Age "Yes": IV`Depvar'0

}



foreach Depvar in Total Gold  {
qui eststo IV`Depvar': xi:ivreghdfe   `Depvar'  $controls4  ($DIVER = $instrument) ,  bw(2) a(finals Team yr) robust savefirst 
estadd local TeamFE "Yes": IV`Depvar'
estadd local YrFE "Yes": IV`Depvar'
estadd local Age "Yes": IV`Depvar'

}




esttab  IVTotal0 IVTotal IVGold0 IVGold using "table_8.tex", replace  fragment   se(%9.3f) b(%9.3f)  drop( age*) order($DIVER) stats(N idstat widstat  TeamFE YrFE Age , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s))  mgroups("\textbf{Placebo}",  ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 1 0 0) span ) nonotes  ///
mtitles("\shortstack{Total\\ medals}""\shortstack{Total\\ medals}" "\shortstack{Gold\\ medals}" "\shortstack{Gold\\ medals}") star(* .10 ** .05 *** .001)  label ///
refcat(Score0 "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 


