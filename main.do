*************************************************************************
*Diversity and Performance,
*Beine, Peracchi, Zanaj.

*BILATERAL SPECIFICATION

*estimation do-file
*************************************************************************
clear all
*replace with data's directory
cd "C:\Users\silviap\Dropbox\Diversity_and_performance\0 REVISION JEBO\files_replication"
*************************************************************************
use BILATERAL_replication.dta

set matsize 10000
*************************************************************************
*re-label to obtain nicer summary stats
label variable  goal_diff  "Goal difference"
label variable  hsin_GD  "Goal difference, hyperbolic sine"
label variable  SCORE_Start_o  "Initial Elo score, home"
label variable  SCORE_Start_d  "Initial Elo score, away"

label variable  abs_divers_size  "Bilateral diversity"
label variable  abs_diversityAPP_size "Bilateral diversity, appearance"
label variable  weighted_diversity_size "Bilateral diversity, SW"
label variable  Div_home_size "Diversity, home"
label variable  Div_away_size "Diversity, away"
label variable  age_home "Squad age, home"
label variable  age_dev_home "Stand. dev. squad age, home"
label variable  age_away "Squad age, away"
label variable  age_dev_away "Stand. dev. squad age, away"
label variable  age_home2 "Squad age, squared, home"
label variable  age_away2 "Squad age, squared, away"

label variable  pop_o "Population (mln), home"
label variable  pop_d "Population (mln), away"
label variable  lgdpcap_o "Log of GDP/capita, home"
label variable  lgdpcap_d "Log of GDP/capita, away"
label variable  limmig_o "Log immig. stocks, 18y lag, home"
label variable  limmig_d "Log immig. stocks, 18y lag, away"
label variable  App_Min_home "Stand. dev. appearances, home"
label variable  App_Min_away "Stand. dev. appearances, away"
label variable  Team_Strenght_faced_home "Adversary's strength, home"
label variable  Team_Strenght_faced_away "Adversary's strength, away"
label variable  contig  "Contiguity"
label variable  smctry "Same nation"
label variable  comlang_off  "Common language"
*coach
label variable  foreign_coach_home  "Foreign coach, home"
label variable  foreign_coach_away  "Foreign coach, away"
label variable  Age_approx_coach_home  "Coach age, home"
label variable  Age_approx_coach_away  "Coach age, away"
label variable  bestcoach_home  "Coach award, home"
label variable  bestcoach_away  "Coach award, away"
label variable  past_exp_coach_home  "Coach tenure, home"
label variable  past_exp_coach_away  "Coach tenure, away"
*IV
label variable  DIV_miss_dom20  "IV, 20 yrs lag, home vs. away"
label variable  DIV_miss_dom22  "IV, 22 yrs lag, home vs. away"
label variable  DIV_miss_dom  "IV, home vs. away"

label variable  DIV_miss_o  "IV, home"
label variable  DIV_miss_d "IV, away"


egen pair_id_ordered=group(home_team away_team)



rename finals Finals

sum goal_diff $DIVER 

global controls1 age_* 
global controls2 lgdpcap* limmig* age_* App_Min_away App_Min_home 
global controls3 lgdpcap* limmig* age_* pop*  App_Min_away App_Min_home contig comlang_off smct* 
global controls3_pairFE lgdpcap* limmig* age_* pop*  App_Min_away App_Min_home  

global DIVER abs_divers_size
global DIVERAPP abs_diversityAPP_size
global DIV_H Div_home_size
global DIV_A Div_away_size
global IV DIV_miss_dom
global instrument DIV_miss_dom
global FES  home_team away_team Finals yearWC 

global CLUS pair_id_ordered
*Standardize the instrument

foreach x in $IV $IV20 $IV22 DIV_miss_o DIV_miss_d {
sum `x'
replace `x' = (`x'-`r(mean)' )/`r(sd)'
}

***********************************************************************
*table 2: Sum stats
***********************************************************************
eststo clear
eststo summstats: estpost summarize goal_diff hsin_GD abs_divers_size abs_diversityAPP_size weighted_diversity_size Div_home_size ///
Div_away_size age_* App_Min_home App_Min_away  foreign_coach_* Age_approx_coach_* pop_o pop_d lgdpcap_o lgdpcap_d limmig_o limmig_d ///
Team_Strenght_faced_home Team_Strenght_faced_away contig smctry comlang_off $instrument


esttab summstats, label cells("mean sd count min max") noobs  nonumbers collabels(Mean "Standard Deviation" N Min Max) ///



esttab summstats using "table_2.tex",replace  label  nomtitle nogaps compress nonumbers ///
cells("mean(fmt(3)) sd count(fmt(g)) min max") noobs collabels(Mean "Standard deviation" N Min Max) ///
refcat(goal_diff "\textbf{\emph{Performance measures}}" ///
abs_divers_size "\textbf{\emph{Diversity measures}}" age_dev_home "\textbf{\emph{Team level variables}}" ///
pop_o "\textbf{\emph{Macroeconomic variables}}" $instrument "\textbf{\emph{IV}}", nolabel) 

*************************************************************************************************************
global STATS stats(N id wid TeamFE YrFE Age App geo PAIRFE, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls" "Minute appearances"  "Geo-political controls" "Pair FE") fmt(0 %9.2f  %9.2f %~12s %~12s %~12s  %~12s %~12s) ) 
***********************************************************************
*table 3: Baseline
***********************************************************************
eststo clear
forvalues k = 1/3 {
local ck "controls`k'"
qui eststo P`k': xi:reghdfe goal_diff $DIVER $`ck' if !missing( $IV ),  cluster($CLUS)   a( $FES)
estadd local TeamFE "Yes": P`k'
estadd local YrFE "Yes": P`k'
estadd local Age "Yes": P`k'
if `k'>1 estadd local App "Yes": P`k' 
if `k'==3 estadd local geo "Yes": P`k' 
}
qui eststo P4: xi:reghdfe goal_diff $DIVER $controls3_pairFE if !missing( $IV ),  cluster($CLUS)   a( pair_id_ordered Finals yearWC  )
estadd local TeamFE "No": P4
estadd local YrFE "Yes": P4
estadd local Age "Yes": P4
estadd local App "Yes" : P4
estadd local PAIRFE "Yes" : P4

forvalues k =  1/3  {
local ck "controls`k'"
qui eststo IV`k': xi:ivreghdfe goal_diff ($DIVER = $IV ) $`ck' , robust cluster($CLUS) first  a( $FES) savefirst savefprefix(f`k')
estadd local TeamFE "Yes": IV`k'
estadd local YrFE "Yes": IV`k'
estadd local Age "Yes": IV`k'
if `k'>1 estadd local App "Yes": IV`k' 
if `k'==3 estadd local geo "Yes": IV`k' 
estadd scalar id `e(idp)': IV`k' 
estadd scalar wid `e(widstat)': IV`k' 
*1st stage
estadd local TeamFE "Yes": f`k'$DIVER
estadd local YrFE "Yes": f`k'$DIVER
estadd local Age "Yes": f`k'$DIVER
if `k'>1 estadd local App "Yes" : f`k'$DIVER
if `k'==3 estadd local geo "Yes" : f`k'$DIVER
estadd scalar id `e(idp)': f`k'$DIVER
estadd scalar wid `e(widstat)': f`k'$DIVER
}
qui eststo IV4: xi:ivreghdfe goal_diff  ($DIVER = $IV ) $controls3_pairFE if !missing( $IV ),  cluster($CLUS )   a( pair_id_ordered Finals yearWC  ) savefirst savefprefix(f4)
estadd local TeamFE "No": IV4
estadd local YrFE "Yes": IV4
estadd local Age "Yes": IV4
estadd local App "Yes" : IV4
estadd local PAIRFE "Yes" : IV4
estadd scalar id `e(idp)': IV4
estadd scalar wid `e(widstat)': IV4
*first stage locals
estadd local TeamFE "No": f4$DIVER
estadd local YrFE "Yes": f4$DIVER
estadd local Age "Yes": f4$DIVER
estadd local App "Yes" : f4$DIVER
estadd local PAIRFE "Yes" : f4$DIVER
estadd scalar id `e(idp)': f4$DIVER
estadd scalar wid `e(widstat)': f4$DIVER

esttab P* IV*,  replace  se(%9.3f) b(%9.3f)  drop( _cons age*   ) $STATS  ///
mtitles("OLS" "OLS" "OLS" "OLS"  "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label ///
refcat(lgdpcap_o "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 


esttab P* IV* using  "table_3.tex", replace  se(%9.3f) b(%9.3f)  drop(_cons age* App* contig comlang_off smct* )  booktabs  ///
mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label $STATS  mgroups("\textbf{Dependent variable:} goal difference", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) nonotes ///
refcat(lgdpcap_o "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 



*1st STAGE
*************************************************************************************************

esttab  f1$DIVER f2$DIVER f3$DIVER f4$DIVER ,replace  se(%9.3f) b(%9.3f)  keep($IV  lgdpcap* limmig*  pop* App_Min_away App_Min_home  )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV" ) 

esttab f1$DIVER f2$DIVER f3$DIVER f4$DIVER  using "table_3_first.tex" , replace  se(%9.3f) b(%9.3f) keep($IV  lgdpcap* limmig* pop* App_Min_away App_Min_home )  booktabs  mtitles( "IV" "IV" "IV" "IV"  )   star(* .10 ** .05 *** .001) compress label $STATS ///
 mgroups("\textbf{First stage:} Dependent variable: bilateral diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0  ) span ) nonotes ///
refcat($IV "\textbf{\emph{Instrumental variable}}"  lgdpcap_o "\textbf{\emph{Control variables}}", nolabel) 




********************************************************************************
*Table 4: winning or not
g W = home_score>away_score
tab W
eststo clear
eststo drop *
forvalues k = 1/3 {
local ck "controls`k'"
qui eststo P`k': xi:reghdfe W $DIVER $`ck' if !missing( $IV ),  cluster($CLUS)   a( $FES)
estadd local TeamFE "Yes": P`k'
estadd local YrFE "Yes": P`k'
estadd local Age "Yes": P`k'
if `k'>1 estadd local App "Yes": P`k' 
if `k'==3 estadd local geo "Yes": P`k' 
}
qui eststo P4: xi:reghdfe W $DIVER $controls3_pairFE if !missing( $IV ),  cluster($CLUS)   a( pair_id_ordered Finals yearWC  )
estadd local TeamFE "No": P4
estadd local YrFE "Yes": P4
estadd local Age "Yes": P4
estadd local App "Yes" : P4
estadd local PAIRFE "Yes" : P4
 
forvalues k =  1/3  {
local ck "controls`k'"
qui eststo IV`k': xi:ivreghdfe W ($DIVER = $IV ) $`ck' , robust cluster($CLUS) first  a( $FES) savefirst savefprefix(f`k')
estadd local TeamFE "Yes": IV`k'
estadd local YrFE "Yes": IV`k'
estadd local Age "Yes": IV`k'
if `k'>1 estadd local App "Yes": IV`k' 
if `k'==3 estadd local geo "Yes": IV`k' 
estadd scalar id `e(idp)': IV`k' 
estadd scalar wid `e(widstat)': IV`k' 
*1st stage
estadd local TeamFE "Yes": f`k'$DIVER
estadd local YrFE "Yes": f`k'$DIVER
estadd local Age "Yes": f`k'$DIVER
if `k'>1 estadd local App "Yes" : f`k'$DIVER
if `k'==3 estadd local geo "Yes" : f`k'$DIVER
estadd scalar id `e(idp)': f`k'$DIVER
estadd scalar wid `e(widstat)': f`k'$DIVER
}
qui eststo IV4: xi:ivreghdfe W  ($DIVER = $IV ) $controls3_pairFE if !missing( $IV ), robust cluster($CLUS )   a( pair_id_ordered Finals yearWC  ) savefirst savefprefix(f4)
estadd local TeamFE "No": IV4
estadd local YrFE "Yes": IV4
estadd local Age "Yes": IV4
estadd local App "Yes" : IV4
estadd local PAIRFE "Yes" : IV4
estadd scalar id `e(idp)': IV4
estadd scalar wid `e(widstat)': IV4
*first stage locals
estadd local TeamFE "No": f4$DIVER
estadd local YrFE "Yes": f4$DIVER
estadd local Age "Yes": f4$DIVER
estadd local App "Yes" : f4$DIVER
estadd local PAIRFE "Yes" : f4$DIVER
estadd scalar id `e(idp)': f4$DIVER
estadd scalar wid `e(widstat)': f4$DIVER

esttab P* IV*,  replace  se(%9.3f) b(%9.3f)  drop( _cons age*   ) $STATS  ///
mtitles("OLS" "OLS" "OLS" "OLS"  "OLS" "IV" "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label ///
refcat(lgdpcap_o "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 



esttab  P* IV*  using  "table_4.tex", replace  se(%9.3f) b(%9.3f)  drop( _cons age* App* contig comlang_off smct* )  booktabs  ///
mtitles("OLS" "OLS" "OLS" "OLS"  "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label  $STATS   mgroups("\textbf{Dependent variable:} goal difference", prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) nonotes ///
refcat(past_exp_coach_home "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 



*first stage
esttab  f1$DIVER f2$DIVER f3$DIVER f4$DIVER    ,replace  se(%9.3f) b(%9.3f) keep($IV  lgdpcap* limmig* App_Min_away App_Min_home )  star(* .10 ** .05 *** .001)   label  $STATS  mtitles("IV" "IV" "IV" "IV" "IV" ) 

esttab f1$DIVER f2$DIVER f3$DIVER f4$DIVER   using "table_4_first.tex" ,replace  se(%9.3f) b(%9.3f) ///
keep($IV  lgdpcap* limmig* pop*  App_Min_away App_Min_home  )  booktabs order($IV) ///
mtitles(  "IV" "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label $STATS mgroups("\textbf{First stage:} Dependent variable: bilateral diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0  ) span ) nonotes ///
refcat($IV "\textbf{\emph{Instrumental variable}}"  foreign_coach_home "\textbf{\emph{Control variables}}", nolabel) 
 

 
 

******************************************************************************************
*Table 6: controlling for initial strength
******************************************************************************************

eststo clear
forvalues k = 1/3 {
local ck "controls`k'"
qui eststo P`k': xi: reghdfe goal_diff $DIVER $`ck' SCORE_Start_*  if !missing($IV),  cluster($CLUS )  a( $FES)
estadd local TeamFE "Yes": P`k'
estadd local YrFE "Yes": P`k'
estadd local Age "Yes": P`k'
if `k'>1 estadd local App "Yes" : P`k'
if `k'==3 estadd local geo "Yes" : P`k'

}
qui eststo P4: xi:reghdfe goal_diff $DIVER $controls3_pairFE SCORE_Start_*  if !missing( $IV ),  cluster($CLUS)   a( pair_id_ordered Finals yearWC  )
estadd local TeamFE "No": P4
estadd local YrFE "Yes": P4
estadd local Age "Yes": P4
estadd local App "Yes" : P4
estadd local PAIRFE "Yes" : P4


forvalues k = 1/3 {
local ck "controls`k'"
qui eststo IV`k': xi:ivreghdfe goal_diff ($DIVER = $IV ) $`ck'  SCORE_Start_*  , robust cluster($CLUS ) first  a( $FES) savefirst savefprefix(f`k')
estadd local TeamFE "Yes": IV`k'
estadd local YrFE "Yes": IV`k'
estadd local Age "Yes": IV`k'
if `k'>1 estadd local App "Yes" : IV`k'
if `k'==3 estadd local geo "Yes" : IV`k'
estadd scalar id `e(idp)': IV`k' 
estadd scalar wid `e(widstat)': IV`k' 
*first stage
estadd local TeamFE "Yes": f`k'$DIVER
estadd local YrFE "Yes": f`k'$DIVER
estadd local Age "Yes": f`k'$DIVER
if `k'>1 estadd local App "Yes" : f`k'$DIVER
if `k'==3 estadd local geo "Yes" : f`k'$DIVER
estadd scalar id `e(idp)': f`k'$DIVER
estadd scalar wid `e(widstat)': f`k'$DIVER
}

qui eststo IV4: xi:ivreghdfe goal_diff ($DIVER = $IV ) $controls3_pairFE SCORE_Start_*  ,  cluster($CLUS )   a( pair_id_ordered Finals yearWC  )  savefirst savefprefix(f4)
estadd local TeamFE "No": IV4
estadd local YrFE "Yes": IV4
estadd local Age "Yes": IV4
estadd local App "Yes" : IV4
estadd local PAIRFE "Yes" : IV4
estadd scalar id `e(idp)': IV4
estadd scalar wid `e(widstat)': IV4
*first stage locals
estadd local TeamFE "No": f4$DIVER
estadd local YrFE "Yes": f4$DIVER
estadd local Age "Yes": f4$DIVER
estadd local App "Yes" : f4$DIVER
estadd local PAIRFE "Yes" : f4$DIVER
estadd scalar id  `e(idp)': f4$DIVER
estadd scalar wid `e(widstat)': f4$DIVER

esttab P* IV* , replace  se(%9.3f) b(%9.3f)  drop( _cons age*  ) $STATS  ///
mtitles("OLS" "OLS" "OLS" "OLS"  "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label mgroups("\textbf{Dependent variable:} goal difference", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0  0 0 0 0 ) span ) nonotes 



esttab P* IV*  using  "table_6.tex", replace  se(%9.3f) b(%9.3f)  drop( _cons age* App* contig comlang_off smct* )  booktabs  ///
mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label $STATS ///
mgroups("\textbf{Dependent variable:} goal difference", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) nonotes ///
refcat(SCORE_Start_o "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 





*1st STAGE

esttab  f1$DIVER f2$DIVER f3$DIVER f4$DIVER ,replace  se(%9.3f) b(%9.3f)   keep($IV  lgdpcap* limmig*  pop* App_Min_away App_Min_home  SCORE_Start_*)    star(* .10 ** .05 *** .001)   label  stats(N id wid TeamFE YrFE Age App geo PAIRFE, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls" "Minute appearances"  "Geo-political controls" "Pair FE") fmt(0 %9.2f  %9.2f %~12s %~12s %~12s  %~12s %~12s) )  mtitles("IV" "IV"  "IV" "IV" ) 

esttab f1$DIVER f2$DIVER f3$DIVER f4$DIVER using "table_6_first.tex" , replace  se(%9.3f) b(%9.3f) ///
 keep($IV  lgdpcap* limmig* pop*  App_Min_away App_Min_home  SCORE_Start_*)   booktabs  ///
mtitles(  "IV" "IV"  "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label $STATS order($IV)  mgroups("\textbf{First stage:} Dependent variable: bilateral diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0  ) span ) nonotes ///
refcat($IV "\textbf{\emph{Instrumental variable}}"  SCORE_Start_o "\textbf{\emph{Control variables}}", nolabel) 



***************************************************************************************
*Table 7: further results
***************************************************************************************


eststo clear
eststo drop *

qui eststo IV_App: ivreghdfe goal_diff ($DIVERAPP = $IV )  $controls3 , robust cluster($CLUS) first a($FES) savefirst savefprefix(f)

qui eststo IV_W : xi:ivreghdfe goal_diff ( weighted_diversity_size = $IV ) $controls3 , robust cluster($CLUS)  first  a($FES) savefirst savefprefix(f2)

qui eststo IV_L22: xi:ivreghdfe goal_diff ($DIVER= DIV_miss_dom22 ) $controls3 , robust cluster($CLUS)  first a($FES) savefirst savefprefix(f3)

qui eststo IV_Coach : xi:ivreghdfe goal_diff ($DIVER=$IV )  $controls3 past_exp_coach* Age_approx_coach* bestcoac* foreign_coa*, robust cluster($CLUS)  first  a($FES) savefirst savefprefix(f4)

qui eststo IV_hsin_GD : xi:ivreghdfe hsin_GD ($DIVER = $IV ) $controls3 , robust cluster($CLUS) first  a($FES) savefirst savefprefix(f5)

qui eststo IV_HOME_SCORE : xi:ivreghdfe home_score ($DIVER= $IV )  $controls3 , first robust cluster($CLUS)  a($FES) savefirst savefprefix(f6)

qui eststo IV_SEPARATE : xi:ivreghdfe goal_diff ( $DIV_H $DIV_A  = DIV_miss_o DIV_miss_d ) $controls3 , robust cluster($CLUS)  first  a($FES) savefirst savefprefix(f7)

foreach ii in IV_App IV_W IV_L22 IV_Coach IV_HOME_SCORE IV_SEPARATE IV_hsin_GD f$DIVERAPP f2weighted_diversity_size  f3$DIVER f4$DIVER f5$DIVER f6$DIVER f7$DIV_H  {
	estadd local TeamFE "Yes": `ii'
	estadd local YrFE "Yes":  `ii'
	estadd local Age "Yes":  `ii'
	estadd local App "Yes" :  `ii'
	estadd scalar id `e(idp)': `ii'
	estadd scalar wid `e(widstat)': `ii'
}


 
esttab ,replace   se(%9.3f) b(%9.3f)  drop(*oach*  age* App*) order( $DIVER $DIVERAPP weighted_diversity_size $DIV_H $DIV_A ) stats(N idp widstat TeamFE YrFE Age App , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls" "Minute appearances"  ) fmt(0 %9.2f  %9.2f %~12s %~12s %~12s  ) )   ///
mtitles("IV_App" "IV_W" "IV_22"  "IV_Coach" "IV_HS" "IV_Home" "IV_Sep" )   star(* .10 ** .05 *** .001) compress label 

 
 *****************
local addline "\shortstack{\textbf{IV:"
esttab using "table_7.tex", replace  se(%9.3f) b(%9.3f)  drop(*oach* App* age* contig comlang_off smct*) order( $DIVER $DIVERAPP weighted_diversity_size $DIV_H $DIV_A ) stats(N idp widstat TeamFE YrFE Age App , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls" "Minute appearances"  ) fmt(0 %9.2f  %9.2f %~12s %~12s %~12s  ) )  mgroups("\textbf{Dependent variable:} goal difference" "hyperbolic sine" "home score" "goal difference", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 1 1 1  ) span ) nonotes ///
booktabs mtitles("`addline'}Diversity,\\ appearance}" "`addline'}\\ Diversity,\\ SW}" "`addline'}\\ 22 years lag}"  "`addline'}\\ Coach info}"  "`addline'}Goal difference, \\ hyperbolic sine}"  ///
 "`addline'}Outcome:\\ home score}"  "`addline'}Diversity,\\ home vs. away}" ) star(* .10 ** .05 *** .001)  label ///
refcat(lgdpcap_o "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 


*1st STAGE

esttab f$DIVERAPP  f2weighted_diversity_size  f3$DIVER f4$DIVER f5$DIVER f6$DIVER f7$DIV_H , replace  se(%9.3f) b(%9.3f) keep($IV  lgdpcap* pop* App_Min_away App_Min_home ) ///
mtitles( "IV" "IV" "IV" "IV"  "IV" "IV"  "IV" )   star(* .10 ** .05 *** .001) compress label stats(N idp widstat TeamFE YrFE Age App , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls" "Minute appearances"  ) fmt(0 %9.2f  %9.2f %~12s %~12s %~12s  ) ) 
 
local addline "\shortstack{\textbf{IV:"
esttab f$DIVERAPP  f2weighted_diversity_size  f3$DIVER f4$DIVER f5$DIVER f6$DIVER f7$DIV_H  ///
 using "table_7_first.tex" , replace  se(%9.3f) b(%9.3f) keep($IV  lgdpcap* pop* App_Min_away App_Min_home )  booktabs order( DIV* ) ///
mtitles("`addline'}Diversity,\\ appearance}" "`addline'}\\ Diversity,\\ SW}" "`addline'}\\ 22 years lag}"  "`addline'}\\ Coach info}"  "`addline'}Goal difference, \\ hyperbolic sine}"  ///
 "`addline'}Outcome:\\ home score}"  "`addline'}Diversity,\\ home vs. away}" ) star(* .10 ** .05 *** .001) compress label stats(N idp widstat TeamFE YrFE Age App , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls" "Minute appearances"  ) fmt(0 %9.2f  %9.2f %~12s %~12s %~12s  ) )   mgroups("\textbf{Dependent variable in second stage:} goal difference" "hyperbolic sine" "Home score" "Goal difference", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 1 1 1  ) span ) nonotes  ///
refcat($IV "\textbf{\emph{Instrumental variable}}"  lgdpcap_o "\textbf{\emph{Control variables}}", nolabel) 

  






********************************************************************************
*Table A2
********************************************************************************
encode home_team, ge(ht)
encode away_team, ge(at)
encode Finals, ge(fin)
*given unbalanceness
bysort pair_id_ordered (fin yearWC): ge PanelSize=_N
egen clu=group( pair_id_ordered PanelSize)
eststo clear

parallel setclusters 3
global SEEDS 1234 77745 12654 
*controls1
*
program drop _all
program my2sls, rclass
args contrs type
capture drop newclus
g newclus=clu
global cont1 "`contrs'"
reghdfe Div_away_size DIV_miss_d DIV_miss_o $cont1 , absorb(yearWC fin ht at)   residuals(Resid_d) 
reghdfe Div_home_size DIV_miss_o DIV_miss_d $cont1 , absorb(yearWC fin ht at) residuals(Resid_o) 
*home
ppmlhdfe home_score Div_away_size Div_home_size Resid_*  $cont1  ,  absorb(yearWC fin   ht at) d 
*global names : colfullnames e(b)
*foreach ii in $names {
*return scalar b`ii'=_b["`ìi'"]
*}

*return scalar BHS_Divaway=r(table)[1,1]
*return scalar BHS_Divhome=r(table)[1,2]
margins, dydx(Div*) post
matrix list r(b)
return scalar HS_Divaway=r(b)[1,1]
return scalar HS_Divhome=r(b)[1,2]
*away
ppmlhdfe away_score Div_away_size Div_home_size Resid_*   $cont1  ,   absorb(yearWC fin  ht at)  d 
*return scalar BAS_Divaway=r(table)[1,1]
*return scalar BAS_Divhome=r(table)[1,2]

margins, dydx(Div*) post
matrix list r(b)
return scalar AS_Divaway=r(b)[1,1]
return scalar AS_Divhome=r(b)[1,2]

capture drop Resid* 

end
*

global RETURN m1=r(HS_Divaway) m2=r(HS_Divhome) m3=r(AS_Divaway) m4=r(AS_Divhome)
*if you want the betas:   BHS_Divaway=r(BHS_Divaway) BHS_Divhome=r(BHS_Divhome) BAS_Divaway=r(BAS_Divaway) BAS_Divhome=r(BAS_Divhome) 



set seed 1234
timer on 1
eststo IV_POIS_HOME1: bootstrap $RETURN ,  cluster( clu)  idcluster( newclus)  reps(2000) level(90) saving(bstr1.dta): my2sls "$controls1"
estadd local TeamFE "Yes": IV_POIS_HOME1
estadd local YrFE "Yes": IV_POIS_HOME1
estadd local Age "Yes": IV_POIS_HOME1
timer off 1
timer list 1
timer clear 1

drop if missing(limmig_o)
drop if missing(limmig_d)

*controls2 

eststo IV_POIS_HOME2 : bootstrap  $RETURN  , cluster( clu)  idcluster( newclus)  seed(1234)  reps(2000) level(90) saving(bstr2.dta):  my2sls "$controls2"
estadd local TeamFE "Yes": IV_POIS_HOME2
estadd local YrFE "Yes": IV_POIS_HOME2
estadd local Age "Yes": IV_POIS_HOME2
estadd local App "Yes" : IV_POIS_HOME2


*controls3

sleep 10
set seed 1234
timer on 1
eststo IV_POIS_HOME3: bootstrap $RETURN , cluster( clu)  idcluster( newclus)  seed(1234) reps(2000)  level(90) saving(bstr3.dta): my2sls "$controls3"
estadd local TeamFE "Yes": IV_POIS_HOME3
estadd local YrFE "Yes": IV_POIS_HOME3
estadd local Age "Yes": IV_POIS_HOME3
estadd local App "Yes" : IV_POIS_HOME3
estadd local geo "Yes" : IV_POIS_HOME3

timer off 1
timer list 1
timer clear 1

**************************************************************
*
program my2slsPAIRFE, rclass
args contrs type
capture drop newclus
g newclus=clu
global cont1 "`contrs'"
reghdfe Div_away_size DIV_miss_d DIV_miss_o $cont1 , absorb(yearWC fin pair_id_ordered)   residuals(Resid_d) 
reghdfe Div_home_size DIV_miss_o DIV_miss_d $cont1 , absorb(yearWC fin ht pair_id_ordered) residuals(Resid_o) 
*home
ppmlhdfe home_score Div_away_size Div_home_size Resid_*  $cont1  ,  absorb(yearWC fin  pair_id_ordered) d 
*global names : colfullnames e(b)
*foreach ii in $names {
*return scalar b`ii'=_b["`ìi'"]
*}

*return scalar BHS_Divaway=r(table)[1,1]
*return scalar BHS_Divhome=r(table)[1,2]
margins, dydx(Div*) post
matrix list r(b)
return scalar HS_Divaway=r(b)[1,1]
return scalar HS_Divhome=r(b)[1,2]
*away
ppmlhdfe away_score Div_away_size Div_home_size Resid_*   $cont1  ,   absorb(yearWC fin pair_id_ordered)  d 
*return scalar BAS_Divaway=r(table)[1,1]
*return scalar BAS_Divhome=r(table)[1,2]

margins, dydx(Div*) post
matrix list r(b)
return scalar AS_Divaway=r(b)[1,1]
return scalar AS_Divhome=r(b)[1,2]

capture drop Resid* 

end


timer on 1
eststo IV_POIS_HOME4: bootstrap $RETURN , cluster( clu)  idcluster( newclus)  seed(1234) reps(2000)  level(90) saving(bstr4.dta): my2slsPAIRFE "$controls3_pairFE"
estadd local TeamFE "No": IV_POIS_HOME4
estadd local YrFE "Yes": IV_POIS_HOME4
estadd local Age "Yes": IV_POIS_HOME4
estadd local App "Yes" : IV_POIS_HOME4
estadd local PAIRFE "Yes" : IV_POIS_HOME4

timer off 1
timer list 1
timer clear 1


do poisson_boot.do
********************************************************************* 

 