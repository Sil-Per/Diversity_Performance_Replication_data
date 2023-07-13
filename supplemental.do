*************************************************************************************************
*Diversity and Performance

*UNILATERAL SPECIFICATION

*estimation do-file
clear all
****************************************************************************************************
cd "C:\Users\silviap\Dropbox\Diversity_and_performance\0 REVISION JEBO\files_replication"
use "UNILATERAL_replication.dta", clear
****************************************************************************************************
global instrument IV_France
global DEPVAR Score_ch

xtset ID yr, delta(2)

******************************************************************************************************
*SUMMARY STATISTICS, LIST OF COUNTRIES BY YEAR
******************************************************************************************************
*re-label variables for the table
label variable dist2herf_size "Diversity" 
label variable dist2herf_w_size "Diversity, SW" 
label variable dist2herfAPP_size "Diversity, appearance"
label variable App_Min "Stand. dev. appearances" 
label variable age_dev "Stand. dev. squad age" 
label variable age "Squad age" 
label variable age_squared "Squad age, squared" 

label variable Str_faced  "Adversary's strength" 
label variable Div_faced  "Adversary's diversity" 

label variable IV_France "IV, 18y lag"
label variable Score_ch "Elo score changes, computed" 
label variable changeSCORE "Elo score changes" 

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
label variable  bestcoach  "Coach award"
label variable  past_exp_coach  "Coach tenure"

rename foreign_coach Foreign_coach
*************************************************************************************************************
*Add value labels for the year, so you can run latab, it gives tabulate output for LATEX
label define years 1970 "1970" 1972 "1972" 1974 "1974" 1976 "1976" 1978 "1978" 1980 "1980" 1982 "1982" ///
1984 "1984" 1986 "1986"  1988 "1988"  1990 "1990"  1992  "1992" 1994  "1994" 1996  "1996" 1998  "1998" 2000 "2000"  2002 "2002" ///
2004  "2002" 2006 "2006" 2008 "2008"  2010 "2010"  2012 "2012"  2014 "2014"  2016 "2016"  2018 "2018" 
label values yr years
 

*************************************************************************************************************
pwcorr Score Score1

*standardize IV
sum IV_France
replace IV_France = (IV_France-`r(mean)') / `r(sd)'


eststo clear
eststo summstats: estpost summarize Score_ch Score1 Score changeSCORE dist2herf_size dist2herfAPP_size ///
dist2herf_w_size Div_faced Str_faced Foreign_coach Age_approx_coach bestcoach App_Min age* team_size limmig ///
lgdpcap_UN pop_o $instrument  
 
 
esttab summstats , cells("mean sd count min max") noobs  nonumbers nomtitle nogaps compress  label ///
refcat(Score "\textbf{\emph{Performance measures}}" ///
dist2herf_size "\textbf{\emph{Diversity measures}}" Div_faced "\textbf{\emph{Team level variables}}" ///
limmig "\textbf{\emph{Macroeconomic variables}}" $instrument "IV", nolabel) ///
collabels(Mean "Standard Deviation" N Min Max)

******************************************************************************************************
*table OD2, summary statistics, unilateral
esttab summstats using "table_OD2.tex",replace nonumbers ///
cells("mean(fmt(3)) sd count(fmt(g)) min max") noobs label  nomtitle nogaps compress  ///
refcat(Score "\textbf{\emph{Performance measures}}" ///
dist2herf_size "\textbf{\emph{Diversity measures}}" Div_faced "\textbf{\emph{Team level variables}}" ///
limmig "\textbf{\emph{Macroeconomic variables}}" $instrument "\textbf{\emph{IV}}", nolabel) ///
collabels(Mean "Standard Deviation" N Min Max) 
******************************************************************************************************

*quadratic effect 
ge div2 = dist2herf_size*dist2herf_size
ge div2inst = IV_France*IV_France
 
forvalues i = 1970(2)2018 {
generate yr`i' =1*(yr==`i')
}

g res=0

****************************************************
*CONTROLS
drop if regexm(Team, "Gibraltar|Faroe")
global controls1 i.yr age age_dev age_squared 
global controls2 i.yr age age_dev age_squared App_Min 
global controls3 i.yr age age_dev age_squared lgdpcap_UN pop_o App_Min 
global controls4 yr1978-yr2016 age age_dev age_squared lgdpcap_UN limmig  App_Min  

global DIVER  dist2herf_size


****************************************************
*table 5, unilateral
****************************************************
eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
capture drop res
qui eststo P`ck': xi:reg   $DEPVAR $DIVER  $`ck' i.finals i.Team if !missing($instrument), cluster(Team)  robust 
predict res, residuals
xtcd2 res
estadd scalar Pesaran=r(p):  P`ck'
estadd local TeamFE "Yes": P`ck'
estadd local YrFE "Yes":  P`ck'
estadd local Age "Yes": P`ck'

}

forvalues k = 1/4 {
capture drop res
local ck "controls`k'"
qui eststo IV`ck'_app: xi:xtivreg2   $DEPVAR  $`ck' ($DIVER = $instrument)  , fe bw(2) robust savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p) :IV`ck'_app
estadd local TeamFE "Yes" :IV`ck'_app
estadd local YrFE "Yes" :IV`ck'_app
estadd local Age "Yes" :IV`ck'_app

}


esttab ,replace  se(%9.3f) b(%9.3f)  drop(_Iy* _IT* _If* yr* age* _cons)   star(* .10 ** .05 *** .001)   label  ///
stats(N idp widstat TeamFE YrFE Age Pesaran, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f  %~12s %~12s %~12s) )  mgroups("OLS" "Quantile", pattern(1 0 0 0 1)) nonotes ///
mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV" "IV") refcat(App_Min "\textbf{\emph{Control variables}}"   $DIVER "\textbf{\emph{Variable of interest}}",nolabel)


esttab using "table_5.tex", replace  se(%9.3f) b(%9.3f)  drop(_Iy* _cons _If* _IT* yr*  age*) ///
stats(N idp widstat  TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls")   ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s))  mgroups("\textbf{Dependent variable:} change in rating of national football team (Elo score)", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) ///
booktabs mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV" "IV") ///
refcat(App_Min "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel)   star(* .10 ** .05 *** .001) nonotes label
*************************************************************************************************
*1st STAGE
eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
qui eststo : xi:xtivreg2  $DEPVAR   $`ck' ($DIVER = $instrument) , bw(2)  fe robust savefirst savefprefix(f`k')
estadd local TeamFE "Yes": f`k'dist2herf_size
estadd local YrFE "Yes": f`k'dist2herf_size
estadd local Age "Yes": f`k'dist2herf_size
estadd scalar id `e(idp)': f`k'dist2herf_size
estadd scalar wid `e(widstat)': f`k'dist2herf_size

}

esttab  f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size   ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV") 

esttab f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size  using "table_5_first.tex" ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) refcat(App_Min "\textbf{\emph{Control variables}}"  $instrument "IV", nolabel) ///
mtitles("IV" "IV" "IV" "IV") mgroups("\textbf{First stage:} Dependent variable: team's diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 ) span ) nonotes








************************************************************
*table OD3, Score levels
************************************************************
forvalues k = 1/4 {
local ck "controls`k'"
capture drop res
qui eststo P`ck': xi:reg  Score1 Score0 $DIVER  $`ck' i.Team if !missing($instrument),  robust cluster(Team)
predict res, residuals
xtcd2 res
estadd scalar Pesaran=r(p): P`ck'
estadd local TeamFE "Yes": P`ck'
estadd local YrFE "Yes": P`ck'
estadd local Age "Yes": P`ck'
}
forvalues k = 1/4 {
capture drop res
local ck "controls`k'"
qui eststo IV`ck': xi:xtivreg2  Score1 Score0 $`ck' ($DIVER = $instrument), fe robust savefirst  bw(2)
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV`ck'
estadd local TeamFE "Yes": IV`ck'
estadd local YrFE "Yes":  IV`ck'
estadd local Age "Yes":  IV`ck'


}

esttab , drop (_Iy*  yr* _IT*)   se(%9.3f) b(%9.3f)  star(* .10 ** .05 *** .001) compress  stats(idp widstat N Pesaran)  mtitles("OLS" "OLS" "OLS" "OLS"  "IV" "IV" "IV" "IV") 

esttab using "table_OD3.tex", replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* _IT*  age* _cons) order($DIVER)  ///
stats(N idp widstat  TeamFE YrFE Age , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s))   mgroups("\textbf{Dependent variable:} Ending rating of national football team (Elo score)", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) nonotes ///
booktabs mtitles("OLS" "OLS" "OLS" "OLS"  "IV" "IV" "IV" "IV")    star(* .10 ** .05 *** .001)  label ///
refcat(Score0 "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel) 


*************************************************************************************************
*1st STAGE
eststo clear 
forvalues k = 1/4 {
local ck "controls`k'"
qui eststo : xi:xtivreg2   $DEPVAR  $`ck' ($DIVER = $instrument) , bw(2)  fe robust savefirst savefprefix(f`k')
estadd local TeamFE "Yes": f`k'dist2herf_size
estadd local YrFE "Yes": f`k'dist2herf_size
estadd local Age "Yes": f`k'dist2herf_size
estadd scalar wid `e(widstat)': f`k'dist2herf_size
estadd scalar id `e(idp)': f`k'dist2herf_size

}

esttab  f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV") 

esttab f1dist2herf_size f3dist2herf_size f3dist2herf_size f4dist2herf_size  using "table_OD3_first.tex" ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) refcat(lgdpcap_UN "\textbf{\emph{Control variables}}"  $instrument "IV", nolabel) ///
mtitles("IV" "IV" "IV" "IV")  mgroups("\textbf{First stage:} Dependent variable: team's diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0  ) span ) nonotes







*************************************************************
*table OD5, alternative measures
*************************************************************
eststo clear

forvalues k = 1/4 {
capture drop res
local ck "controls`k'"
qui eststo IV`ck'_app: xi:xtivreg2   $DEPVAR  $`ck' ( dist2herfAPP_size  =$instrument), bw(2)  fe robust savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV`ck'_app
estadd local TeamFE "Yes": IV`ck'_app
estadd local YrFE "Yes":  IV`ck'_app
estadd local Age "Yes":  IV`ck'_app
}

capture drop res
qui eststo IV_w: xi:xtivreg2   $DEPVAR (dist2herf_w_size  = $instrument )  $controls3 , bw(2)  fe robust first savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV_w
estadd local TeamFE "Yes": IV_w
estadd local YrFE "Yes":  IV_w
estadd local Age "Yes":  IV_w

capture drop res
qui eststo IV20: xtivreg2   $DEPVAR ($DIVER  =l.$instrument ) yr1974-yr2018 age age_dev age_squared lgdpcap_UN pop_o App_Min , bw(2)  fe robust first savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV20
estadd local TeamFE "Yes": IV20
estadd local YrFE "Yes":  IV20
estadd local Age "Yes":  IV20


capture drop res
qui eststo IV22: xi:xtivreg2   $DEPVAR ($DIVER  =l2.$instrument )   yr1976-yr2018 age age_dev age_squared lgdpcap_UN pop_o App_Min  , bw(2)  fe robust first savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV22
estadd local TeamFE "Yes": IV22
estadd local YrFE "Yes":  IV22
estadd local Age "Yes":  IV22



********************
esttab ,replace  se(%9.3f) b(%9.3f)  drop( *yr*  age*) ///
stats(N idp widstat  TeamFE YrFE Age , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls")  ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s)) mtitles("IV: Diversity, appearance" "IV: Diversity, appearance" "IV: Diversity, appearance" "IV: Diversity, appearance" ///
 "IV: Diversity, SW" "IV: 20 years lag" "IV: 22 years lag")    star(* .10 ** .05 *** .001)  label ///
refcat(App_Min "\textbf{\emph{Control variables}}" dist2herfAPP_size "\textbf{\emph{Variable of interest}}", nolabel) 


*****************
local addline "\shortstack{\textbf{IV:"
esttab using "table_OD5.tex", replace  se(%9.3f) b(%9.3f)  drop( *yr*  age*) order(dist2herfAPP_size dist2herf_w_size $DIVER) ///
stats(N idp widstat  TeamFE YrFE Age , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"   "Team FE" "Year FE" "Age controls")  ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s)) ///
booktabs mtitles("`addline'}Diversity,\\ appearance}" "`addline'}Diversity,\\ appearance}" "`addline'}Diversity,\\ appearance}" ///
 "`addline'}Diversity,\\ appearance}" "`addline'}Diversity,\\ SW}" "`addline'}\\ 20 years lag}" "`addline'}\\ 22 years lag}")    star(* .10 ** .05 *** .001)  label ///
refcat(App_Min "\textbf{\emph{Control variables}}"  dist2herfAPP_size "\textbf{\emph{Variable of interest}}", nolabel)  ///
  mgroups("\textbf{Dependent variable:} change in rating of national football team (Elo score)", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0) span ) nonotes

*************************************************************************************************
*1st STAGE
eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
qui eststo IV`ck'_app: xi:xtivreg2   $DEPVAR  $`ck' ( dist2herfAPP_size  =$instrument) , bw(2)  fe robust savefirst savefprefix(f`k')
estadd local TeamFE "Yes": f`k'dist2herfAPP_size
estadd local YrFE "Yes": f`k'dist2herfAPP_size
estadd local Age "Yes": f`k'dist2herfAPP_size
estadd scalar wid `e(widstat)': f`k'dist2herfAPP_size
estadd scalar id `e(idp)': f`k'dist2herfAPP_size
}

qui eststo IV_w: xi:xtivreg2   $DEPVAR (dist2herf_w_size  = $instrument )  $controls3 , bw(2)  fe robust savefirst savefprefix(f)
estadd local TeamFE "Yes": fdist2herf_w_size
estadd local YrFE "Yes": fdist2herf_w_size
estadd local Age "Yes": fdist2herf_w_size
estadd scalar wid `e(widstat)': fdist2herf_w_size
estadd scalar id `e(idp)': fdist2herf_w_size

qui eststo IV20: xi:xtivreg2   $DEPVAR ($DIVER  =l.$instrument)   yr1974-yr2018 age age_dev age_squared lgdpcap_UN pop_o App_Min  , bw(2)  fe robust savefirst savefprefix(f20)
estadd local TeamFE "Yes": f20dist2herf_size
estadd local YrFE "Yes": f20dist2herf_size
estadd local Age "Yes": f20dist2herf_size
estadd scalar wid `e(widstat)': f20dist2herf_size
estadd scalar id `e(idp)': f20dist2herf_size

qui eststo IV22: xi:xtivreg2   $DEPVAR ($DIVER  =l2.$instrument)   yr1976-yr2018 age age_dev age_squared lgdpcap_UN pop_o App_Min  , bw(2)  fe robust savefirst savefprefix(f22)
estadd local TeamFE "Yes": f22dist2herf_size
estadd local YrFE "Yes": f22dist2herf_size
estadd local Age "Yes": f22dist2herf_size
estadd scalar wid `e(widstat)': f22dist2herf_size
estadd scalar id `e(idp)': f22dist2herf_size

esttab f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size fdist2herf_w_size f20dist2herf_size f22dist2herf_size ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label order(*$instrument* ) ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV") 

esttab f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size fdist2herf_w_size f20dist2herf_size f22dist2herf_size  using "table_OD5_first.tex" ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001) order(*$instrument* )  label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) refcat(Div_faced "\textbf{\emph{Control variables}}"  $instrument "IV", nolabel) ///
mtitles("IV" "IV" "IV" "IV"  "IV" "IV" "IV")  mgroups("\textbf{First stage:} Dependent variable: team's diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 ) span ) nonotes







************************************************************************table OD 6, coach information
***********************************************************************


eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
capture drop res
qui eststo P`ck': xi:reg   $DEPVAR $DIVER   Age_approx_coach past_exp_coach bestcoach i.Foreign_coach $`ck' i.finals i.Team if !missing($instrument),  robust  cluster(Team)
predict res, residuals
xtcd2 res
estadd scalar Pesaran=r(p): P`ck'
estadd local TeamFE "Yes": P`ck'
estadd local YrFE "Yes": P`ck'
estadd local Age "Yes": P`ck'
}

forvalues k = 1/4 {
capture drop res
local ck "controls`k'"
qui eststo IV`ck'_app: xi:xtivreg2   $DEPVAR  $`ck' ($DIVER = $instrument) Age_approx_coach past_exp_coach bestcoach i.Foreign_coach , bw(2)  fe robust savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV`ck'_app
estadd local TeamFE "Yes": IV`ck'_app
estadd local YrFE "Yes":  IV`ck'_app
estadd local Age "Yes":  IV`ck'_app
}

esttab ,replace se(%9.3f) b(%9.3f) drop(_Iy* _IT* _Ifi* yr* age* _cons)   star(* .10 ** .05 *** .001)   label  ///
stats(N idp widstat TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls")  ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV" "IV") 

esttab using "table_OD_6.tex", replace  se(%9.3f) b(%9.3f)  drop(_Iy* _cons _Ifi* _IT* yr*  age*) ///
stats(N idp widstat  TeamFE YrFE Age , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"  "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s))  booktabs mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV" "IV")    star(* .10 ** .05 *** .001)  label ///
refcat(Age_approx_coach "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel)  mgroups("\textbf{Dependent variable:} change in rating of national football team (Elo score)", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) nonotes
*************************************************************************************************
*1st STAGE
eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
qui eststo : xi:xtivreg2   $DEPVAR  $`ck' ($DIVER = $instrument) Age_approx_coach  i.Foreign_coach , bw(2)  fe robust savefirst savefprefix(f`k')
estadd local TeamFE "Yes": f`k'dist2herf_size
estadd local YrFE "Yes": f`k'dist2herf_size
estadd local Age "Yes": f`k'dist2herf_size
estadd scalar wid `e(widstat)': f`k'dist2herf_size
estadd scalar id `e(idp)': f`k'dist2herf_size

}

esttab  f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size  ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV") 

esttab  f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size  using "table_OD_6_first.tex" ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) refcat(Age_approx_coach "\textbf{\emph{Control variables}}"  $ $instrument "IV", nolabel) ///
mtitles("IV" "IV" "IV" "IV")  mgroups("\textbf{First stage:} Dependent variable: team's diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 ) span ) nonotes




****************************************************
*table OD7, alternative score change
****************************************************


eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
capture drop res
qui eststo P`ck': xi:reg  changeSCORE $DIVER  $`ck' i.finals i.Team if !missing($instrument),  robust  cluster(Team)
estadd local TeamFE "Yes":  P`ck'
estadd local YrFE "Yes":  P`ck'
estadd local Age "Yes":  P`ck'
predict res, residuals
xtcd2 res
estadd scalar Pesaran=r(p):  P`ck'

}

forvalues k = 1/4 {
capture drop res
local ck "controls`k'"
qui eststo IV`ck'_app: xi:xtivreg2 changeSCORE  $`ck' ($DIVER = $instrument)  , bw(2)  fe robust savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV`ck'_app
estadd local TeamFE "Yes": IV`ck'_app
estadd local YrFE "Yes":  IV`ck'_app
estadd local Age "Yes":  IV`ck'_app

}


esttab ,replace  se(%9.3f) b(%9.3f)  drop(_Iy* _IT* _If* yr* age* _cons)   star(* .10 ** .05 *** .001)   label  ///
stats(N idp widstat TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f  %~12s %~12s %~12s) )  ///
mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV" "IV") refcat(App_Min "\textbf{\emph{Control variables}}"   $DIVER "\textbf{\emph{Variable of interest}}",nolabel)


esttab using "table_OD7.tex", replace  se(%9.3f) b(%9.3f)  drop(_Iy* _cons _If* _IT* yr*  age*) ///
stats(N idp widstat  TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test"    "Team FE" "Year FE" "Age controls")   ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s))  mgroups("\textbf{Dependent variable:} change in rating of national football team (Elo score); scores from websource", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 ) span ) nonotes ///
booktabs mtitles("OLS" "OLS" "OLS" "OLS" "IV" "IV" "IV" "IV") ///
refcat(App_Min "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel)   star(* .10 ** .05 *** .001)  label
*************************************************************************************************
*1st STAGE
eststo clear
forvalues k = 1/4 {
local ck "controls`k'"
qui eststo : xi:xtivreg2  changeSCORE  $`ck' ($DIVER = $instrument) , bw(2)  fe robust savefirst savefprefix(f`k')
estadd local TeamFE "Yes": f`k'dist2herf_size
estadd local YrFE "Yes": f`k'dist2herf_size
estadd local Age "Yes": f`k'dist2herf_size
estadd scalar id `e(idp)': f`k'dist2herf_size
estadd scalar wid `e(widstat)': f`k'dist2herf_size

}

esttab  f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size   ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV") 

esttab f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size  using "table_OD7_first.tex" ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) refcat(App_Min "\textbf{\emph{Control variables}}"  $instrument "IV", nolabel) ///
mtitles("IV" "IV" "IV" "IV")  mgroups("\textbf{First stage:} dependent variable: team's diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 ) span ) nonotes
*************************************************************************************************

**********************************************************
*table OD4
**********************************************************

global controls1a i.yr age* 
global controls2a i.yr age* Div_faced pop_o App_Min lgdpcap_UN
global controls3a i.yr age* Str_faced pop_o App_Min lgdpcap_UN  
global controls4a yr1978-yr2016 age* Str_faced App_Min lgdpcap_UN  limmig   
global controls5a yr1978-yr2016 age* Str_faced App_Min Div_faced lgdpcap_UN  limmig   

eststo clear
forvalues k = 1/5 {
local ck "controls`k'a"
capture drop res
qui eststo P`ck': xi:reg   $DEPVAR $DIVER  $`ck' i.Team  i.finals if !missing($instrument) ,  robust  cluster(Team)
predict res, residuals
xtcd2 res
estadd scalar Pesaran=r(p): P`ck'
estadd local TeamFE "Yes": P`ck'
estadd local YrFE "Yes": P`ck'
estadd local Age "Yes": P`ck'
}
forvalues k = 1/5 {
capture drop res
local ck "controls`k'a"
qui eststo IV`ck': xi:xtivreg2   $DEPVAR $`ck' ($DIVER =$instrument) , bw(2) fe robust savefirst
predict res, e
xtcd2 res
estadd scalar Pesaran=r(p): IV`ck'
estadd local TeamFE "Yes": IV`ck'
estadd local YrFE "Yes":  IV`ck'
estadd local Age "Yes":  IV`ck'
}

esttab ,  se(%9.3f) b(%9.3f)  drop (_Iy* _IT* yr*)   star(* .10 ** .05 *** .001) compress  stats(idp widstat N Pesaran) 


esttab using "table_OD4.tex", replace  se(%9.3f) b(%9.3f)  drop(_Iy* _cons _Ifi* _IT* yr*  age*) ///
stats(N idp widstat TeamFE YrFE Age , labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %9.2f  %~12s %~12s %~12s)) booktabs mtitles("OLS" "OLS" "OLS" "OLS"  "OLS" "IV" "IV" "IV" "IV"  "IV")    star(* .10 ** .05 *** .001)  label ///
refcat(Div_faced "\textbf{\emph{Control variables}}"  $DIVER "\textbf{\emph{Variable of interest}}", nolabel)  mgroups("\textbf{Dependent variable:} change in rating of national football team (Elo score)", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0 0 0 0 0 0) span ) nonotes

*************************************************************************************************
*1st STAGE
eststo clear
forvalues k = 1/5 {
local ck "controls`k'a"
qui eststo : xi:xtivreg2   $DEPVAR  $`ck' ($DIVER = $instrument) , bw(2)  fe robust savefirst savefprefix(f`k')
estadd local TeamFE "Yes": f`k'dist2herf_size
estadd local YrFE "Yes": f`k'dist2herf_size
estadd local Age "Yes": f`k'dist2herf_size
estadd scalar wid `e(widstat)': f`k'dist2herf_size
estadd scalar id `e(idp)': f`k'dist2herf_size

}

esttab f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size f5dist2herf_size ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s))  ///
mtitles("IV" "IV" "IV" "IV") 

esttab f1dist2herf_size f2dist2herf_size f3dist2herf_size f4dist2herf_size f5dist2herf_size using "table_OD4first.tex" ,replace  se(%9.3f) b(%9.3f)  drop(_Iy*  yr* age* )   star(* .10 ** .05 *** .001)   label  ///
stats(N id wid TeamFE YrFE Age, labels(Observations "Kleibergen-Paap LM test" "Kleibergen-Paap F test" "Team FE" "Year FE" "Age controls") ///
fmt(0 %9.2f  %9.2f %~12s %~12s %~12s)) refcat(Div_faced "\textbf{\emph{Control variables}}"  $instrument "IV", nolabel) ///
mtitles("IV" "IV" "IV" "IV"  "IV")  mgroups("\textbf{First stage:} Dependent variable: team's diversity", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0 0) span ) nonotes
***********************************************************************


***********************************************************************
*Illustrate the effect marginally decreasing mis-specification
***********************************************************************

clear
set obs 2000

ge diversity = _n/2000

ge line2 = 0.9*log(0.6*diversity+0.2)+1.1
drop if line2<0
reg line2 diversity, robust
predict xb, xb

reg line2 diversity if line2 < .8, robust
predict xb_trun, xb

count if line2 < .8
disp  1406/_N
twoway line (xb xb_trun line2 diversity) if diversity>0.3 ,legend(pos(5) col(3) lab(1 "xb") lab(2 "xb truncated") lab(3 "real line")) ///
yline(0.8,lpattern(dash))  ylabel(none) xlabel(none) ytitle(Performance) graphregion(color(white)) xtitle(Diversity)
graph export "Diversity_misspecified_Score1.png", as(png) replace

**************************************************************************************


