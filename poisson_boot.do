
**************************************************************
cd "C:\Users\silviap\Dropbox\Diversity_and_performance\AUGUST2020"
***********************************************************
*Bootstrap estimates
*Fist specification
use "bstr1.dta" , clear
*specification nr
g reg_n=1

*append with other specifications, tracking regression nr.
*****************************
append using bstr2, gen(appd)

replace reg_n=2 if appd==1
drop appd

*****************************
append using bstr3, gen(appd)

replace reg_n=3 if appd==1
drop appd

*****************************
append using bstr4, gen(appd)

replace reg_n=4 if appd==1
drop appd

*****************************
*Trim estimates to avoid extreme values influencing results.
*****************************
foreach R in 1 2 3 4 {

foreach x in 1 2 3 4 {
global EST `x'

sum m$EST if reg_n == `R', detail
replace  m$EST= . if   (m$EST<`r(p10)' |  m$EST> `r(p90)') & reg_n == `R'
*eststo
}
}
**********************************************************************

label variable m1 "AMEs diversity away"
label variable m2 "AMEs diversity home"
label variable m3 "AMEs diversity away"
label variable m4 "AMEs diversity home"


**********************************************************************
foreach x in 1 2 3 4 {

*first coef, first reg
eststo M1: mean m`x'  if reg_n==1
disp e(b)[1,1]/e(sd)[1,1]
matrix  mypval = (2 * ttail(e(df_r), abs(e(b)[1,1]/e(sd)[1,1])))
matrix list mypval

mat colnames mypval= "`:colnames e(b)'"
estadd matrix mypval : M1

*first coef, second reg
eststo M2: mean m`x' if reg_n==2
matrix  mypval = (2 * ttail(e(df_r), abs(e(b)[1,1]/e(sd)[1,1])))
mat colnames mypval= "`:colnames e(b)'"
estadd matrix mypval : M2

*first coef, third reg
eststo M3: mean m`x' if reg_n==3
matrix  mypval = (2 * ttail(e(df_r), abs(e(b)[1,1]/e(sd)[1,1])))
mat colnames mypval= "`:colnames e(b)'"
estadd matrix mypval : M3

*first coef, fourth reg
eststo M4: mean m`x' if reg_n==4
matrix  mypval = (2 * ttail(e(df_r), abs(e(b)[1,1]/e(sd)[1,1])))
mat colnames mypval= "`:colnames e(b)'"
estadd matrix mypval : M4

esttab M1 M2 M3 M4 , label   cell(b( star pvalue(mypval) fmt(%9.3f)) sd(fmt(3)  par("(" ")")))   starlevel(* 0.10 ** 0.05 *** 0.01) noobs

if `x'==1 {
esttab M1 M2 M3 M4  using "table_A2.tex", replace label    cell(b( star pvalue(mypval) fmt(%9.3f)) sd(fmt(3)  par("(" ")")))   starlevel(* 0.10 ** 0.05 *** 0.01) noobs nonumbers collabels(none) fragment plain nomtitles 
}
*************************************************************
if `x' ==2 {
esttab M1 M2 M3 M4  using "table_A2.tex", append label    cell(b( star pvalue(mypval) fmt(%9.3f)) sd(fmt(3)  par("(" ")")))   starlevel(* 0.10 ** 0.05 *** 0.01) noobs nonumbers collabels(none) fragment plain prefoot(\hline)   nomtitles 
}
*************************************************************
if `x'==3 {
esttab M1 M2 M3 M4  using "table_A2.tex", append label    cell(b( star pvalue(mypval) fmt(%9.3f)) sd(fmt(3)  par("(" ")")))   starlevel(* 0.10 ** 0.05 *** 0.01) noobs nonumbers collabels(none) fragment  nomtitles mgroups("\textbf{Dependent variable}: away team's goals scored", ///
prefix(\multicolumn{@span}{c}{) suffix(}) pattern(1 0 0 0  ) span )
}
*************************************************************
if (  `x' ==4) {
esttab M1 M2 M3 M4  using "table_A2.tex", append label    cell(b( star pvalue(mypval) fmt(%9.3f)) sd(fmt(3)  par("(" ")")))   starlevel(* 0.10 ** 0.05 *** 0.01) noobs nonumbers collabels(none) fragment  plain nomtitles 
}
}

**************************************************************

