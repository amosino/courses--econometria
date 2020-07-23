***************************
*** heus_flex.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Exploring heterogeneity of effects


clear all
set more off
set scheme sj
set linesize 79



*** install user-written packages needed for this chapter 
*** if not already installed
capture ssc install parmest // saves estimated coefficients into Stata dataset
capture net install dm79.pkg // a suite of matrix utilities


****************************
*** Quantile regressions ***
****************************
set seed 123456
use heus_mepssample, clear
*** Restrict to subsample with positive total expenditures
drop if exp_tot <= 0
*** Quantile (median) regression of total expenditures
qreg exp_tot age i.female, quantile(0.5) vce(robust)

*** Least-squares regression of total expenditures
regress exp_tot age i.female, vce(robust)

*** Quantile regressions across quantiles and graphing results: Expenditures
preserve
tempfile sqreg ols
sqreg exp_tot age i.female, quantile(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9) ///
	reps(50)
parmest, saving(`sqreg')

regress exp_tot age i.female, vce(bootstrap, reps(50))
parmest, saving(`ols')

use `ols', clear
keep estimate min95 max95 parm
rename estimate estimate_ols
rename min95 min95_ols
rename max95 max95_ols

merge 1:m parm using `sqreg'
generate quantile=substr(eq,2,2)
destring quantile, replace
sort parm quantile

twoway (rarea min95 max95 quantile if parm=="age", color(gs14)) ///
       (line estimate quantile if parm=="age", lpattern(solid)) ///
       (rline min95_ols max95_ols quantile if parm=="age", /// 
	   lpattern(shortdash) lcolor(gs10)) ///
       (line estimate_ols quantile if parm=="age", lpattern(dash) ///
	   lcolor(gs6)), ///
       ylabel(,nogrid) xlabel(10(10)90) ytitle(Coefficient) subtitle(Age) ///
	   legend(off) name(g1,replace) nodraw

twoway (rarea min95 max95 quantile if parm=="1.female", color(gs14)) ///
       (line estimate quantile if parm=="1.female", lpattern(solid)) ///
       (rline min95_ols max95_ols quantile if parm=="1.female", ///
	   lpattern(shortdash) lcolor(gs10)) ///
       (line estimate_ols quantile if parm=="1.female", lpattern(dash) ///
	   lcolor(gs6)), ///
       ylabel(,nogrid) xlabel(10(10)90) ytitle(Coefficient) ///
	   subtitle(Female) legend(off) name(g2,replace) nodraw

graph combine g1 g2, rows(1) xsize(5) ysize(2.5) ///
    note("Quantile regression estimates and 95% CIs denoted by solid lines " ///
		 "and shaded areas, respectively" ///
      "OLS estimate and 95% CI denoted by dashed and dotted lines, respectively")
restore

*** Generate ln(total expenditures)
generate ln_exp_tot = ln(exp_tot)
*** Quantile (median) regression of ln(expenditures)
qreg ln_exp_tot age i.female, quantile(0.5) vce(robust)

*** Quantile regressions across quantiles and graphing results: ln(expenditures)
preserve
tempfile sqreg ols

sqreg ln_exp_tot age i.female, quantile(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9) ///
	reps(50)
parmest, saving(`sqreg')

regress ln_exp_tot age i.female, vce(bootstrap, reps(50))
parmest, saving(`ols')

use `ols', clear
keep estimate min95 max95 parm
rename estimate estimate_ols
rename min95 min95_ols
rename max95 max95_ols

merge 1:m parm using `sqreg'
generate quantile=substr(eq,2,2)
destring quantile, replace
sort parm quantile

twoway (rarea min95 max95 quantile if parm=="age", color(gs14)) ///
       (line estimate quantile if parm=="age", lpattern(solid)) ///
       (rline min95_ols max95_ols quantile if parm=="age", ///
	   lpattern(shortdash) lcolor(gs10)) (line estimate_ols quantile ///
	   if parm=="age", lpattern(dash) lcolor(gs6)), ///
       ylabel(,nogrid) xlabel(10(10)90) ytitle(Coefficient) subtitle(Age) ///
	   legend(off) name(g1,replace) nodraw

twoway (rarea min95 max95 quantile if parm=="1.female", color(gs14)) ///
       (line estimate quantile if parm=="1.female", lpattern(solid)) ///
       (rline min95_ols max95_ols quantile if parm=="1.female", ///
	   lpattern(shortdash) lcolor(gs10)) (line estimate_ols quantile ///
	   if parm=="1.female", lpattern(dash) lcolor(gs6)), ///
       ylabel(,nogrid) xlabel(10(10)90) ytitle(Coefficient) subtitle(Female) ///
	   legend(off) name(g2,replace) nodraw

graph combine g1 g2, rows(1) xsize(5) ysize(2.5) ///
    note("Quantile regression estimates and 95% CIs denoted by solid lines " ///
		"and shaded areas, respectively" ///
      "OLS estimate and 95% CI denoted by dashed and dotted lines, respectively")
restore

*** Partial effects on raw scale after quantile regression of ln(expenditures)
quietly qreg ln_exp_tot age i.female, quantile(0.5) vce(robust)
margins, dydx(age female) expression(exp(predict(xb)))


*****************************
*** Finite mixture models ***
*****************************
set seed 123456
use heus_mepssample, clear
*** Restrict to subsample with positive total expenditures
drop if exp_tot <= 0

*** Two-component FMM parameter estimates: Total expenditures
fmm 2,  vce(robust): glm exp_tot age i.female, family(gamma) link(log)
estimates store fmm2

*** Two-component FMM class probabilities: Total expenditures
estat lcprob

*** Three-component FMM parameter estimates: Total expenditures
fmm 3,  vce(robust): glm exp_tot age i.female, family(gamma) link(log)
estimates store fmm3

*** Three-component FMM class probabilities: Total expenditures
estat lcprob

*** Three-component FMM class probabilities: Total expenditures
local denominator "(1+exp(_b[2.Class:_cons])+exp(_b[3.Class:_cons]))"
nlcom (pi1: 1/`denominator') ///
	 (pi2: exp(_b[2.Class:_cons])/`denominator') ///
	 (pi3: exp(_b[3.Class:_cons])/`denominator')

*** saving probabilities in local macros for later
local pi1e : di %4.2f `=el(r(b),1,1)'
local pi2e : di %4.2f `=el(r(b),1,2)'
local pi3e : di %4.2f `=el(r(b),1,3)'

*** FMM AIC and BIC: total expenditures
estimates stats fmm2 fmm3

*** Three-component FMM predicted and empirical means: Total expenditures
margins
summarize exp_tot

*** Three-component FMM component-level predicted means: Total expenditures
estat lcmean

*** saving class means in local macros for later
local ybar1 : di %5.1f `=el(r(b),1,1)'
local ybar2 : di %5.1f `=el(r(b),1,2)'
local ybar3 : di %5.1f `=el(r(b),1,3)'

*** Three-component FMM component densities
preserve
collapse (median) age female exp_tot
expand 498
replace exp_tot = (_n-1)*40 + 1
predict density*, density
twoway (line density1 exp_tot) ///
	(line density2 exp_tot) ///
	(line density3 exp_tot), ///
	ytitle("") ylabel(none) xtitle(expenditure) ///
	legend(label(1 "f(y|x,c=1)") label(2 "f(y|x,c=2)") label(3 "f(y|x,c=3)") ///
		row(1) symxsize(*.5) size(medsmall)) ///
	note("{&mu}{sub:1} = `ybar1', {&pi}{sub:1} = `pi1e'" ///
	     "{&mu}{sub:2} = `ybar2', {&pi}{sub:2} = `pi2e'" ///
	     "{&mu}{sub:3} = `ybar3', {&pi}{sub:3} = `pi3e'", size(medium))

restore

*** Three-component FMM component-level partial effects: Total expenditures
preserve
margins, dydx(age female) predict(mu class(1))
matrix r1=r(table)'
matselrc r1 r1, row(age 1.female) col(b ll ul)
matrix c = J(2,1,1)
matrix r1 = r1,c

margins, dydx(age female) predict(mu class(2))
matrix r2=r(table)'
matselrc r2 r2, row(age 1.female) col(b ll ul)
matrix c = J(2,1,2)
matrix r2 = r2,c

margins, dydx(age female) predict(mu class(3))
matrix r3=r(table)'
matselrc r3 r3, row(age 1.female) col(b ll ul)
matrix c = J(2,1,3)
matrix r3 = r3,c

matrix r = r1\r2\r3
matrix l r

svmat2 r, names(col) rnames(var)
sort var c1

twoway (bar b c1 if var=="age", barwidth(.8) color(gs14)) ///
       (rcap ll ul c1 if var=="age", color(gs6)), ///
       ylabel(,nogrid) ytitle(Coefficient) xlabel(1(1)3) ///
	   xtitle(Component) subtitle(Age) legend(off) ///
       name(g1,replace) nodraw

twoway (bar b c1 if var=="1.female", barwidth(.8) color(gs14)) ///
       (rcap ll ul c1 if var=="1.female", color(gs6)), ///
       ylabel(,nogrid) ytitle(Coefficient) xlabel(1(1)3) ///
	   xtitle(Component) subtitle(Female) legend(off) ///
       name(g2,replace) nodraw

graph combine g1 g2, rows(1) xsize(5) ysize(2.5) ///
    note("Finite mixture estimates and 95% CI denoted by bars and capped lines, respectively")
restore

*** Three-component FMM posterior probabilities: Total expenditures
predict postpr1, classposteriorpr class(1)
predict postpr2, classposteriorpr class(2)
predict postpr3, classposteriorpr class(3)

summarize postpr*

*** Three-component FMM posterior classification: Total expenditures
generate byte class = 3
replace class = 1 if postpr1>postpr2 & postpr1>postpr3
replace class = 2 if postpr2>postpr1 & postpr2>postpr3

tabstat age female, by(class)
regress age i.class
test 2.class=3.class
regress female i.class
test 2.class=3.class


*** FMM for office-based visits
use heus_mepssample, clear

*** Two-component FMM parameter estimates: # office-based visits
fmm 2, vce(robust): nbreg use_off age i.female, dispersion(constant)
estimates store fmm2

*** Three-component FMM parameter estimates: # office-based visits
fmm 3, vce(robust) difficult: nbreg use_off age i.female, dispersion(constant)
estimates store fmm3

*** Three-component FMM class probabilities: Total expenditures
local denominator "(1+exp(_b[2.Class:_cons])+exp(_b[3.Class:_cons]))"
nlcom (pi1: 1/`denominator') ///
	 (pi2: exp(_b[2.Class:_cons])/`denominator') ///
	 (pi3: exp(_b[3.Class:_cons])/`denominator')

*** Saving probabilities in local macros for later
local pi1e : di %4.2f `=el(r(b),1,1)'
local pi2e : di %4.2f `=el(r(b),1,2)'
local pi3e : di %4.2f `=el(r(b),1,3)'


*** Three-component FMM AIC and BIC: # office-based visits
estimates stats fmm2 fmm3

*** Three-component FMM component-level predicted means: # office-based visits
estat lcmean

*** Saving class means in local macros for later
local ybar1 : di %5.1f `=el(r(b),1,1)'
local ybar2 : di %5.1f `=el(r(b),1,2)'
local ybar3 : di %5.1f `=el(r(b),1,3)'

*** Three-component FMM component densities and empirical density
preserve
collapse (median) age female use_off
expand 30
replace use_off = _n-1
predict density*, density
twoway (line density1 use_off) ///
	(line density2 use_off) ///
	(line density3 use_off), ///
	ytitle("") ylabel(none) xtitle(# office-based visits) ///
	legend(label(1 "f(y|x,c=1)") label(2 "f(y|x,c=2)") label(3 "f(y|x,c=3)") ///
		row(1) symxsize(*.5) size(medsmall)) ///
	note("{&mu}{sub:1} = `ybar1', {&pi}{sub:1} = `pi1e'" ///
	     "{&mu}{sub:2} = `ybar2', {&pi}{sub:2} = `pi2e'" ///
	     "{&mu}{sub:3} = `ybar3', {&pi}{sub:3} = `pi3e'", size(medium))

restore


*** Three-component FMM component-level partial effects: # office-based visits
preserve
margins, dydx(age female) predict(mu class(1))
matrix r1=r(table)'
matselrc r1 r1, row(age 1.female) col(b ll ul)
matrix c = J(2,1,2)
matrix r1 = r1,c

margins, dydx(age female) predict(mu class(2))
matrix r2=r(table)'
matselrc r2 r2, row(age 1.female) col(b ll ul)
matrix c = J(2,1,3)
matrix r2 = r2,c

margins, dydx(age female) predict(mu class(3))
matrix r3=r(table)'
matselrc r3 r3, row(age 1.female) col(b ll ul)
matrix c = J(2,1,1)
matrix r3 = r3,c

matrix r = r1\r2\r3
matrix l r

svmat2 r, names(col) rnames(var)
sort var c1

twoway (bar b c1 if var=="age", barwidth(.8) color(gs14)) ///
       (rcap ll ul c1 if var=="age", color(gs6)), ///
       ylabel(,nogrid) ytitle(Coefficient) xlabel(1(1)3) ///
	   xtitle(Component) subtitle(Age) legend(off) ///
       name(g1,replace) nodraw

twoway (bar b c1 if var=="1.female", barwidth(.8) color(gs14)) ///
       (rcap ll ul c1 if var=="1.female", color(gs6)), ///
       ylabel(,nogrid) ytitle(Coefficient) xlabel(1(1)3) ///
	   xtitle(Component) subtitle(Female) legend(off) ///
       name(g2,replace) nodraw

graph combine g1 g2, rows(1) xsize(5) ysize(2.5) ///
    note("Finite mixture estimates and 95% CI denoted by bars and capped lines, respectively")
restore


****************************
*** Nonparametric regression
****************************
set seed 123456
use heus_mepssample, clear

*** Restrict to subsample with positive total expenditures
*** Restrict to sample of Hispanic males
drop if exp_tot <= 0
keep if female==0 & eth_hisp==1
regress exp_tot age pcs i.anylim

timer on 1
npregress kernel exp_tot age pcs i.anylim, reps(100)
timer off 1

timer on 2
margins, at(pcs=(30(5)60) anylim=(0 1)) reps(100)
marginsplot, title(Predicted values by pcs and anylim)
timer off 2


timer list

exit
