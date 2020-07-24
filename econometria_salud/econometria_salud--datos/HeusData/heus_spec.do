***************************
*** heus_spec.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Model Specification and Checks


clear all
set more off
set scheme sj
set linesize 79



*** MEPS data
use heus_mepssample
*** Restrict to subsample with positive total expenditures
drop if exp_tot <= 0

*** Regression to predict total expenditures
regress exp_tot c.age##i.female i.anylim, vce(robust)

*** Average marginal effects for age and female
margins, dydx(age female)

*** Marginal effect of age by gender
margins, dydx(age) at(female=(0 1))

*** Marginal effects of female by age
margins, dydx(female) at(age=(20 45 70))

*** Margins (predictions) showing importance of interaction term
margins, at(age=(20(30)80) female=(0 1) anylim=(0 1))
marginsplot


*** Margins for any limitations with sample-average standard errors
margins i.anylim

*** Treatment effect for any limitations with sample-average standard errors
margins r.anylim, contrast(nowald)


*** Treatment effects ATE for any limitations
teffects ra (exp_tot c.age##i.female) (anylim), ate

*** OLS regression with interaction between anylim and covariates
regress exp_tot (c.age##i.female)##i.anylim, vce(robust)
estimates store SPEC_me_ate

*** Margins for any limitations with population-averaged standard errors
margins i.anylim, vce(unconditional)            	        

*** Treatment effect for any limitations with population-averaged standard errors
margins r.anylim, contrast(nowald) vce(unconditional)


*** Treatment effects ATET for any limitations
teffects ra (exp_tot c.age##i.female) (anylim), atet

*** Treatment effects POMEANS for any limitations
teffects ra (exp_tot c.age##i.female) (anylim), pomeans



************************************
*** Consequences of misspecification
************************************

clear all
set more off
set scheme sj


*** Generate simulation data with quadratic specification
clear
set seed 123456
generate x = 2 + 6*runiform()
generate z = 2 + 6*rbeta(2,4)
generate u = 3*rnormal()
generate y = 1000 - 2*x + 0.3*x^2  - 2*z + 0.3*z^2 + u

drop x u y

*** Generate simulation data with exponential specification
clear
set seed 123456
generate x = 2 + 6*runiform()
generate d = runiform()>0.5
generate u = 1.5*rnormal()
generate ylog = 2 + .5*x + .5*d + u
generate y = exp(ylog)



clear all
set more off
set scheme sj


*** program to simulate a quadratic mean specification
program quadraticexample, rclass
	syntax [, OBs(integer 100)]

	clear
	set obs `obs'

	generate x = 2 + 6*runiform()
	generate z = 2 + 6*rbeta(2,4)
	generate u = 3*rnormal()

	generate y = 1000 - 2*x + 0.3*x^2  - 2*z + 0.3*z^2 + u

	regress y x z
	scalar bl_x = _b[x]
	scalar bl_z = _b[z]

	regress y c.x##c.x c.z##c.z
	estimates store quad

	margins, dydx(x z) nose post
	scalar b_x = _b[x]
	scalar b_z = _b[z]

	estimates restore quad
	margins, dydx(x z) at(x=6) nose post
	scalar b6_x = _b[x]
	scalar b6_z = _b[z]

	return scalar bl_x = bl_x
	return scalar bl_z = bl_z
	return scalar b_x = b_x
	return scalar b_z = b_z
	return scalar b6_x = b6_x
	return scalar b6_z = b6_z

end


*** program to simulate an exponential mean specification
program exponentialexample, rclass
	syntax [, OBs(integer 100)]

	clear
	set obs `obs'

	generate x = 2 + 6*runiform()
	generate d = runiform()>0.5
	generate u = 1.5*rnormal()

	generate ylog = 2 + .5*x + .5*d + u
	generate y = exp(ylog)


	regress y x d
	scalar bl_x = _b[x]
	scalar bl_d = _b[d]

	regress ylog x d
	estimates store expo

	margins, expression(_b[x]*exp(predict(xb) + 0.5*`e(rmse)'^2)) nose post
	scalar b_x = _b[_cons]
	estimates restore expo
	margins, expression(_b[d]*exp(predict(xb) + 0.5*`e(rmse)'^2)) nose post
	scalar b_d = _b[_cons]
	estimates restore expo
	margins, expression(_b[d]*exp(predict(xb) + 0.5*`e(rmse)'^2)) nose post at(d=1)
	scalar b1_d = _b[_cons]

	return scalar bl_x = bl_x
	return scalar bl_d = bl_d
	return scalar b_x = b_x
	return scalar b_d = b_d
	return scalar b1_d = b1_d

end


*** simulate "true" marginal effects for quadratic mean example
set seed 123456
quadraticexample, obs(1000000)
scalar tme_x = b_x
scalar tme_z = b_z

scalar tme6_x = b6_x
scalar tme6_z = b6_z


*** simulate quadratic mean example: Sample size 10,000
clear
tempfile mcdata
set seed 123456
simulate bl_x = r(bl_x) bl_z = r(bl_z) b_x = r(b_x) b_z = r(b_z) ///
	 b6_x = r(b6_x) b6_z = r(b6_z), reps(500): quadraticexample, obs(10000)

*** summarize/graph results of quadratic mean example: Sample size 10,000
generate dev_blx = (bl_x - tme_x)
generate dev_blz = (bl_z - tme_z)
generate dev_bx = (b_x - tme_x)
generate dev_bz = (b_z - tme_z)
generate dev_bl6x = (bl_x - tme6_x)
generate dev_bl6z = (bl_z - tme6_z)
generate dev_b6x = (b6_x - tme6_x)
generate dev_b6z = (b6_z - tme6_z)

twoway (kdensity dev_bx) (kdensity dev_blx), xline(0) ylabel(, nogrid) ///
	subtitle("Average marginal effect of X") xtitle("deviation") ///
	legend(label(1 "Correctly specified: OLS quadratic in X & Z") ///
            label(2 "Misspecified: OLS linear in X & Z")  ///
        symxsize(*.5) size(small) row(2))


twoway (kdensity dev_bz) (kdensity dev_blz), xline(0) ylabel(, nogrid) ///
	subtitle("Average marginal effect of Z")  xtitle("deviation") ///
	legend(label(1 "Correctly specified: OLS quadratic in X & Z") ///
            label(2 "Misspecified: OLS linear in X & Z")  ///
        symxsize(*.5) size(small) row(2))


twoway (kdensity dev_b6x) (kdensity dev_bl6x), xline(0) ylabel(, nogrid) ///
	subtitle("Marginal effect of X at X=6") xtitle("deviation") ///
	legend(label(1 "Correctly specified: OLS quadratic in X & Z") ///
            label(2 "Misspecified: OLS linear in X & Z")  ///
        symxsize(*.5) size(small) row(2))


*** simulate "true" marginal effects for quadratic mean example
set seed 123456
exponentialexample, obs(1000000)
scalar tme_x = b_x
scalar tme_d = b_d

scalar tme1_d = b1_d


*** simulate exponential mean example: Sample size 10,000
clear
tempfile mcdata
set seed 123456
simulate bl_x = r(bl_x) bl_d = r(bl_d) b_x = r(b_x) b_d = r(b_d) ///
	 b1_d = r(b1_d), reps(500): exponentialexample, obs(10000)

*** summarize/graph results of exponential mean example: Sample size 10,000
generate dev_blx = (bl_x - tme_x)
generate dev_bld = (bl_d - tme_d)
generate dev_bx = (b_x - tme_x)
generate dev_bd = (b_d - tme_d)
generate dev_bl1d = (bl_d - tme1_d)
generate dev_b1d = (b1_d - tme1_d)

twoway (kdensity dev_bx) (kdensity dev_blx), xline(0) ylabel(, nogrid) ///
	subtitle("Average marginal effect of X") xtitle("deviation") ///
	legend(label(1 "Correctly specified: OLS exponential in X & D") ///
            label(2 "Misspecified: OLS linear in X & D")  ///
        symxsize(*.5) size(small) row(2)) 


twoway (kdensity dev_bd) (kdensity dev_bld), xline(0) ylabel(, nogrid) ///
	subtitle("Average marginal effect of D")  xtitle("deviation") ///
	legend(label(1 "Correctly specified: OLS exponential in X & D") ///
            label(2 "Misspecified: OLS linear in X & D")  ///
        symxsize(*.5) size(small) row(2))


twoway (kdensity dev_b1d) (kdensity dev_bl1d), xline(0) ylabel(, nogrid) ///
	subtitle("Marginal effect of D at D=1")  xtitle("deviation") ///
	legend(label(1 "Correctly specified: OLS exponential in X & D") ///
            label(2 "Misspecified: OLS linear in X & D")  ///
        symxsize(*.5) size(small) row(2))


*************************************
*** Visual checks of misspecification
*************************************

**********************
*** Simulated examples
**********************
clear all
set more off
set scheme sj

*** Simulate a dataset

*** Generate data for visual checks
clear
set obs 1000
set seed 123456
generate x = runiform()
generate z = runiform()
generate u = rnormal()
generate y1 = 0 + 1*x + 0.0*z + u
generate y2 = 0 + 10*x^2 + 0.0*z + u
generate y3 = exp(-1 + 1*x + 0.0*z + u)

*** Fit correctly specified model: Linear in x
regress y1 x z

*** Create residual plots
rvfplot, name(rvf1, replace) nodraw
rvpplot x, name(rvpx1, replace) nodraw
rvpplot z, name(rvpz1, replace) nodraw
graph combine rvf1 rvpx1 rvpz1, col(3) xsize(7) ysize(2)

*** Fit misspecified model: True model is quadratic in x
regress y2 x z

*** Create residual plots
rvfplot, name(rvf2, replace) nodraw
rvpplot x, name(rvpx2, replace) nodraw
rvpplot z, name(rvpz2, replace) nodraw
graph combine rvf2 rvpx2 rvpz2, col(3) xsize(7) ysize(2)

*** Fit misspecified model: True model is quadratic in x
regress y3 x z

*** Create residual plots
rvfplot, name(rvf3, replace) nodraw
rvpplot x, name(rvpx3, replace) nodraw
rvpplot z, name(rvpz3, replace) nodraw
graph combine rvf3 rvpx3 rvpz3, col(3) xsize(7) ysize(2)


**********************
*** MEPS data examples
**********************
clear all
set more off
set scheme sj

*** Use MEPS data; keep only positive expenditures
use heus_mepssample, clear
drop if exp_tot <= 0
generate ln_exp_tot = ln(exp_tot)

*** Linear regression of expenditures on age and ln(income)
regress exp_tot age lninc
*** Create residual plots
preserve
set seed 123456
keep if runiform()<0.1
rvfplot, name(rvfm1, replace) nodraw
rvpplot age, name(rvpxm1, replace) nodraw xlabel(20(20)80)
rvpplot lninc, name(rvpzm1, replace) nodraw xlabel(2(2)12)
graph combine rvfm1 rvpxm1 rvpzm1, col(3) xsize(7) ysize(2)
restore

*** Linear regression of ln(expenditures) on age and ln(income)
regress ln_exp_tot age lninc

*** Create residual plots
preserve
set seed 123456
keep if runiform()<0.1
rvfplot, name(rvfm2, replace) nodraw
rvpplot age, name(rvpxm2, replace) nodraw xlabel(20(20)80)
rvpplot lninc, name(rvpzm2, replace) nodraw xlabel(2(2)12)
graph combine rvfm2 rvpxm2 rvpzm2, col(3) xsize(7) ysize(2)
restore


*****************************************
*** Statistical tests of misspecification
*****************************************

clear all
set more off
set scheme sj

*** MEPS data, focus on positive values of total expenditures
use heus_mepssample, clear
drop if exp_tot <= 0

*** OLS regression of exp_tot for y>0 with simple specification
regress exp_tot age female, vce(robust)

*** Generate terms for link and RESET tests
predict yhat1 if e(sample), xb
quietly summarize yhat1
*** Normalization
replace  yhat1 = (yhat1 - r(min))/(r(max) - r(min))
generate yhat2 = yhat1^2
generate yhat3 = yhat1^3
generate yhat4 = yhat1^4

*** Pregibon's link test, with vce(robust)
regress exp_tot yhat1 yhat2, vce(robust)
test yhat2
	
*** Ramsey's RESET test, with vce(robust)
regress exp_tot yhat1 yhat2 yhat3 yhat4, vce(robust)
test yhat2 yhat3 yhat4

*** Link and RESET tests with Stata commands
quietly regress exp_tot age female, vce(robust)
linktest, vce(robust)
*** vce(robust) not possible
estat ovtest

*** Modified Hosmer-Lemeshow test
quietly regress exp_tot age female, vce(robust)
predict yhat if e(sample), xb
predict resid if e(sample), residual
xtile Ipcat = yhat if e(sample), nq(10)	
sort Ipcat
quietly regress resid ibn.Ipcat if e(sample), nocons vce(robust)
testparm i.Ipcat

*** Graph
predict mhlhat, xb
predict mhlstd, stdp
generate hi95 = mhlhat + 1.96*mhlstd
generate lo95 = mhlhat - 1.96*mhlstd
line mhlhat hi95 lo95 Ipcat, yline(0,lwidth(medium)) ///
	legend(off) ///
	title("Modified Hosmer-Lemeshow test") ///
	ytitle("mHL coefficients and CI") xtitle("Predicted categories")


*** Richer model with interactions
drop yhat yhat1 yhat2 yhat3 yhat4 resid mhl* Ipcat hi95 lo95
*** OLS regression of exp_tot for y>0 with more general specification
regress exp_tot c.age##c.age##i.female, vce(robust)


*** The following code is repeated from above, for model with interactions
*** Generate terms for link and RESET tests
predict yhat1 if e(sample), xb
quietly summarize yhat1
*** normalization
replace  yhat1 = (yhat1 - r(min))/(r(max) - r(min))
generate yhat2 = yhat1^2
generate yhat3 = yhat1^3
generate yhat4 = yhat1^4

*** Pregibon's link test, with vce(robust)
quietly regress exp_tot yhat1 yhat2, vce(robust)
test yhat2
	
*** Ramsey's RESET test, with vce(robust)
quietly regress exp_tot yhat1 yhat2 yhat3 yhat4, vce(robust)
test yhat2 yhat3 yhat4

*** Modified Hosmer-Lemeshow test
quietly regress exp_tot c.age##c.age##i.female, vce(robust)
predict yhat if e(sample), xb
predict resid if e(sample), residual
xtile Ipcat = yhat if e(sample), nq(10)	
sort Ipcat
quietly regress resid ibn.Ipcat if e(sample), nocons vce(robust)
testparm i.Ipcat

*** Graph
predict mhlhat, xb
predict mhlstd, stdp
generate hi95 = mhlhat + 1.96*mhlstd
generate lo95 = mhlhat - 1.96*mhlstd
line mhlhat hi95 lo95 Ipcat, yline(0,lwidth(medium)) ///
	legend(off) ///
	ytitle("mHL coefficients and CI") xtitle("Predicted categories") ///
	title("Modified Hosmer-Lemeshow test")


*** AIC and BIC
clear all
set more off
set scheme sj

*** Generate simulation data with quadratic specification
set obs 1000
set seed 123456
generate x = 2 + 6*runiform()
generate z = 2 + 6*rbeta(2,4)
generate u = 3*rnormal()
generate y = 1000 - 2*x + 0.3*x^2  - 2*z + 0.3*z^2 + u

quietly regress y x z
estimates store linear
quietly regress y c.x##c.x c.z
estimates store quadratic_x
quietly regress y c.x c.z##c.z 
estimates store quadratic_z
quietly regress y c.x##c.x c.z##c.z 
estimates store quadratic_xz
quietly regress y c.x##c.x##c.x c.z
estimates store cubic_x
quietly regress y c.x c.z##c.z##c.z
estimates store cubic_z
quietly regress y c.x##c.x##c.x c.z##c.z##c.z
estimates store cubic_xz


estimates stats linear quadratic_* cubic_*

*** Generate simulation data with quadratic specification
clear
set obs 10000
set seed 123456
generate x = 2 + 6*runiform()
generate z = 2 + 6*rbeta(2,4)
generate u = 3*rnormal()
generate y = 1000 - 2*x + 0.3*x^2  - 2*z + 0.3*z^2 + u

quietly regress y x z
estimates store linear

quietly regress y c.x##c.x c.z
estimates store quadratic_x
quietly regress y c.x c.z##c.z 
estimates store quadratic_z
quietly regress y c.x##c.x c.z##c.z 
estimates store quadratic_xz

quietly regress y c.x##c.x##c.x c.z
estimates store cubic_x
quietly regress y c.x c.z##c.z##c.z
estimates store cubic_z
quietly regress y c.x##c.x##c.x c.z##c.z##c.z
estimates store cubic_xz


estimates stats linear quadratic_* cubic_*

*** MEPS data, focus on positive values of total expenditures
use heus_mepssample, clear
drop if exp_tot <= 0

regress exp_tot age female
estimates store linear

regress exp_tot c.age##i.female
estimates store interact
regress exp_tot c.age##c.age##i.female
estimates store quad_int

estimates stats linear interact quad_int

regress exp_tot age female anylim
estimates store linear

regress exp_tot c.age##i.female anylim
estimates store interact
regress exp_tot c.age##c.age##i.female anylim
estimates store quad_int

estimates stats linear interact quad_int

exit
