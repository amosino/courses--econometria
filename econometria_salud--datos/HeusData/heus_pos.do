***************************
*** heus_pos.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Models for Positive Continuous Values

clear all
set more off
set scheme sj
set linesize 79



*****************************************
*** Models for Positive Continuous Values
*****************************************

use heus_mepssample
*** OLS regression of ln(y) for y>0
drop if exp_tot <= 0    // focus on positive values of y
generate ln_exp_tot = ln(exp_tot)
regress ln_exp_tot age female, robust

*** Kennedy transformation and standard error
quietly regress ln_exp_tot age female
matrix V = e(V)
scalar Vc = V[rownumb(V,"female"),rownumb(V,"female")]
display "%Change = " 100*(exp(_b[female]) - 1)
display "Kennedy = " 100*(exp(_b[female] - .5*(Vc)) - 1)
display "s.e.    = " 100*sqrt(exp(2*_b[female])*(exp(-Vc)-exp(-2*Vc)))


program lnmodelpredictions, eclass
	capture drop xbhat ehat expxbhat normalfactor duanfactor yhat_*
	*** Store ln(y) results for later use
	quietly regress ln_exp_tot age female
	estimates store lnymodel
	predict xbhat, xb
	predict ehat, residual
	generate expxbhat = exp(xbhat)

	*** Normal factor
	egen normalfactor = mean(ehat^2)
	replace normalfactor = exp(0.5*normalfactor)
	generate yhat_normalfactor = expxbhat * normalfactor

	*** Duan factor
	egen duanfactor = mean(exp(ehat))
	generate yhat_duanfactor = expxbhat * duanfactor

	mean normalfactor duanfactor exp_tot yhat_*
end

bootstrap _b, reps(200) seed(123456): lnmodelpredictions


*** Box-Cox Models
use heus_mepssample, clear

*** Box-Cox test for total expenditures
boxcox exp_tot if exp_tot > 0, nolog

*** Box-Cox test for inpatient physician expenditures
boxcox exp_ip_md if exp_ip_md > 0, nolog

exit
