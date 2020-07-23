***************************
*** heus_glm.do
***************************

*** STATA Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Generalized Linear Models

clear all
set more off
set scheme sj
set linesize 79




/*Density plots of expenditures and residuals*/
use heus_mepssample
drop if exp_tot == . | exp_tot == 0
quietly regress exp_tot age i.female i.race_bl i.race_oth i.eth_hisp famsize ///
	i.ed_hs i.ed_hsplus i.ed_col i.ed_colplus lninc ///
	i.reg_midw i.reg_south i.reg_west i.anylim mcs12 pcs12 ///
	i.ins_mcare i.ins_mcaid i.ins_unins ins_dent
predict exphat
centile exphat, centile(2 5 6 7)
predict u, residual

twoway kdensity exp_tot if exp_tot<100000, ///
	ytitle(density) xtitle(total expenditure) name(a, replace) nodraw
twoway 	kdensity u if exp_tot<100000, ///
	ytitle(density) xtitle(OLS residuals) name(b, replace) nodraw
graph combine a b, xsize(6) ///
		title("Densities of total expenditures and its residuals" ///
		"among those who had at least some expenditure") ///
		note("Expenditures above $100,000 (around the 99.9th percentile) dropped")


/* GLM with log link and Gaussian family */
*** GLM of total expenditures, log link and Gaussian family
glm exp_tot age female, link(log) family(gaussian) vce(robust)

/* GLM with log link and gamma family */
*** GLM of total expenditures, log link and gamma family
glm exp_tot age female, link(log) family(gamma) vce(robust)

*** Predicted mean of total expenditures from GLM with log link and gamma family
quietly glm exp_tot age female, link(log) family(gamma) vce(robust)
margins


/* GLM with interaction between age and female */
*** GLM of total expenditures, log link and gamma family
*** Specification includes interaction between age and gender
glm exp_tot c.age##i.female, link(log) family(gamma) vce(robust)

*** margins: Overall predicted mean
margins

*** margins: Predicted values at different ages, by gender
margins, at(age=(20(30)80) female=(0 1))

*** margins: Predicted values at different ages, by gender
quietly margins, at(age=(20(10)80) female=(0 1))
marginsplot

*** Marginal effect of age and gender
margins, dydx(age female)

*** Marginal effect of age at different ages, by gender
margins, dydx(age female) at(age=(20(30)80) female=(0 1))

margins, at(age = generate(age)) at(age = generate(age+10)) ///
		contrast(atcontrast(r) nowald) vce(unconditional)

quietly margins, dydx(age) at(age=(20(10)80) female=(0 1))
marginsplot


/* Specification tests */
*** AIC and BIC tests for link function and distribution family
local xvar age i.female i.race_bl i.race_oth i.eth_hisp famsize ///
	i.ed_hs i.ed_hsplus i.ed_col i.ed_colplus lninc ///
	i.reg_midw i.reg_south i.reg_west i.anylim mcs12 pcs12 ///
	i.ins_mcare i.ins_mcaid i.ins_unins ins_dent
	
quietly glm exp_tot `xvar', iter(40) link(log) family(gamma)
estimates store glm_log_gam
quietly glm exp_tot `xvar', iter(40) link(power .5) family(gamma)
estimates store glm_sqrt_gam

quietly glm exp_tot `xvar', iter(40) link(log) family(gaussian)
estimates store glm_log_gau
quietly glm exp_tot `xvar', iter(40) link(power .5) family(gaussian)
estimates store glm_sqrt_gau

quietly glm exp_tot `xvar', iter(40) link(log) family(poisson) scale(x2)
estimates store glm_log_poi
quietly glm exp_tot `xvar', iter(40) link(power .5) family(poisson) scale(x2)
estimates store glm_sqrt_poi

*** Results of AIC and BIC tests
estimates stats *

*** Link test
xi: boxcox exp_tot age i.female i.race_bl i.race_oth i.eth_hisp famsize ///
	i.ed_hs i.ed_hsplus i.ed_col i.ed_colplus lninc ///
	i.reg_midw i.reg_south i.reg_west i.anylim mcs12 pcs12 ///
	i.ins_mcare i.ins_mcaid i.ins_unins ins_dent

*** Run GLM to generate residuals for Park test
quietly glm exp_tot age i.female i.race_bl i.race_oth i.eth_hisp famsize ///
	i.ed_hs i.ed_hsplus i.ed_col i.ed_colplus lninc ///
	i.reg_midw i.reg_south i.reg_west i.anylim mcs12 pcs12 ///
	i.ins_mcare i.ins_mcaid i.ins_unins ins_dent, link(log) family(gamma)

*** Generate ln(raw residuals squared) and xbetahat for Park test
predict double rawresid, response
generate lnrawresid2 = ln(rawresid^2)
predict double xbetahat, xb

*** Modified Park test
regress lnrawresid2 xbetahat, robust

exit
