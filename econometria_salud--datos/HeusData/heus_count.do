***************************
*** heus_count.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Models for Count data

clear all
set more off
set scheme sj
set linesize 79
capture cd "C:\DMN"



***************************************
*** What count data densities look like
***************************************

set obs 10000
set seed 123456

*** Poisson density with a mean of 0.5
generate y = rpoisson(0.5)
replace y = min(y,10)
generate i=100/_N
preserve
collapse (sum) i, by(y)
graph bar (asis) i, over(y) bar(1, fintensity(inten50)) ///
	ytitle(percent) subtitle(Poisson with mean 0.5)
restore


*** Poisson density with a mean of 5
replace y = rpoisson(5)
replace y = min(y,10)
preserve
collapse (sum) i, by(y)
graph bar (asis) i, over(y, relabel(11 "10+")) bar(1, fintensity(inten50)) ///
	ytitle(percent) subtitle(Poisson with mean 5)
restore


*** Empirical count densities in the MEPS data
use heus_mepssample.dta, clear

generate truse_off = min(use_off,20)

histogram truse_off, discrete percent name(g1, replace) fintensity(inten50) ///
	xtitle("# office-based provider visits") xlabel(20 "20+", add) nodraw
histogram use_er, discrete percent name(g2, replace) fintensity(inten50) ///
	xtitle("# ER visits") note(" ") nodraw
graph combine g1 g2, rows(1) xsize(5) ysize(2.5)


*** Poisson regression analysis Office visits
*** Poisson regression coefficients
use heus_mepssample.dta, clear
poisson use_off age i.female

*** Poisson average marginal effects
margins, dydx(age female)

*** Poisson marginal effects at means
margins, dydx(age female) at((mean) _all) noatlegend

*** Poisson coefficients with robust standard errors
poisson use_off age i.female, vce(robust)

preserve
*** Poisson regression observed and predicted densities
poisson use_off age i.female
forvalues j=0/20 {
	generate byte y_`j' = `e(depvar)' == `j'
	predict pr_`j', pr(`j')
}

*** Graph of Poisson regression  observed and predicted densities
collapse (mean) y_* pr_*
generate i=_n
reshape long y_ pr_, i(i) j(y)

graph bar (asis) y_ pr_, bar(1, fintensity(inten0) lwidth(medthick)) ///
	over(y) name(g1, replace) nodraw ///
	subtitle("# office-based provider visits", size(large)) ///
	legend(label(1 Empirical) label(2 Predicted))
restore


*** Poisson regression analysis repeated for ER use
poisson use_er age i.female
preserve
forvalues j=0/10 {
	generate byte y_`j' = `e(depvar)' == `j'
	predict pr_`j', pr(`j')
}
collapse (mean) y_* pr_*
generate i=_n
reshape long y_ pr_, i(i) j(y)

graph bar (asis) y_ pr_, bar(1, fintensity(inten0) lwidth(medthick)) ///
	over(y) name(g2, replace) nodraw ///
	subtitle("# ER visits", size(large)) ///
	legend(label(1 Empirical) label(2 Predicted))
restore

graph combine g1 g2, rows(1) xsize(5) ysize(2.5)


*** Negative binomial density with a mean of 2
clear
set obs 10000
set seed 123456

generate y1 = rpoisson(2)
generate g = rgamma(0.5,2)
generate y2 = rpoisson(2*g)

replace y1 = min(y1,10)
replace y2 = min(y2,10)

generate i1=100/_N
generate i2=100/_N
preserve
collapse (sum) i1, by(y1)
rename y1 y
tempfile using
save `using'
restore
collapse (sum) i2, by(y2)
rename y2 y

merge 1:1 y using `using'

graph bar (asis) i1 i2, over(y, relabel(11 "10+")) ytitle(percent) ///
	subtitle(Poisson and negative binomial with mean 2) ///
	legend(label(1 "Poisson") label(2 "Negative binomial"))



*** NB2 regression coefficients
use heus_mepssample.dta, clear
nbreg use_off age i.female, vce(robust)

*** NB2 regression marginal effects
margins, dydx(age female)

*** NB2 regression coefficients
nbreg use_off age i.female, dispersion(constant) vce(robust)

*** NB2 regression coefficients
margins, dydx(age female)

nbreg use_off age i.female, vce(robust)
preserve
forvalues j=0/20 {
	generate byte y_`j' = `e(depvar)' == `j'
	predict pr_`j', pr(`j')
}
collapse (mean) y_* pr_*
generate i=_n
reshape long y_ pr_, i(i) j(y)

graph bar (asis) y_ pr_, bar(1, fintensity(inten0) lwidth(medthick)) ///
	over(y) name(g1, replace) ///
	subtitle("# office-based provider visits", size(large)) ///
	legend(label(1 Empirical) label(2 Predicted))

generate pr_diff = (y_ - pr_)*100
generate pr_diff2 = pr_diff^2
summarize pr_diff pr_diff2
restore


nbreg use_er age i.female, vce(robust)
preserve
forvalues j=0/10 {
	generate byte y_`j' = `e(depvar)' == `j'
	predict pr_`j', pr(`j')
}
collapse (mean) y_* pr_*
generate i=_n
reshape long y_ pr_, i(i) j(y)

graph bar (asis) y_ pr_, bar(1, fintensity(inten0) lwidth(medthick)) ///
	over(y) name(g2, replace) ///
	subtitle("# ER visits", size(large)) ///
	legend(label(1 Empirical) label(2 Predicted))

generate pr_diff = (y_ - pr_)*100
generate pr_diff2 = pr_diff^2
summarize pr_diff pr_diff2
restore


nbreg use_los age i.female, vce(robust)
preserve
forvalues j=0/5 {
	generate byte y_`j' = `e(depvar)' == `j'
	predict pr_`j', pr(`j')
}
collapse (mean) y_* pr_*
generate i=_n
reshape long y_ pr_, i(i) j(y)

graph bar (asis) y_ pr_, bar(1, fintensity(inten0) lwidth(medthick)) ///
	over(y) name(g3, replace) ///
	subtitle("# nights in hospital", size(large)) ///
	legend(label(1 Empirical) label(2 Predicted))

generate pr_diff = (y_ - pr_)*100
generate pr_diff2 = pr_diff^2
summarize pr_diff pr_diff2
restore

graph combine g1 g2, rows(1) xsize(5) ysize(2.5)


*** Hurdle models
*** Hurdle Poisson model hurdle part estimates
generate any_off = use_off>0
logit any_off age i.female, vce(robust)

*** Hurdle Poisson model hurdle part marginal effects
margins, dydx(*)

*** Hurdle Poisson model estimates
tpoisson use_off age i.female if use_off>0, ll(0) vce(robust)

*** Hurdle Poisson model marginal effects
margins, dydx(*) predict(cm) noesample

*** Hurdle Poisson model suest
quietly logit any_off age i.female
estimates store h1
quietly tpoisson use_off age i.female if use_off>0, ll(0)
estimates store h2
suest h1 h2

*** Hurdle Poisson model marginal effects
local logit "invlogit(predict(eq(h1_any_off))) "
local ey "exp(predict(eq(h2_use_off))) "
local pygt0 "poissontail(exp(predict(eq(h2_use_off))),1)"
margins, dydx(*) expression("`logit'*`ey'/`pygt0'")


*** Hurdle NB2 model marginal effects
quietly logit any_off age i.female
estimates store h1
quietly tnbreg use_off age i.female if use_off>0, ll(0)
estimates store h2
quietly suest h1 h2

local logit "invlogit(predict(eq(h1_any_off))) "
local ey "exp(predict(eq(h2_use_off))) "
local pygt0 "(nbinomialtail(exp(-predict(eq(/h2:lnalpha))),1," ///
	"1/(1+exp(predict(eq(h2_use_off)))/exp(-predict(eq(/h2:lnalpha))))))"
margins, dydx(*) expression("`logit'*`ey'/`pygt0'")

*** Zero-inflated NB2 model estimates
zinb use_off age i.female, inflate(age i.female) vce(robust)

*** Zero-inflated NB2 model margins
margins, dydx(age female)


***************************************
*** Choosing between alternative models
***************************************

local xvar age i.female i.race_bl i.race_oth i.eth_hisp famsize ///
	i.ed_hs i.ed_hsplus i.ed_col i.ed_colplus lninc i.reg_midw ///
	i.reg_south i.reg_west i.anylim mcs12 pcs12 ///
	i.ins_mcare i.ins_mcaid i.ins_unins ins_dent

*** Estimate models and calculate AIC, BIC for in sample comparisons
foreach yvar of varlist use_off use_er {

	display _n "outcome is `yvar'"
	display "poisson"
	quietly {
		poisson `yvar' `xvar', iter(30)
		scalar N = e(N)
		scalar ll_poisson = e(ll)
		scalar k_poisson = e(k)
		scalar aic_poisson = -2*ll_poisson + 2*k_poisson
		scalar bic_poisson = -2*ll_poisson + k_poisson*ln(N)
	}

	display "nb2"
		quietly {
		nbreg `yvar' `xvar', iter(30)
		scalar ll_nb2 = e(ll)
		scalar k_nb2 = e(k)
		scalar aic_nb2 = -2*ll_nb2 + 2*k_nb2
		scalar bic_nb2 = -2*ll_nb2 + k_nb2*ln(N)
	}

	display "nb1"
	quietly {
		nbreg `yvar' `xvar', dispersion(constant) iter(30)
		scalar ll_nb1 = e(ll)
		scalar k_nb1 = e(k)
		scalar aic_nb1 = -2*ll_nb1 + 2*k_nb1
		scalar bic_nb1 = -2*ll_nb1 + k_nb1*ln(N)
	}

	quietly {
		logit `yvar' `xvar', iter(30)
		scalar ll1 = e(ll)
		scalar k1 = e(k)
		scalar N = e(N)
	}

	display "hurdle poisson"
	quietly {
		tpoisson `yvar' `xvar' if `yvar'>0, ll(0) iter(30)
		scalar ll2 = e(ll)
		scalar k2 = e(k)
		scalar ll_hpoisson = ll1 + ll2
		scalar k_hpoisson = k1 + k2
		scalar aic_hpoisson = -2*ll_hpoisson + 2*k_hpoisson
		scalar bic_hpoisson = -2*ll_hpoisson + k_hpoisson*ln(N)
	}

	display "hurdle nb2"
	quietly {
		tnbreg `yvar' `xvar' if `yvar'>0, ll(0) iter(30)
		scalar ll2 = e(ll)
		scalar k2 = e(k)
		scal ll_hnb2 = ll1 + ll2
		scalar k_hnb2 = k1 + k2
		scalar aic_hnb2 = -2*ll_hnb2 + 2*k_hnb2
		scalar bic_hnb2 = -2*ll_hnb2 + k_hnb2*ln(N)
	}

	display "hurdle nb1"
	quietly {
		tnbreg `yvar' `xvar' if `yvar'>0, ll(0) dispersion(constant) iter(30)
		scalar ll2 = e(ll)
		scalar k2 = e(k)
		scal ll_hnb1 = ll1 + ll2
		scalar k_hnb1 = k1 + k2
		scalar aic_hnb1 = -2*ll_hnb1 + 2*k_hnb1
		scalar bic_hnb1 = -2*ll_hnb1 + k_hnb1*ln(N)
	}

	display "zi poisson"
	quietly {
		zip `yvar' `xvar', inflate(`xvar') iter(30)
		scalar ll_zip = e(ll)
		scalar k_zip = e(k)
		scalar aic_zip = -2*ll_zip + 2*k_zip
		scalar bic_zip = -2*ll_zip + k_zip*ln(N)
	}

	display "zi nb2"
	quietly {
		zinb `yvar' `xvar', inflate(`xvar') iter(30)
		scalar ll_zinb2 = e(ll)
		scalar k_zinb2 = e(k)
		scalar aic_zinb2 = -2*ll_zinb2 + 2*k_zinb2
		scalar bic_zinb2 = -2*ll_zinb2 + k_zinb2*ln(N)
	}

	matrix stats = ///
		k_poisson, ll_poisson, aic_poisson, bic_poisson \ ///
		k_nb2, ll_nb2, aic_nb2, bic_nb2 \ ///
		k_nb1, ll_nb1, aic_nb1, bic_nb1 \ ///
		k_hpoisson, ll_hpoisson, aic_hpoisson, bic_hpoisson \ ///
		k_hnb2, ll_hnb2, aic_hnb2, bic_hnb2 \ ///
		k_hnb1, ll_hnb1, aic_hnb1, bic_hnb1 \ ///
		k_zip, ll_zip, aic_zip, bic_zip \ ///
		k_zinb2, ll_zinb2, aic_zinb2, bic_zinb2

	matrix rownames stats = Poisson NB2 NB1 ///
		Hurdle_Poisson Hurdle_NB2 Hurdle_NB1 ZIP ZINB2

	matrix colnames stats = K LogLik AIC BIC

	display "Model selection statistics for `yvar'"
	matrix list stats
}


************************************
*** Cross-validation model selection
************************************

generate training = .
generate replicate = .
*** generate variables to fill in with log likelihoods
generate llnb2 = .
generate llnb1 = .
generate llhnb2 = .
generate llhnb1 = .
generate llzinb2 = .
generate dllnb1 = .
generate dllhnb2 = .
generate dllhnb1 = .
generate dllzinb2 = .

local st1 "# Office-based visits"
local st2 "#ER visits"
local i 0

*** K fold cross validation
set seed 123456
generate _u = runiform()
xtile Kfold = _u, nq(10)
drop _u

foreach yvar of varlist use_off use_er {

local i `=`i'+1'

forvalues rep = 1/10 {
	replace training = (Kfold!=`rep')
	replace replicate = `rep' in `rep'

	quietly {
		nbreg `yvar' `xvar' if training, iter(30)
		matrix s=e(b)
		matrix s2 = e(b)
		nbreg `yvar' `xvar' if !training, iter(0) from(s)
		replace llnb2 = e(ll) in `rep'

		nbreg `yvar' `xvar' if training, dispersion(constant) iter(30)
		matrix s=e(b)
		matrix s1 = e(b)
		nbreg `yvar' `xvar' if !training, dispersion(constant) iter(0) from(s)
		replace llnb1 = e(ll) in `rep'

		logit `yvar' `xvar' if training,
		matrix s=e(b)
		logit `yvar' `xvar' if !training, iter(0) from(s)
		scalar ll1 = e(ll)

		tnbreg `yvar' `xvar' if `yvar'>0 & training, ll(0) iter(30) from(s2)
		matrix s=e(b)
		tnbreg `yvar' `xvar' if `yvar'>0 & !training, ll(0) iter(0) from(s)
		scalar ll2 = e(ll)
		scal llc = ll1 + ll2
		replace llhnb2 = llc in `rep'

		tnbreg `yvar' `xvar' if `yvar'>0 & training, ll(0) ///
			dispersion(constant) iter(30) from(s1)
		matrix s=e(b)
		tnbreg `yvar' `xvar' if `yvar'>0 & !training, ll(0) ///
			dispersion(constant) iter(0) from(s)
		scalar ll2 = e(ll)
		scal llc = ll1 + ll2
		replace llhnb1 = llc in `rep'

		zinb `yvar' `xvar' if training, inflate(`xvar') iter(30)
		matrix s=e(b)
		zinb `yvar' `xvar' if !training, inflate(`xvar') iter(0) from(s)
		replace llzinb2 = e(ll) in `rep'
	}
}

replace dllnb1 = llnb1 - llnb2
replace dllhnb2 = llhnb2 - llnb2
replace dllhnb1 = llhnb1 - llnb2
replace dllzinb2 = llzinb2 - llnb2

preserve
keep replicate dllnb1 dllhnb2 dllhnb1 dllzinb2
keep in 1/10
generate obs=_n
reshape long dll, i(obs) j(model) string

generate nmodel = 1 if model=="nb1"
replace nmodel = 2 if model=="hnb2"
replace nmodel = 3 if model=="hnb1"
replace nmodel = 4 if model=="zinb2"

graph bar (asis) dll, intensity(*.7) over(nmodel) over(replicate) asyvars exclude0 ///
	subtitle(`st`i'') ytitle(change in log likelihood over NB2) ///
	legend(label(1 NB1) label(2 Hurdle-NB2) label(3 Hurdle-NB1) ///
	label(4 Zi-NB2) col(4)) ///
	xsize(5) ysize(2.5)

restore

}

exit
