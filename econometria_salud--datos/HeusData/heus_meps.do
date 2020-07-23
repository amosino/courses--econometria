***************************
*** heus_meps.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** MEPS data

clear all
set more off
set scheme sj
set linesize 79



*** 2004 MEPS data
*** 19386 observations on 44 variables

*** Stata: Describe dataset
clear

*** Variable description for the example MEPS 2004 dataset
use heus_mepssample.dta
*** 2004 MEPS data:  ID and sampling variables
describe duid pid famidyr dupersid hieuidx wtdper var*

*** 2004 MEPS data:  Demographic variables
describe age female race_* eth_hisp famsize ed_* lninc reg_*

*** 2004 MEPS data:  Health and insurance variables
describe anylim mcs12 pcs12 ins_*

*** 2004 MEPS data:  Expenditures and use variables
describe exp_* use_*


*** Stata: Summary statistics for expenditure variables
foreach v of varlist exp_tot exp_ip exp_er exp_dent exp_self {
	generate `v'0 = (`v' == 0)
	generate `v'gt0 = `v'
	replace `v'gt0 = . if `v' == 0
	generate ln`v' = ln(`v'gt0)
}

*** Summary statistics for expenditure variables
tabstat exp_tot exp_ip exp_ip_fac exp_ip_md exp_er exp_er_fac exp_er_md ///
	exp_dent exp_self, ///
	stats(mean sd skewness kurtosis min max) columns(statistics) varwidth(8)

*** Percent of observations with zero expenditures
tabstat exp_tot0 exp_ip0 exp_er0 exp_dent0 exp_self0, ///
	stats(mean) columns(statistics) varwidth(14)

*** Summary statistics for expenditure variables, if any expenditures
tabstat exp_totgt0 exp_ipgt0 exp_ergt0 exp_dentgt0 exp_selfgt0, ///
	stats(n mean sd skewness min max) columns(statistics) ///
	varwidth(13)

*** Summary statistics for logarithm of expenditure variables
tabstat lnexp_tot lnexp_ip lnexp_er lnexp_dent lnexp_self, ///
	stats(n mean sd skewness min max) columns(statistics) ///
	varwidth(13)

*** Summary statistics for use variables
tabstat use_*, stats(n mean sd skewness min max) columns(statistics)

*** Stata: Summary statistics for explanatory variables
*** Summary statistics for demographic variables
summarize age female race_* eth_hisp famsize ed_* lninc reg_*

*** Summary statistics for health and insurance variables
summarize anylim mcs12 pcs12 ins_*


*** Graphs

*** Histograms
histogram lnexp_tot, normal bin(40) legend(off) ///
	yline(0) xlabel(0(2)14) xtitle("ln(total expenditures)") ///
    title("Histogram of ln(total expenditures)") name(lnexp_tot,replace)


*** Office-based provider visits
preserve
count if use_off == 0
local N0 = r(N)
count if use_off > 25
local Ntc = r(N)
replace use_off = 25 if use_off > 25
graph twoway histogram use_off, fraction legend(off) discrete ///
	title("Office-based provider visits") xtitle("Number of visits") ///
	text(.27 3 "`N0' have 0 visits", place(ne)) ///
	text(.06 25 "`Ntc' top-coded at 25", place(nw)) lwidth(medthick) ///
	name(use_off, replace) nodraw
restore

*** Dental visits
count if use_dent == 0
local N0 = r(N)
graph twoway histogram use_dent, fraction legend(off) discrete ///
	title("Dental visits") xtitle("Number of dental visits") ///
	text(.55 3 "`N0' have 0 visits", place(ne)) lwidth(medthick) ///
	name(use_dent, replace) nodraw

*** Prescriptions and refills
preserve
count if use_rx == 0
local N0 = r(N)
count if use_rx > 25
local Ntc = r(N)
replace use_rx = 25 if use_rx > 25
graph twoway histogram use_rx, fraction legend(off) discrete ///
	title("Prescriptions and refills") 	///
	xtitle("Number of prescriptions and refills") ///
	text(.31 3 "`N0' have 0 visits", place(ne)) ///
	text(.18 25 "`Ntc' top-coded at 25", place(nw)) lwidth(medthick) ///
	name(use_rx, replace) nodraw
restore

graph combine use_off use_dent use_rx, cols(1) xsize(3) ysize(5)

exit
