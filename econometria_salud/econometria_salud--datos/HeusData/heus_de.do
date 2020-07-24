***************************
*** heus_de.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Taking Design Effects into account


**************************************
*** Taking Design Effects into account
**************************************

clear all
set more off
set scheme sj
set linesize 79



*** Setup
use heus_mepssample.dta, clear
svyset [pweight=wtdper], strata(varstr) psu(varpsu)

egen clusterid=group(varpsu varstr)
codebook clusterid

*** Weighted sample means
generate race_bl_pct = race_bl*100
quietly mean exp_tot race_bl_pct
estimates store noadjust
quietly mean exp_tot race_bl_pct, vce(cluster clusterid)
estimates store cluster
quietly mean exp_tot race_bl_pct [pw=wtdper]
estimates store weights
quietly mean exp_tot race_bl_pct [pw=wtdper], vce(cluster clusterid)
estimates store clust_wgt
quietly svy: mean exp_tot race_bl_pct
estimates store survey
estimates table *, b(%7.1f) modelwidth(9) ///
	title(Alternative cluster and weight options: Sample means)

estimates drop *

*** Sample mean and simple t tests
mean exp_tot, over(race_bl)
test [exp_tot]_subpop_1 = [exp_tot]_subpop_2

*** Sample mean and simple t tests incorporating survey features
svy: mean exp_tot, over(race_bl)
test [exp_tot]_subpop_1 = [exp_tot]_subpop_2


*** Linear regression
quietly regress exp_tot age i.female i.race_bl i.reg_south, vce(robust)
estimates store robust
quietly regress exp_tot age i.female i.race_bl i.reg_south, ///
	vce(cluster clusterid)
estimates store cluster
quietly regress exp_tot age i.female i.race_bl i.reg_south [pw=wtdper]
estimates store weights
quietly regress exp_tot age i.female i.race_bl i.reg_south [pw=wtdper], ///
	vce(cluster clusterid)
estimates store clust_wgt
quietly svy: regress exp_tot age i.female i.race_bl i.reg_south
estimates store survey
estimates table *, b(%7.2f) se(%7.2f) p(%7.4f) modelwidth(9) drop(_cons) ///
	title(Alternative cluster and weight options: Linear regression estimates)


*** Poisson
quietly poisson use_off age i.female i.race_bl i.reg_south, vce(robust)
quietly margins, dydx(*) post
estimates store robust
quietly poisson use_off age i.female i.race_bl i.reg_south, ///
	vce(cluster clusterid)
quietly margins, dydx(*) post
estimates store cluster
quietly poisson use_off age i.female i.race_bl i.reg_south [pw=wtdper]
quietly margins, dydx(*) post
estimates store weights
quietly poisson use_off age i.female i.race_bl i.reg_south [pw=wtdper], ///
	vce(cluster clusterid)
quietly margins, dydx(*) post
estimates store clust_wgt
quietly svy: poisson use_off age i.female i.race_bl i.reg_south
quietly margins, dydx(*) post
estimates store survey
estimates table *, b(%7.4f) se(%7.4f) p(%7.4f) modelwidth(9) ///
	title(Alternative cluster and weight options: Poisson effects)

exit
