***************************
*** heus_zero.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Models for positive continuous values with some zeros

clear all
set more off
set scheme sj



*********************************************************
*** Motivations for generalized tobit and two-part models
*********************************************************
*** Example data-generating process identified only from functional form (z = x)
set obs 10000
set seed 123456
matrix C1 = (1, .7, 1)                             // covariance matrix
drawnorm eps1 eta1, cov(C1) cstorage(lower) double // draw errors
generate double x1 = 1 + rnormal()                 // model for x1
generate s1 = (-1 + x1 + eps1 >0)                  // model for s1
generate double y1 = s1*(100 + x1 + eta1)          // model for y1

*** Results for two-part model and generalized tobit
quietly twopm y1 x1, firstpart(probit) secondpart(regress)
estimates store tpm1
quietly heckman y1 x1, select(s1 = x1)
estimates store heckman1
estimates table tpm1 heckman1, se equations(1:2,2:1)

*** Marginal effects are nearly identical
estimates restore tpm1
quietly margins, dydx(x1) post
estimates store tpmmargins1
estimates restore heckman1
quietly margins, dydx(x1) predict(yexpected) post
estimates store heckmanmargins1
estimates table tpmmargins1 heckmanmargins1, se


*** Example data-generating process with exclusion restriction z
clear all
set obs 10000
set seed 123456
matrix C2 = (1, .7, 1)                             // covariance matrix
drawnorm eps2 eta2, cov(C2) cstorage(lower) double // draw errors
generate double x2 = 1 + rnormal()                 // model for x
generate double z2 = rnormal()                     // model for z
generate s2 = (-1 + x2 + z2 + eps2 >0)             // model for s
generate double y2 = s2*(100 + x2 + eta2)          // model for y

*** Results for two-part model and generalized tobit
quietly twopm y2 x2, firstpart(probit) secondpart(regress)
estimates store tpm2
quietly heckman y2 x2, select(s2 = x2)
estimates store heckman2
estimates table tpm2 heckman2, se equations(1:2,2:1)

*** Marginal effects are nearly identical
estimates restore tpm2
*** Marginal effect for two-part model
margins, dydx(x2)
estimates restore heckman2
*** Marginal effect for generalized tobit
margins, dydx(x2) predict(yexpected)



*********************************************************
*** Models for positive continuous values with some zeros
*********************************************************
use heus_mepssample, clear
svyset, clear

*******************
*** Two-part models
*******************
*** twopm: 1) probit  2) GLM with log link and gamma distribution
twopm exp_tot c.age##i.female, firstpart(probit, nolog) secondpart(glm, family(gamma) ///
	link(log) nolog) vce(robust)
estimates store lntot_twopm

*** Overall average prediction of total expenditures
margins

*** Marginal effects of age and gender
margins, dydx(*)

*** Marginal effect of age at different ages
margins, dydx(*) at(female=(0 1) age=(20(20)80))


*** Figure for predictions by age
margins, at(age=(20(1)80) female=(0 1))
marginsplot




****************************
*** Heckman Selection models
****************************
*** Dental expenditures compared with two-part model
use heus_mepssample, clear
generate y_heckman = exp_dent if exp_dent > 0

quietly twopm exp_dent c.age##i.female, firstpart(probit, nolog) ///
		secondpart(glm, family(gamma) link(log) nolog) vce(robust)
estimates store lndent_twopm
quietly heckman y_heckman c.age##i.female, select(c.age##i.female)
estimates store dent_FIML
quietly heckman y_heckman c.age##i.female, select(c.age##i.female) twostep
estimates store dent_LIML
estimates table lndent_twopm dent_FIML dent_LIML, se equations(1:2:2,2:1:1)


*** Predict dental expenditures from two-part model
estimates restore lndent_twopm
margins

*** Predict marginal effects for dental expenditures from two-part model
margins, dydx(*)

*** Dental marginal effects for two-part model
estimates restore lndent_twopm
margins, dydx(age) at(female=(0 1) age=(25 65))

*** Predicted dental expenditures from FIML
estimates restore dent_FIML
margins, predict(yexpected)
margins, dydx(*) predict(yexpected)

*** Dental marginal effects for FIML
margins, dydx(age) at(female=(0 1) age=(25 65)) predict(yexpected)

*** Latent outcomes for FIML
estimates restore dent_FIML
margins
margins, dydx(*)

exit
