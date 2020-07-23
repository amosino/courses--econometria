***************************
*** heus_endog.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Endogeneity

clear all
set more off
set scheme sj
set linesize 79



****************************************************
*** Artificial data: continuous endogenous regressor
****************************************************
set obs 10000
set seed 123456
generate x  = rnormal(0,1)        // exogenous variable
generate w  = rnormal(0,1)        // instrumental variable
generate u  = rnormal(0,1)        // omitted variable
generate e1 = rnormal(0,1)        // outcome eq error
generate e2 = rnormal(0,1)        // endogenous regressor eq error
generate y2 = x + .2*w + u + e2   // endogenous regressor equation
generate y1 = y2 + x + u + e1     // outcome equation


*** OLS: Regression with u included
regress y1 y2 x u

*** OLS: Regression with u omitted
regress y1 y2 x

*** 2SLS
ivregress 2sls y1 x (y2 = w), first

*** Specification tests: First stage
estat firststage

*** Specification test: Exogeneity
estat endogenous

*** 2SRI
program regress_continuousendog_2SRI, eclass
	capture drop nu2_hat
	*** First stage (same as 2SLS)
	regress y2 x w
	predict double nu2_hat, residuals
	*** Main regression
	regress y1 y2 x nu2_hat
end
bootstrap _b, reps(200) seed(123456): regress_continuousendog_2SRI

*** ERM with FIML
eregress y1 x, endogenous(y2 = x w)


************************************************
*** Artificial data: binary endogenous regressor
************************************************
clear
set obs 10000
set seed 123456
generate x  = rnormal(0,1)                // exogenous variable
generate w  = rnormal(0,1)                // instrumental variable
generate u  = sqrt(0.5)*rnormal(0,1)      // omitted variable
generate e1 = sqrt(0.5)*rnormal(0,1)      // outcome eq error
generate e2 = sqrt(0.5)*rnormal(0,1)      // endogenous regressor eq error
generate y2st = -1.8 + x + .2*w + u + e2  // endogenous regressor latent eq
generate y2 = y2st>0                      // binary variable from latent
generate y1 = y2 + x + u + e1             // outcome eq

tabulate y2

*** OLS: Regression with u omitted
regress y1 1.y2 x

*** 2SLS
ivregress 2sls y1 x (1.y2 = w), first

program regress_binaryendog_2SRI, eclass
	capture drop y2_hat nu2_hat
	*** First stage probit
	probit y2 x w
	predict double y2_hat
	generate nu2_hat = y2 - y2_hat
	*** Main regression
	regress y1 1.y2 x nu2_hat
end
bootstrap _b, reps(200) seed(123456): regress_binaryendog_2SRI

*** ERM with FIML
eregress y1 x, endogenous(y2 = x w, probit)

*** 2SLS
ivregress 2sls y1 x c.x#c.x (y2 = w c.w#c.w c.w#c.x c.w#c.x#c.x), first


*******
*** GMM
*******

clear
set obs 10000
set seed 123456
generate x  = rnormal(0,1)        // exogenous variable
generate w  = rnormal(0,1)        // instrumental variable
generate u  = rnormal(0,1)        // omitted variable
generate e1 = rnormal(0,1)        // outcome eq error
generate e2 = rnormal(0,1)        // endogenous regressor eq error
generate y2 = x + .2*w + u + e2   // endogenous regressor equation
generate y1 = y2 + x + u + e1     // outcome equation

*** 2SLS: GMM version
ivregress gmm  y1 x (y2 = w)

*** GMM control function estimator with consistent standard errors
gmm (r1: y2 - {xb1:w x _cons}) ///
(r2: (y1 - {b0} - {b1}*y2 - {b2}*x - {b3}*(y2-{xb1:}))) ///
(r3: (y1 - {b0} - {b1}*y2 - {b2}*x - {b3}*(y2-{xb1:}))* ///
(y2-{xb1:})), ///
instruments(r1: w x) ///
instruments(r2: y2 x ) ///
instruments(r3: ) ///
onestep winitial(identity)

exit
