***************************
*** heus_statapressbook.do
***************************

*** Stata Program
*** copyright C 2017 by Partha Deb, Edward C. Norton, and Willard G. Manning
*** used for "Health Econometrics Using Stata"
*** by Partha Deb, Edward C. Norton, and Willard G. Manning (2017)
*** Stata Press

*** Sequentially do'es all do files for the book

clear all
set more off

do heus_meps.do
do heus_spec.do
do heus_glm.do
do heus_pos.do
do heus_zero.do
do heus_count.do
do heus_flex.do
do heus_endog.do
do heus_de.do
