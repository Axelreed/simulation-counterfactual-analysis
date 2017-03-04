

* Description: Paper 3 - Health shock and schooling
* Description:
// In this file I run models of schooling, this is the counterpart to the math 
// score analyses models


capture log close
log using paper3.log, replace
set more off, permanent

*****************************
*Schooling analysis, and health shocks
******************************
clear
if regexm(c(os),"Mac") == 1 {
	global ds `"/Users/Alex/Google Drive/DS"' 
	}
	else if regexm(c(os),"Windows") == 1 global ds `"C:/Users/areda/Google Drive/DS"' 

cd "$ds"
use allrf2, clear

* #################################
* Descriptive statistics

* Schooling - These values are based on anyone who ever attended school.

tab enrschr3 // Round 3
tab enrsch2 // Round 2

tab enrsch2 cntry1, col chi
tab enrschr3 cntry1, col chi

* Generating reverse coded stayin in school models
gen enrschr3x = .
replace enrschr3x = 1 if enrschr3 == 0
replace enrschr3x = 0 if enrschr3 == 1

* Math scores
* Here I restrict cases to only those that had responses on both r1 and r2
replace rmath3 = . if rmath2 == .
replace rmath2 = . if rmath3 == .

******************************************
* Counter-factual models
******************************************

* Generating dummies for wealth
tabulate wi14, gen(wi14d)

* Generating dummies for education
tabulate educpar1, gen(educpar1d)

*** Everyone low SES but no adversity

meqrlogit enrschr3x hhjob1 hhdeathill1 assetloss1 emp2 deathill2 asset2 stunted1 chores1 childwork1 order1 ageyr1 sex1 typesite1 educpar1d* wi14d* ///
	ntotal1 clustsch1 i.cntry1 || nclustid: , mle or
	
foreach i in hhdeathill1 deathill2 educpar1d1 educpar1d2 educpar1d3 wi14d1 wi14d2 wi14d3 wi14d4  {
gen real`i' = `i'
}


foreach i in hhdeathill1 deathill2 {
replace `i' = 0
}

foreach i in educpar1d1 wi14d1 {
replace `i' = 1
}

foreach i in educpar1d2 educpar1d3 wi14d2 wi14d3 wi14d4  {
replace `i' = 0
}

predict predsch_uh1, xb
gen p_pred_uh1 = exp(predsch_uh1)/(1 + exp(predsch_uh1))  // Fixed effects value
sum p_pred_uh1

* Everyone high SES and no adversity
	
foreach i in hhdeathill1 deathill2 {
replace `i' = 0
}

foreach i in educpar1d3 wi14d4 {
replace `i' = 1
}

foreach i in educpar1d1 educpar1d2 wi14d1 wi14d2 wi14d3 {
replace `i' = 0
}

predict predsch_uh2, xb
gen p_pred_uh2 = exp(predsch_uh2)/(1 + exp(predsch_uh2))  // Fixed effects value
sum p_pred_uh2


* Everyone low status and with adversity

foreach i in hhdeathill1 deathill2 {
replace `i' = 1
}

foreach i in educpar1d1 wi14d1 {
replace `i' = 1
}

foreach i in educpar1d2 educpar1d3 wi14d2 wi14d3 wi14d4  {
replace `i' = 0
}

predict predsch_uh3, xb
gen p_pred_uh3 = exp(predsch_uh3)/(1 + exp(predsch_uh3))  // Fixed effects value
sum p_pred_uh3




























**********************************
*** Shock and Wealth counterfactuals

// Dropout at ages 14-15 - using wealth counterfactuals too
foreach i in badevent1 badevent2 {
drop `i'
} 
foreach i in badevent1 badevent2 {
rename real`i' `i'
}

meqrlogit enrschr3 rmath2 badevent1 badevent2 stunted1 chores1 childwork1 order1 ageyr1 sex1 typesite1 i.educpar1 wi14d* ///
	ntotal1 clustsch1 i.cntry1 || nclustid: , mle

foreach i in badevent1 badevent2 wi14d1 wi14d2 wi14d3 wi14d4  {
gen real`i' = `i'
}

//Generating scores for those that faced economic shocks and belonged to the poorest quartile
foreach i in badevent1 badevent2 wi14d1 {
replace `i' = 1
}

predict predsch_uh1a, xb
gen p_pred_uh1a = exp(predsch_uh1a)/(1 + exp(predsch_uh1a))  // Fixed effects value
sum p_pred_uh1a
dis 1 - r(mean)

//Generating scores for the wealthy and no shocks
foreach i in badevent1 badevent2 wi14d1 wi14d2 wi14d3 {
replace `i' = 0
}

predict predsch_h1b, xb
gen p_pred_h1b = exp(predsch_h1b) / (1 + exp(predsch_h1b))
summarize p_pred_h1b // Fixed effects value
dis 1 - r(mean)

** Doing it separately for those that did not face shocks and are in the wealthiest quartile

foreach i in badevent1 badevent2 wi14d1 wi14d2 wi14d3 wi14d4 {
drop `i'
}

foreach i in badevent1 badevent2 wi14d1 wi14d2 wi14d3 wi14d4 {
rename real`i' `i'
}

meqrlogit enrschr3 rmath2 badevent1 badevent2 stunted1 chores1 childwork1 order1 ageyr1 sex1 typesite1 i.educpar1 wi14d2 wi14d3 wi14d4 wi14d1 ///
	ntotal1 clustsch1 i.cntry1 || nclustid: , mle
//Generating scores for the wealthy and no shocks
foreach i in badevent1 badevent2 {
replace `i' = 0
}
replace wi14d4 = 1
predict predsch_h1c, xb
gen p_pred_h1c = exp(predsch_h1c) / (1 + exp(predsch_h1c))
summarize p_pred_h1c // Fixed effects value
dis 1 - r(mean)




