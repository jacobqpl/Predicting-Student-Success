*******EDUC 799 Assignment3********
*Created by Peilin Qiu
*October 7, 2019
clear all
set more off, permanently
set matsize 5000
cap log close
set linesize 80	

use "D:\EDUC 799\Assignment2\Iowa Data File.dta"
ssc install missing

**Question 1
*Data Cleaning
generate id=_n	
order id

codebook enroll
tab enroll
replace enroll=. if app==0
replace enroll=. if admit==0 
// or coding it as keep if admit
tab enroll, missing
tab enroll

*Logistic Model
logit enroll actcomp underrep pubhs female hsrpct, robust
logit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust //it is the same!
outreg2 using "`Table'Enrollment Logit.doc", replace ctitle("Raw coefficients") 
estimate store logit_1

*Probit Model
probit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust //it is the same!
outreg2 using "`Table'Enrollment Probit.doc", replace ctitle("Raw coefficients") 
estimate store probit_1

*Difference between Logit and Probit
display -0.0575/-0.0358 //the result is 1.6061

**Question 2
logit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust 
estimate restore logit_enroll
cap drop pr_enroll

keep if admit
logit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust 
cap drop pr_enroll
predict pr_enroll if admit==1, pr //pr for logit, xb for LPM.
sum pr_enroll
sum pr_enroll, d
histogram pr_enroll, width(0.01) frequency scheme(s1mono)

**Question 3
*I don't need to write code for this question.

**Question 4
findit listcoef
logit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust 
listcoef
logit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust or 
listcoef, percent
margins, dydx (actcomp underrep pubhs female hsrpct) 
margins, dydx (actcomp underrep pubhs female hsrpct) asobserved //The default is asobserved

**Question 5
keep if admit
sum actcomp,d
help mchange
mchange actcomp, at(actcomp==12) delta(7) brief asobserved
mchange actcomp, at(actcomp==20) delta(5) brief asobserved
mchange actcomp, at(actcomp==26) delta(10) brief asobserved

**Question 6
margin, at(actcomp=(12(1)36)) asobserved
marginsplot, title("Predicted Probabilities for Enrollment by ACT Composite Score") ///
			scheme(s1mono) ///
			note("Note: All other values set as observed")
			graph export Graph1.png,wid(800)hei(600)

**Question 7
margin, at(actcomp=(12(1)36) female=1 underrep=1 pubhs=1 hsrpct=85) 
marginsplot, title("Predicted Probabilities for Enrollment by ACT Composite Score") ///
			scheme(s1mono) ///
			note("Note: For underrepresented female students from public high schools with high school rank percentiles of 85")
			graph export Graph2.png,wid(800)hei(600)

**Question 8
margins, at(underrep==1 female==1 pubhs==1) atmeans

sum hsrpct actcomp
sum hsrpct, d
sum actcomp, d
   
**Question 9 //This question has been coded in Question 1 part.
*Logistic Model
logit enroll actcomp underrep pubhs female hsrpct, robust
logit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust //it is the same!
outreg2 using "`Table'Enrollment Logit.doc", replace ctitle("Raw coefficients") 
estimate store logit_1

*Probit Model
probit enroll c.actcomp i.underrep i.pubhs i.female c.hsrpct, robust //it is the same!
outreg2 using "`Table'Enrollment Probit.doc", replace ctitle("Raw coefficients") 
estimate store probit_1

*Difference between Logit and Probit
display -0.0575/-0.0358 //the result is 1.6061

**Question 10

