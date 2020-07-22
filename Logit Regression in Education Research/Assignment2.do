clear all
set more off, permanently
set matsize 5000
cap log close
set linesize 80	

use "D:\EDUC 799\Assignment2\Iowa Data File.dta"

***Question 1
generate id=_n	
order id

*DV
codebook enroll
tab enroll
replace enroll=. if app==0
replace enroll=. if admit==0
tab enroll, missing
tab enroll

*IV
codebook actcomp hsrpct female underrep pubhs pubcol
sum actcomp hsrpct

*Logit Regression
logit enroll actcomp hsrpct female underrep pubhs pubcol, robust
outreg2 using "Table'Admission Logit.doc", replace

***Question 2
logit enroll actcomp hsrpct female underrep pubhs pubcol, robust or
outreg2 using "Table'Admission Logit.doc", append ctitle("OR") eform

***Question 3


***Question 4




