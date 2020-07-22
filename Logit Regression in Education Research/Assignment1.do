clear all
set more off, permanently
set matsize 5000
cap log close
set linesize 80	

use "D:\EDUC 799\Assignment1\Iowa Data File.dta"

********************************************************************************
***Exploring the dataset
describe //obs: 61,811  vars: 164  size: 14,834,640
describe, short

***Attaching a unique id to each observation
capture drop id				
generate id=_n			
list id in 1/20

***Labeling the index score variable
lookfor index //Index=high school rank percentile+2*ACT composite score
codebook indexscr
list indexscr in 1/20
des indexscr
sum indexscr
label var indexscr "Index=high school rank percentile+2*ACT composite score"
notes indexscr: Index=high school rank percentile+2*ACT composite score
***Sorting the data
sort indexscr

***Listing specific observations
list in 10

********************************************************************************
*Question 1
describe, short

*Question 2
tabulate app
tab admit if (app==1)
tab enroll if (admit==1)

*Question 3
sum admit if (app==1)
sum actcomp hsrpct female privhs alum pardeg
sum actcomp if (app==1) 
*Question 4
sum enroll if(app==1) //The total number is 24902.
tab enroll underrep if(app==1) , row // table 1 for the paper

*Question 5
tab enroll underrep if (app==1), chi2 exp
tab enroll underrep, chi2 exp

*Question 6
sum hsrpct if(app==1)
lookfor private
codebook privhs
sum privhs if(app==1)
lookfor alumni
codebook alum
sum alum if(app==1)
lookfor pardeg
codebook pardeg
sum pardeg if(app==1)
sum female if(app==1)
sum underrep if(app==1)

areg admit c.actcomp c.hsrpct i.female i.underrep c.privhs i.alum i.pardeg if app==1, a(hszip5) robust 
outreg2 using "Table2_${date}.doc"

*Question 7 
areg admit c.actcomp c.hsrpct i.female i.underrep c.privhs i.alum i.pardeg if app==1, a(hszip5) robust 
cap drop pr_admit1
predict pr_admit1, xb
sum pr_admit1, d
replace pr_admit1=. if app==0

*Question 8
count if((pr_admit1<0 | pr_admit1>1) & app==1)
cap drop problem
gen problem=(pr_admit1<0 | pr_admit1>1)
tab problem if(app==1)
sum pr_admit1 if problem==1 & app==1, d




















