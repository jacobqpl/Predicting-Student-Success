*****Assignment 4*****
**Created by Peilin Qiu
**Date： 10/15/2019

clear all
set more off, permanently
set matsize 5000
cap log close
set linesize 80	

use "D:\EDUC 799\Assignment4\Lab 6\Iowa Data File.dta"

**Data Cleaning
ssc install missing
generate id=_n	
order id
tab enroll
sum enroll,d
keep if admit==1 //it is same with keep if admit or keep if (admit==1)
tab enroll, missing

**Question 1 
	* generate new variable
	gen parincome=dadinc+mominc
	drop if parincome<=0
	cap drop subsample
	gen subsample=. 
	replace subsample=1 if(parincome!=.)	
	tab enroll subsample
	* estimate two models	
	logit enroll female underrep actcomp if(subsample==1) 
	estimate store restricted		
	outreg2 using "`Table'Enrollment_restricted.doc", replace ctitle("Restricted")
	
	logit enroll female underrep actcomp appaid totaid parincome
	estimate store unrestricted		
	outreg2 using "`Table'Enrollment_unrestricted.doc", replace ctitle("Unrestricted")
	
	*lrtest
	lrtest restricted unrestricted
	estimates restore unrestricted
	
**Question 2
	* Wald test	
	test actcomp
	test female underrep //testing whether the coefficients are zero.
	test female==underrep //testing whether the coefficients are the same

**Question 3
	* fitstat
	ssc install fitstat
	estimates restore restricted 
	fitstat, save					// fit statistics for a single mode	
	estimates restore unrestricted
	fitstat, diff			// compare models

**Question 4 //我还有问题。为啥list admit $cov if rstd<-20; 为啥冒出来个$cov?
	* check outliers and influential observations [LOGIT MODEL]	
	estimates restore unrestricted				
	predict rstd, rstandard			// standardized residual
	sort actcomp
	gen index = _n		
	twoway scatter rstd index
	list admit $cov if rstd<-20
	twoway scatter rstd index if rstd<-10 	

	predict deltabeta, dbeta		// influential obs
		twoway scatter deltabeta index
		twoway scatter deltabeta index if(deltabeta>6)
		
		list admit deltabeta rstd if rstd<-20

**Question 5 //下面的display的具体数据要一一更新
	* classification table [both logit and probit]
	
	estat classification, all //all does it for all observations
	
	display 5885/10059 //of those who truly enrolled, how many did the model classify as enroll (sensitivity)
	display 11423/12066  //of those who did not enroll, how many did the model classify as not enrolled (specificity)
	display 5885/6528 //of those classified as enrolled, how many actually enrolled
	display 11423/15597 // of those classified as not enrolled, who didn't enroll. 
	display (5885+11423)/22125 //correct classification
	
	estat classification, all cutoff(0.6) //cutoff changes the cut score
	
	*how could we get this into a table?
	return list

**Question 6
	* LROC & LSENS [both logit and probit]
	
	lroc, scheme(s2mono)
	/* A model with no predictive power would be a 45◦ line. The greater the 
	   predictive power, the more bowed the curve, and hence the area beneath 
	   the curve is often used as a measure of the predictive power. A model 
	   with no predictive power has area 0.5; a perfect model has area 1. */
	// see more http://www.stata.com/manuals13/rlroc.pdf 
	//Also note that you can put varisty code on to the lroc
	
	graph query, scheme
	
	lsens, scheme(burd) ///
			title("LSENS Graph")
	/* lsens plots sensitivity and specificity; it plots both sensitivity and 
	   specificity versus probability cutoff c. 
	   The graph is equivalent to what you would get from estat classification 
	   if you varied the cutoff probability c from 0 to 1. 				 */
	// see more http://www.stata.com/manuals13/rlsens.pdf
	//also note that you can put varisty code on the lsens

**图表...
