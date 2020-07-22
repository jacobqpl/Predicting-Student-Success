*===============================================================================
**Early Momentum Study
**Created by: Peilin Qiu
**Date: October 15
*===============================================================================

	cls
	clear all
	version 14
	set more off, permanently
	capture log close
	set matsize 5000

***************************************************************************
***** SETUP WORKING DIRECTORY *****
***************************************************************************
	cd
	use "D:\EM Study\MyData3.dta"
	cd "D:\EM Study\"

	
***************************************************************************
***** Data Cleaning: Clean the dependent variables *****
***************************************************************************
	browse
	describe
	sum
	
**Next term (winter) enrollment
	sum winterenl_new 
	tab winterenl_new 
	dis 2430/3214 //the result is 76%


**Summer enrollment
	sum summenl_new 
	tab summenl_new 
	dis 894/3214 //the result is 28&

**Max degree in six years
	des maxofdeg
	tab maxofdeg 
/*
. tab maxofdeg

   MaxOfDeg |      Freq.     Percent        Cum.
------------+-----------------------------------
       Assc |        308        9.58        9.58
   Bachelor |        540       16.80       26.38
       Cert |        356       11.08       37.46
      NoDeg |      2,010       62.54      100.00
------------+-----------------------------------
      Total |      3,214      100.00

*/
	cap drop deg
	gen deg=.
	replace deg=1 if maxofdeg=="NoDeg"
	replace deg=2 if maxofdeg=="Cert"
	replace deg=3 if maxofdeg=="Assc"
	replace deg=4 if maxofdeg=="Bachelor"
	tab deg
	sum deg
	des deg

***************************************************************************
***** Data Cleaning: Clean the independent variables *****
***************************************************************************


**Pass remedial courses
	*Pass Math
	sum pass_math
	tab pass_math
	recode pass_math (0=0) (1 2=1)
	**Pass reading
	sum pass_read
	tab pass_read
	recode pass_read (0=0) (1 2=1)
	*Pass writing
	sum pass_write 
	tab pass_write 
	recode pass_write (0=0) (1 2=1)
	
	*Pass English (combine both reading and writing)
	sum engcollpass
	tab engcollpass
	recode engcollpass (0=0) (1 2=1)
	tab engcollpass
	
	*Pass Math
	sum mathcollpass
	tab mathcollpass
	recode mathcollpass (0=0) (1 2=1)
	
	*Pass either English or Math
	des engmathcollpass
	sum engmathcollpass
	tab engmathcollpass
	
	*Winter Exclude Tranfer Students
	des wintertrans
	sum wintertrans
	tab wintertrans
	recode wintertrans (2 4 =.) (0=1), gen (winterexctrans)
	tab winterexctrans //3010 students left
	
	
	*Summer Exclude Tranfer Students
	des summtran
	sum summtran
	tab summtran
	recode summtran (1=.)(0=1), gen (summerexctrans)
	tab summerexctrans 
	tab summerexctrans if winterexctrans==1 //2958 students left
	
	
	
**First Term Credits Attempted
des semcratt
sum semcratt
hist semcratt
tab semcratt

	*Generate New Variable Called semcrattX to show the three category of first term credits attempted
	cap drop semcrattX
	recode semcratt (0.5/5.5=1) (6/12=2) (12.5/22.5=3), gen (semcrattX) 
	tab semcrattX
	hist semcrattX

	graph pie,over(semcrattX) plabel(_all percent)

**First Term College-level Credits earned
sum fallcollcredits
hist fallcollcredits
tab fallcollcredits

	*Generate New Variable Called fallcollcreditsX to show the three category of first term college-level credits earned
	cap drop fallcollcreditsX
	recode fallcollcredits (0/5.5=1) (6/12=2) (12.5/22.5=3), gen (fallcollcreditsX) 
	tab fallcollcreditsX
	hist fallcollcreditsX

	graph pie,over(fallcollcreditsX) plabel(_all percent)

**College-level Credits Earned in First Year
sum fyrcollcredit
hist fyrcollcredit
tab fyrcollcredit

	*Generate New Variable Called fallcollcreditsX to show the three category of first year college-level credits earned
	cap drop fyrcollcreditX
	recode fyrcollcredit (0/14.5=1) (15/24=2) (24.5/61.5=3), gen (fyrcollcreditX) 
	hist fyrcollcreditX
	sum fyrcollcreditX
	
	graph pie,over(fyrcollcreditX) plabel(_all percent)


***************************************************************************
***** Data Cleaning: Clean the control variables *****
***************************************************************************

*female
sum sex
des sex

cap drop female
gen female=.
replace female=1 if sex=="F"
replace female=0 if sex=="M"

sum female
tab female


*race //Generate dummy variables
lookfor eth
codebook ethx

tab ethx,gen(x)
rename x1 asian
rename x2 black
rename x3 hispanic
rename x4 other
rename x5 unknown

cap drop white
rename x6 white

tab asian, m

sum asian
sum black
sum hispanic
sum other
sum unknown
/*
  tabulation:  Freq.  Value
                            98  "Asian"
                           620  "Black"
                           117  "Hispanic"
                           146  "Other"
                           114  "Unknown"
                          2119  "White"
*/



*age
lookfor age
des agex
tab agex

cap drop ageX
gen ageX=.
replace ageX=1 if agex=="<19"
replace ageX=2 if agex=="19-24"
replace ageX=3 if agex=="25-50"

tab ageX
des ageX
sum ageX

*pell
lookfor pell
des pell
sum pell
tab pell


*FTEIC
des ftiac 
sum ftiac
tab ftiac

*GPA
lookfor gpa
sum semgpa
tab semgpa
hist semgpa

cap drop semgpaX
recode semgpa (0/2.99375=0) (3/4=1), gen (semgpaX) 
hist semgpaX
tab semgpaX
sum semgpaX
*****Create new data file
//save "MyData31.dta", replace

use "D:\EM Study\MyData31.dta", replace
cd "D:\EM Study\"


*create Mydata4 to exlude transfer students in winter and summer
//drop if winterexctrans==.
//drop if summerexctrans==.
//save "MyData4.dta", replace
//use "D:\EM Study\MyData4.dta"
	

***************************************************************************
***** Global Setting *****
***************************************************************************
//注意！经过我修改后，大多数的变量都变成了Categorical Variables,因此注意加i.variable


*Global outcome
global outcome1 "winterenl_new"
global outcome2 "summenl_new"
global outcome "deg"

*Global Covariates
global cov1 "pass_math pass_read pass_write"
global cov11 "engcollpass mathcollpass"
global cov12 "engmathcollpass"

global cov2 "i.semcrattX"

global cov3 "i.fyrcollcreditX"
global cov31 "i.semcrattX i.fyrcollcreditX" //Multicollinearity?
global cov32 "i.semcrattX i.fyrcollcreditX engcollpass mathcollpass"
global cov33 "i.semcrattX i.fyrcollcreditX engmathcollpass"


global control "female asian black hispanic other unknown i.ageX pell ftiac semgpa"
global control1 "female asian black hispanic other unknown i.ageX pell ftiac semgpaX"



//Regression Models

***************************************************************************
***** Research Question 1 Model 1 Mydata31 *****
***************************************************************************	
logit $outcome1 $cov1 $control, robust
	est store m1
outreg2 using "logit.doc", replace ctitle("Raw coefficients") 

logit $outcome1 $cov1 $control, robust or
	listcoef, percent
	est store m11

outreg2 using "logit.doc", append ctitle("Odds Ratio") 	eform

margins, dydx(*)

	cap drop pr_winterpersist
	predict pr_winterpersist, pr
	sum pr_winterpersist
	histogram pr_winterpersist, width(0.01) frequency scheme(s1mono)

*Alternative models for this model
//logit $outcome1 $cov11 $control, robust
//logit $outcome1 $cov12 $control, robust
//logit $outcome1 $cov1 $control1, robust
	
		
	/*Plot the predicted probabilities for all outcome categories for just one 
	group (e.g., females only), using the same variable on the X-axis as in the 
	first graph you produced.*/

	margins, at(semgpa=(0(0.05)4) female==1) asobserved
	marginsplot
	graph export Graph1.png,wid(800)hei(600)
	
	
	margins, at(semgpa=(0(0.05)4) pass_math==1) asobserved
	marginsplot //这里用graph editor编辑标题
	graph export Graph11.png,wid(800)hei(600) 
	graph save Graph "D:\EM Study\Graph11.gph", replace
	
	margins, at(semgpa=(0(0.05)4) pass_read==1) asobserved
	marginsplot
	graph export Graph12.png,wid(800)hei(600)
	graph save Graph "D:\EM Study\Graph12.gph", replace
	
	margins, at(semgpa=(0(0.05)4) pass_write==1) asobserved
	marginsplot
	graph export Graph13.png,wid(800)hei(600)
	graph save Graph "D:\EM Study\Graph13.gph", replace
	
	graph combine Graph11.gph Graph12.gph Graph13.gph, title("Predictive Margins of Remedial Courses with 95% CIs")
	//用graph editor 编辑legend: Predictive Margins of Remedial Courses
	graph save Graph "D:\EM Study\Graphmodel1.gph"
	
	
	*Model Test
		*LR Test
		mlogtest, lr
		*Wald Test
		mlogtest, wald

		mlogtest, combine
		mlogtest, lrcombine
	
	

//keep if  winterenl_new
//save "MyData32.dta", replace
***************************************************************************
***** Research Question 1 Model 2 Mydata32 *****
***************************************************************************	
use "D:\EM Study\MyData32.dta", replace
cd "D:\EM Study\"

logit $outcome2 $cov1 $control, robust
	est store m2


outreg2 using "logit.doc", append ctitle("Raw Coefficient") 
	
	cap drop pr_summerpersist
	predict pr_summerpersist, pr
	sum pr_summerpersist
	histogram pr_summerpersist, width(0.01) frequency scheme(s1mono)


keep if winterpersist==1	
logit $outcome2 $cov1 $control, robust or
	listcoef, percent
	
outreg2 using "logit.doc", append ctitle("Raw Coefficient") eform

*Alternative models for this model
//logit $outcome2 $cov11 $control, robust
//logit $outcome2 $cov12 $control, robust

margins, dydx(*)

/*Plot the predicted probabilities for all outcome categories for just one 
group (e.g., females only), using the same variable on the X-axis as in the 
first graph you produced.*/

margins, at(semgpa=(0(0.05)4) female==1) asobserved
marginsplot
graph export Graph3.png,wid(800)hei(600)

	margins, at(semgpa=(0(0.05)4) pass_math==1) asobserved
	marginsplot //这里用graph editor编辑标题
	graph export Graph21.gph,wid(800)hei(600) 
	graph save Graph "D:\EM Study\Graph21.gph", replace
	
	margins, at(semgpa=(0(0.05)4) pass_read==1) asobserved
	marginsplot
	graph export Graph22.gph,wid(800)hei(600)
	graph save Graph "D:\EM Study\Graph22.gph", replace
	
	margins, at(semgpa=(0(0.05)4) pass_write==1) asobserved
	marginsplot
	graph export Graph23.gph,wid(800)hei(600)
	graph save Graph "D:\EM Study\Graph23.gph", replace
	
	graph combine Graph21.gph Graph22.gph Graph23.gph, title("Predictive Margins of Remedial Courses with 95% CIs")
	//用graph editor 编辑legend: Predictive Margins of Remedial Courses
	graph save Graph "D:\EM Study\Graphmodel2.gph"


**LR Test
	mlogtest, lr
**Wald Test
	mlogtest, wald

	mlogtest, combine
	mlogtest, lrcombine
	
	
***************************************************************************
***** Research Question 2 Model 3 Mydata4 *****
***************************************************************************	

use "D:\EM Study\MyData4.dta", replace
cd "D:\EM Study\"

//此处注意还没有global, 要先回去global一下

mlogit $outcome $cov2 $control, baseoutcome(1)
est store m3

outreg2 using "mlogit.doc", replace ctitle("Raw coefficients") 

mlogit $outcome $cov2 $control, rrr baseoutcome(1)
outreg2 using "mlogitrrr.doc"

esttab m3, eform compress wide	


	/* Plot the predicted probabilities for all outcome categories for just one 
	group (e.g., females only), using the same variable on the X-axis as in the 
	first graph you produced.*/

	margins, at(semgpa=(0(0.5)24) female==1) asobserved
	marginsplot
	graph export Graph4.png,wid(800)hei(600)
	graph save Graph "D:\EM Study\Graphmodel3.gph"
	
	**LR Test
	mlogtest, lr
	**Wald Test
	mlogtest, wald

	mlogtest, combine
	mlogtest, lrcombine
	


***************************************************************************
***** Research Question 3 Model 4 Mydata4 *****
***************************************************************************
mlogit $outcome $cov3 $control, baseoutcome(1)
est store m4



mlogit $outcome $cov3 $control, rrr baseoutcome(1)
outreg2 using "mlogitrq3.doc"


*Alternative models for this model
//mlogit $outcome $cov31 $control, baseoutcome(1)
//mlogit $outcome $cov32 $control, baseoutcome(1)
//mlogit $outcome $cov33 $control, baseoutcome(1)

esttab m4, eform compress wide	


outreg2 [m3 m4] using "mlogit.xls", replace dec(3) eform

/* Plot the predicted probabilities for all outcome categories for just one 
group (e.g., females only), using the same variable on the X-axis as in the 
first graph you produced.*/

	margins, at(semgpa=(0(0.5)4) female==1) asobserved
	marginsplot
	graph export Graph5.png,wid(800)hei(600)
	graph save Graph "D:\EM Study\Graphmodel4.gph"
	
**LR Test
	mlogtest, lr
**Wald Test
	mlogtest, wald

	mlogtest, combine
	mlogtest, lrcombine
	
margins, dydx(*)
margins, dydx(*) predict(outcome(1))
margins, dydx(*) predict(outcome(2))
margins, dydx(*) predict(outcome(3))
margins, dydx(*) predict(outcome(4))

*Compare which model is better?



*Compare Different Models //以下是Meghan的命令

	* mlogit
	mlogit $outcome $cov $control
		est store mlogit1
		fitstat, save
		
	* ologit
	ologit $outcome $cov $control
		est store ologit1
		
		* model fit
			fitstat, diff
			// AIC and BIC supports different models
			// mlogit uses many more parameters
