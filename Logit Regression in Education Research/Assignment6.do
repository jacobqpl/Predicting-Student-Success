*****Assignment6*****
*****Created by Peilin Qiu***

***************************************************************************
***** STANDARD HEADERS *****
***************************************************************************
	clear all
	set more off, permanently
	set matsize 5000
	
***************************************************************************
***** SETUP WORKING DIRECTORY *****
***************************************************************************
	cd
	use "D:\EDUC 799\Assignment6\grade8-coded.dta"
	
***************************************************************************
***** (2) CLEAN THE VARIABLES *****
***************************************************************************	
*	Overview of data
	d, short
		// 107 vars, 12,144 obs	
	d

	numlabel, add
	
* (2.a) Clean outcome variable (f2s59b)
	codebook f2s59a
	
	gen collegeexp=f2s59a
	// generate a more readable variable; leave raw variables for backup
	
	replace collegeexp=. if(f2s59a>3)
	// drop missing cases; may explore these cases later
	// 2,180 obs dropped

* (2.b) Clean independent variables
		
	* race
		gen urm=(race==2 | race==3 | race==5)
		replace urm=. if(race>5)
		label var urm "URM=1"
		
	* SES
		gen ses=byses
		label var ses "SES"
		replace ses=. if(byses>20)

	* academic performance (standardized test composite of reading, math)
		codebook by2xcomp
		sum by2xcomp, d
		gen test=by2xcomp if(by2xcomp>0)
		replace test=. if(by2xcomp>80)
		
		label var test "Standardized test score in 8th grade"

* (2.c) Clean other covariates

	* female
		gen female=(sex==2)
		
		label var female "female=1"
		replace female=. if(sex>2)
		
		tab female sex, m

	* family size
		codebook byfamsiz
		tab byfamsiz
	
		gen fsize=byfamsiz
		replace fsize=. if(byfamsiz>10)
		
		label var fsize "Family size"
		
	* r talked to school rep about fin aid
		codebook f2s58b	
		gen talkaid=f2s58b
		replace talkaid=. if (f2s58b>2)
		recode talkaid (1=1) (2=0)
		tab talkaid, m
			
	* urbanicity (school)
		codebook g8urban
		tab g8urban
		tab g8urban, gen(x)
		rename x1 urban
		rename x2 suburban
		rename x3 rural
		
		foreach x in urban suburban rural {
			replace `x'=. if(g8urban>3)
		}
		
		gen urbanicity=g8urban
		replace urbanicity=. if(g8urban>3)
		
		label define urbanicity 1"urban" 2"suburban" 3"rural" 
		label val urbanicity urbanicity
		label var urbanicity "Urbanicity of the school. Missing/skip system missing"
		
	* region (school)
		codebook g8regon
		/*
		
            tabulation:  Freq.   Numeric  Label
                         2,185         1  1. northeast
                         3,092         2  2. north central
                         3,819         3  3. south
                         2,269         4  4. west
                            19         8  8. {missing}
                           760         9  9. {legitimate skip/not in wave}
						   
*/

		gen region=g8regon
		replace region=. if(g8regon>4)
		
		label define region 1"northeast" 2"North central" 3"South" 4"west" 
		label val region region
		label var region "Region of school. Missing/skip system missing"
		
/* (2.d) Clean cluster, weight, FE, etc (if any)	

		egen cluster=group(region urbanicity), label
*/
		
***************************************************************************
***** Multinomial model estimation *****
***************************************************************************

* Global variables
	global outcome 	"collegeexp"	
	global cov		"urm ses test"
	global control	"female fsize suburban rural talkaid"	
	
* (3.a) Baseline multinomial logit model


	foreach outcome in $outcome {
	
		mlogit  `outcome' $cov $control,  	 baseoutcome(1)
			est store m_logit
			
		mlogit  `outcome' $cov $control, rrr baseoutcome(1)
			est store m_1
		
		// rrr = relative risk ratios
			// cl=command cluster
			// cluster = the variable cluster
			// baseoutcome = setting the base outcome for the comparison
			// est store = storing the model as "m1a"
			
		mlogit  `outcome' $cov $control, rrr baseoutcome(2)
			est store m_2
			
		mlogit  `outcome' $cov $control, rrr baseoutcome(3)
			est store m_3
			
	}
	
		esttab m_1 m_2 m_3, eform compress wide		
		
		/*Interpretation:
			FOR ESTTAB! You should be looking down the columns.  Not across the columns
			
			Categorical: Females students have odds of saying college expenses 
			are somewhat important relative to not important that are 1.17 times
			that of male students. In other words, compared to males, female students 
			are more likely to say that college expenses are somewhat important versus not 
			important. 
			
			Continuous: For every one point increase in SES composite score,
			the odds of saying that college expenses are somewhat important 
			relative to not important change by a factor of 0.68. Said differently,
			as SES increases, students are less likely to say that college expenses
			are somewhat important compared to not important. */
		
		//Do interpretation here as well! 
				
		// change the baseoutcome if you want other comparisions
		
		outreg2 [m_1 m_2 m_3] using "mlogit1.xls", replace dec(3) eform
		
		
		ologit $outcome $cov $control , or
		outreg2 using "ologit1.doc", replace dec(3) eform

		
		/*descriptives in the sample*/
	cap drop insample
	mark insample
    markout insample collegeexp urm ses test female fsize suburban rural talkaid
    keep if(insample==1)
	
	d,short 
	foreach x in collegeexp urm female suburban rural talkaid{
			tab `x', m
		}
	sum ses test fsize 
		
		
		//Do interpretation here as well! 
		
	/*	1not/2some/3very
	
		compared to male, female students increase the relative risk ratio by 18%(by a factor of 1.178) /56% of saying college expenses somewhat/very important
		compared to not important, holding all other vars constant, statistically significant.
		
		// change the baseoutcome if you want other comparisions
		outreg2 [m_1 m_2 m_3] using "reg.xls", replace dec(3)
	*/

* (3.b) listcoef - pairwise comparison
		
		// mlogit without clustered s.e.
		mlogit $outcome $cov $control,  baseoutcome(1)
		
		//Just for female
		listcoef female, help
		
		// all possbile comparisons/contrasts
		listcoef, help
		
		/*Interpretation:
			
			Categorical: For URM students, the odds of saying that college expenses 
			are not important relative to very important are 0.78 times that of 
			non-URM students. In other words, URM students are less likely to say
			that college expenses are not important relative to very important compared
			to non-URM students. 
			
			Continuous: For every one point increase in SES composite score,
			the odds of saying that college expenses are not important 
			relative to very important change by a factor of 2.16.  In other words, 
			as SES increases, students are more likely to say that college expenses
			are not important compare to very important. */
		
		//LOTS of interpretation - 
		
* (3.c) mlogtest 
	
	* - joint significance test of coefficients
	* - same X variable across different outcomes
		
		// one step
		mlogtest, lr
		mlogtest, wald
		
		/*  mlogtest computes three types of tests for the multinomial logit model.  
		First, for each regressor it can perform an LR or Wald test of the 
		hypothesis that all coefficients for each regressor are zero. Sets of 
		variables can be tested simultaneously.  Second, it computes the 
		Hausman-McFadden test of the independence of irrelevance alternatives (IIA), 
		the Small-Hsiao test, or the SUEST version of the Hausman-McFadden test.  
		Third, it computes Wald and LR ratio tests of whether pair of outcome 
		categories can be combined.  Multiple tests are available. 
		
		lr          Compute LR tests for each regressor.

		wald        Compute Wald tests for each regressors. */

		
		//https://www3.nd.edu/~rwilliam/stats3/Mlogit2.pdf <--good resource for what is going on with these two tests
		
	* - tests for combining alternative outcomes
	* - all X variables for one outcome
	
		// all
		mlogtest, combine
		mlogtest, lrcombine
		
		// test outcome 2 vs. baseoutcome 1
		test [2]
		
		// test outcomes 2 vs. 3
		test [2=3]

		
* (3.d) predicted probability
		cap drop yhat1 yhat2 yhat3
		predict yhat1 yhat2 yhat3, pr
		
		sum yhat1 yhat2 yhat3
		
* (3.e) margins - marginal effects

		margins, dydx(*)
		/*Interpretation:
			Categorical: URMs predicted probability of saying that college expenses are not 
			important is 1.6 percentage points lower than non-URMs all else held as observed. 
			
			Continuous: For every one point increase in SES composite, the predicted
			probability of saying that college expenses are not important increases
			by 8.2 percentage points with all else held as observed in the model. */
		margins, dydx(*) predict(outcome(1))
		margins, dydx(*) atmeans
	
		
* (3.f) prediction figures

/* Global variables
	global outcome 	"collegeexp"	
	global cov		"urm ses test"
	global control	"female fsize suburban rural"	*/

mlogit collegeexp i.urm c.ses c.test i.female c.fsize i.suburban i.rural i.talkaid
	

/* 7. Plot the predicted probabilities for one outcome category 
(e.g., very important) and two groups (e.g., male/female) over the range of 
some continuous variable (or quasi-continuous, such as income) plotted on the X-axis.*/

margins i.female, at(test=(30(5)80)) asobserved predict(outcome(3))
marginsplot
graph export Graph1.png,wid(800)hei(600)


/* 8. Plot the predicted probabilities for all outcome categories for just one 
group (e.g., females only), using the same variable on the X-axis as in the 
first graph you produced.*/

margins, at(test=(30(5)80) female==1) asobserved
marginsplot
graph export Graph2.png,wid(800)hei(600)


* (3.g) compare mlogit vs. ologit

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

			
***************************************************************************
***** END OF DO FILE *****
***************************************************************************		

* Close the log file
	log close _all



































