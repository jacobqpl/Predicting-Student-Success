***Assignment 5****
use "D:\EDUC 799\Assignment5\lab 8\grade8-coded.dta"
set matsize 5000

*****Overview of data
	d, short
		// 107 vars, 12,144 obs	
	d
	
*****CLEAN THE VARIABLES *****
*Clean the dependent variable
	codebook f2s59a
	numlabel, add
	tab f2s59a
/*
.         tab f2s59a

       how important are college |
                        expenses |      Freq.     Percent        Cum.
---------------------------------+-----------------------------------
                1. not important |      2,028       16.70       16.70
              2. some importance |      4,962       40.86       57.56
               3. very important |      2,981       24.55       82.11
              6. {mult response} |          1        0.01       82.11
                    8. {missing} |        487        4.01       86.12
9. {legitimate skip/not in wave} |      1,685       13.88      100.00
---------------------------------+-----------------------------------
                           Total |     12,144      100.00  */
	
	gen expimp=f2s59a
	replace expimp=. if(f2s59a>3)	
	tab expimp f2s59a, m

*Clean the independent variables
	
	* race
		lookfor race
		codebook race

		
		gen urm=(race==2 | race==3 | race==5)
		replace urm=. if(race>5)
		// you may use detailed racial groups
		
		label var urm "URM=1"
		
		tab urm race, m
		
	* SES ///要不要drop这个变量？？
		lookfor ses
		codebook byses
		
		sum byses, d
		
		gen ses=byses
		
		label var ses "SES"
		
		xtile sesq=byses if(byses<=4), nq(4)
		
		tab sesq bysesq //which one would you go with? and why?
		
		label var sesq "SES quartiles"
		
	* academic performance (standardized test composite of reading, math)
		codebook by2xcomp
		sum by2xcomp, d
		
		gen test=by2xcomp
		replace test=. if(by2xcomp<0)
		sum test, d
		label var test "Standardized test score in 8th grade"
		
*Clean other covariates

	* female
		codebook sex

		gen female=(sex==2)
		replace female=. if(sex>3)
		
		label var female "female=1"
		
	* r talked to school rep about fin aid
		codebook f2s58b	
		gen talkaid=f2s58b
		replace talkaid=. if (f2s58b>2)
		recode talkaid (1=1) (2=0)
		tab talkaid, m
		
	* family size
		codebook byfamsiz
		tab byfamsiz
		
		gen fsize=byfamsiz
		replace fsize=. if(byfamsiz>10)
		
		label define fsize 1"1" 2"2" 3"3" 4"4" 5"5" 6"6" 7"7" 8"8" 9"9" 10"10+"
		label val fsize fsize
		
		label var fsize "Family size (top coded at ten)"
		
		tab fsize byfamsiz, m
		
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
		

	* Omitted variables?
		*other high school variation? (free lunch, minority, academic, etc.)	
	
		* other individual variation? (age, experience)
		
* (2.d) Clean cluster, weight, FE, etc (if any)	
		cap drop cluster
		egen cluster=group(region urbanicity), label
		tab cluster /// tab cluster, gen(new variable)
		tab cluster, gen(temp)
		tab temp1
		
mark insample
markout insample expimp urm ses test female fsize suburban rural talkaid cluster

keep if(insample==1)

sum expimp urm ses test female fsize suburban rural talkaid

***** (3) Ordered model estimation *****

* Global variables
	global outcome 	"expimp"	
	global cov		"urm ses test"
	global control	"female fsize suburban rural talkaid"	


* (3.a) Baseline model & Covariates
	ssc install outreg2
	foreach outcome in $outcome {
	
		ologit `outcome' $cov, or 
			est store m1
			
		ologit `outcome' $cov, or robust
			est store m2
			
		ologit `outcome' $cov, or cl(cluster)	
			est store m3

		ologit `outcome' $cov $control, or 	
			est store m4
			
		ologit `outcome' $cov $control, or cl(cluster)	
			est store m5

          }
		
		outreg2 [m1 m2 m3 m4 m5] using "ologit_1_$date.doc", replace dec(3) eform
			erase "ologit_1_$date.txt"		
			
		help esttab// click and install
		esttab m1 m2 m3 
		esttab m1 m2 m3, eform	 
		
		esttab m4 m5
		esttab m4 m5, eform 
		
		estimates restore m5

		listcoef
		listcoef, std
		
		lr 
		
* marginal effects /// 
		margins, dydx(*) /*average marginal effects*/
		margins, dydx(*) asobserved /*average marginal effects*/
		margins, dydx(*) atmeans /*marginal effects at the means*/
			

***** (4) Ordered model post-estimation *****


* (4.a) Model check
	* Model
	ologit expimp $cov $col, cl(cluster)	
		est store baseline

	* Goodness of fit
		fitstat

	* Test the parallel assumption (brant test) - IN ASSIGNMENT! 
		brant
		brant, detail
	
* (4.b) Prediction
	* Check with original distribution
	ologit expimp $cov, cl(cluster)
		predict not_import some_import v_import, pr
		
		histogram not_import
		histogram some_import
		histogram v_import
		
		sum not_import some_import v_import
		dotplot   not_import some_import v_import


* (4.c) Prediction tables
	* Table using mtable
		ologit expimp i.urm c.test c.ses, cl(cluster)	
		
		margins, at(urm=(0 1)) asobserved
		marginsplot


		sum ses
		mtable, at(urm=(0 1) ses=(-2(1)1)) asobserved stat(ci)
		mtable, at(urm=(0 1) ses=(-2(1)1)) atmeans stat(ci)	
			
	* mchange
		mchange, atmeans
		
* (4.d) Prediction figures
	preserve	
		* mgen
			mgen, at(test=(30(5)80)) stub(fig_) atmeans replace
			
		* graph w/o CI	
			sort test
			twoway  (connected fig_pr1 fig_pr2 fig_pr3 fig_test) 
				
		* graph w/ CI
		twoway  (line fig_pr1 fig_test)  									/// 
				(line fig_pr2 fig_test)  									/// 
				(line fig_pr3 fig_test, lcolor(navy) lpattern(longdash))  	/// 
				(rcap fig_ll1 fig_ul1 fig_test, lcolor(gs10))				///
				(rcap fig_ll2 fig_ul2 fig_test, lcolor(gs10))				///
				(rcap fig_ll3 fig_ul3 fig_test, lcolor(gs10)				///
			title("Importance of College Expenses in College Choice", 			///
				  size(medium) color(black) margin(zero)) 		 			///
			ytitle("Predicted probability", size(small))					/// 
			xtitle("Standardized test score", size(small))					///
			ylabel(0(.2).6, nogrid labs(small) angle(h))			 		///
			xlabel(30(10)80, angle(0) labs(small))							///
			legend(order(1 2 3) label(1 "Not important") 					/// 
			label(2 "Some important") label(3 "Very important") 			/// 
			label(4 "95% CI") cols(3) region(color(none)) margin(zero)) 	///
			graphregion(color(white)) bgcolor(none))	
	restore
	
* (4.e) Prediction figures (combine two figures)
	preserve
		// URM
		mgen, at(test=(30(5)80) urm=1) stub(fig_) atmeans replace
			
	twoway  (line fig_pr1 fig_test)  										/// 
			(line fig_pr2 fig_test)  										/// 
			(line fig_pr3 fig_test, lcolor(navy) lpattern(longdash))  		/// 
			(rcap fig_ll1 fig_ul1 fig_test, lcolor(gs10))					///
			(rcap fig_ll2 fig_ul2 fig_test, lcolor(gs10))					///
			(rcap fig_ll3 fig_ul3 fig_test, lcolor(gs10)					///
		title("URM", size(medium) color(black) margin(zero)) 		 		///		
		ytitle("Predicted probability", size(small))						/// 
		xtitle("Standardized test score", size(small))						///
		ylabel(0(.2).8, nogrid labs(small) angle(h))			 			///
		xlabel(30(10)80, angle(0) labs(small))								///
		legend(order(1 2 3) label(1 "Not important") 						/// 
		label(2 "Some important") label(3 "Very important") 				/// 
		label(4 "95% CI") cols(3) region(color(none)) margin(zero)) 		///
		graphregion(color(white)) bgcolor(none))	
		
		graph save tmp1.gph, replace
		
		
		// Non-URM
		mgen, at(test=(30(5)80) urm=0) stub(fig_) atmeans replace		
		
	twoway  (line fig_pr1 fig_test)  										/// 
			(line fig_pr2 fig_test)  										/// 
			(line fig_pr3 fig_test, lcolor(navy) lpattern(longdash))  		/// 
			(rcap fig_ll1 fig_ul1 fig_test, lcolor(gs10))					///
			(rcap fig_ll2 fig_ul2 fig_test, lcolor(gs10))					///
			(rcap fig_ll3 fig_ul3 fig_test, lcolor(gs10)					///
		title("Non-URM", size(medium) color(black) margin(zero)) 		 	///	
		ytitle("Predicted probability", size(small))						/// 
		xtitle("Standardized test score", size(small))						///
		ylabel(0(.2).8, nogrid labs(small) angle(h))			 			///
		xlabel(30(10)80, angle(0) labs(small))								///
		legend(order(1 2 3) label(1 "Not important") 						/// 
		label(2 "Some important") label(3 "Very important") 				/// 
		label(4 "95% CI") cols(3) region(color(none)) margin(zero)) 		///
		graphregion(color(white)) bgcolor(none))
		
		graph save tmp2.gph, replace
		cd
		
		// combine two figures
		graph combine tmp1.gph tmp2.gph, cols(2) scale(0.9)					/// 
			title("Importance of College Expenses in College Choice", 			/// 
			size(medium) color(black) margin(zero)) 						///
			graphregion(color(white))
			
		// combine two figures - common legend
		grc1leg tmp1.gph tmp2.gph, cols(2) scale(0.9)						/// 
			title("Importance of College Expenses in College Choice", 			/// 
			size(medium) color(black) margin(zero)) 						///
			subtitle(" ") graphregion(color(white))	
			
			erase tmp1.gph
			erase tmp2.gph
	restore	

***	
estimates restore m5
mtable, at(urm=(0 1)) asobserved stat(ci)
mtable, at(talkaid=(0 1)) asobserved



* Close the log file
	log close _all
	translate lab8_$date.smcl lab8_$date.pdf, replace
	
* Drop temp data
	sysuse census, clear	/* clear existing data changes, using sys data */
	erase nels_analysis.dta
	clear

* Putting the exit command at the end of a dofile is a good idea.
	exit
			

