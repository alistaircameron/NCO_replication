/*
NOTE THAT THESE FILES REPLICATE ONLY THE TABLES AND GRAPHS OF THE MAIN PAPER. RANDOM NOISE HAS BEEN ADDED TO PARTICIPANT RESPONSES SO NO-ONE SCOOPS US! We plan on making the raw data available when we no longer risk being scooped.

Replication file for:
Does Public Redistribution Crowd Out Private Transfers? Evidence from Four Countries
Cameron, Gangadharan, Maitra, Santos, Vecci

Regressions are done in Stata, then, since we're interested in non-standard outputs, these outputs are then fed as text into python, where they're rearranged to produce latex tables. #stataisabitch.




The following packages need to be installed.
ssc install mat2txt
ssc install joy_plot
net describe ritest, from(https://raw.githubusercontent.com/simonheb/ritest/master/)
net install ritest
net get ritest
ssc install estout, replace
ssc install missings
net install cleanplots, from("https://tdmize.github.io/data/cleanplots")



Note that 'change' is the amount paid to increase/decrease/not_change partner's income. All other variables should be self-explanatory.
*/





cls  
clear all  
cap log close  
set mem 500M  
set more off, perm  
set graphics on  
cap set scheme cleanplots
graph set window fontface "Times New Roman"


use replication_set, replace


*** Table 3: Experimental Sample ***

label variable equality_effort "Equality vs. Effort"
label variable private_government "Private vs. Government"
label variable competition_good_harmful "Competition good vs. harmful"
label variable effort_luck "Effort vs. Luck"
label variable left_right "Left vs. Right"
label variable government_individual "Government vs. Individual"
label variable age "Age"
label variable female "Female"
label variable hh_size "Household Size"
label variable income_ladder "Income Ladder"
label variable mobile "Mobile"

estpost tabstat equality_effort private_government competition_good_harmful effort_luck left_right government_individual age female hh_size income_ladder mobile if iid == 1, by(country) statistics(mean sd) columns(statistics) listwise nototal
esttab using wvs_comparison.tex, main(mean) aux(sd) nostar unstack noobs nonote nomtitle replace nonumber b(%9.2f) eqlabels("Germany" "India" "Indonesia" "USA") label




*** Figure 3: Overview of Choices, Part 1. 

forvalues p = 1/2{

cap graph drop *
use replication_set, clear
keep if part == `p'
replace income_real_ppp = income_real_ppp / 1000000 // annual income in millions of dollars
* graph bar wrong_, over(treatment)

* Intensive margin (N.B. this is give_int, not give_int_weak, even though it's a = 0!!!):
g give_int = 0
g take_int = 0
replace give_int = change if change > 0
replace take_int = -change if change < 0

// cap drop iid
// bysort id: gen iid = _n

// Got to do things this way to get the confidence intervals (stupid fricken program): 
g int prosocial = change > 0
replace prosocial = -1 if change < 0


*		Extensive Margin with Zero-payment - Country.

preserve
g byte zero = change == 0

collapse (mean) meangive = give meantake = take meanzero = zero (sebinomial) setake = take segive = give sezero = zero, by (country)

// FIDDLING. 
save temp, replace
append using temp
save temp, replace 
append using temp

keep in 1/12

g meanchange = meangive
g sechange = segive

local j = 1
forvalues i = 5/8{
	replace meanchange = meantake[`j'] in `i'
	replace sechange = setake[`j'] in `i'
	local j = `j' + 1
}

local j = 1
forvalues i = 9/12{
	replace meanchange = meanzero[`j'] in `i'
	replace sechange = sezero[`j'] in `i'
	local j = `j' + 1
}


g prosocial = 1
replace prosocial = -1 in 5/8
replace prosocial = 0 in 9/12
replace country = country - 0.25 if prosocial==1
replace country = country + 0.25 if prosocial == -1
 
gen dpucl = meanchange + sechange
gen dplcl = meanchange - sechange
	
bysort country: gen ps1 = strofreal(meanchange, "%03.2f") if prosocial == 1
bysort country: gen ps2 = strofreal(meanchange, "%03.2f") if prosocial == -1	
bysort country: gen ps3 = strofreal(meanchange, "%03.2f") if prosocial == 0	

cap graph drop *	
graph twoway (bar meanchange country if prosocial==1,  barwidth(.4) color(blue) fintensity(60) ysc(r(0 .4)) ytick(0.1(0.1)0.5) ylabel(0.1(0.1)0.5)) ///
 (bar meanzero country if prosocial==0, barwidth(.4) color(green) fintensity(45)) ///
 (bar meanchange country if prosocial==-1, barwidth(.4) color(red) fintensity(30)) ///
 (rcap dpucl dplcl country if prosocial==0, blwid(thin) blcolor(black) ) ///
 (rcap dpucl dplcl country if prosocial==1, blwid(thin) blcolor(black) ) ///
 (rcap dpucl dplcl country if prosocial==-1, blwid(thin) blcolor(black) ) , ///
  xlabel(1 "Germany" 2 "India" 3 "Indonesia" 4 "USA", labsize(medium) axis(1)) ///
  xtitle("") ///
  ytitle("", size(medium)) ///
 legend(order(1 "Prosocial" 2 "No Payment" 3 "Antisocial" ) cols(3) size(medium) position(6)) graphregion(color(white) ) ///
 name(a) || scatter meanchange country if prosocial == 1, ms(none) mla(ps1) mlabpos(11) mlabcolor(blue) || scatter meanchange country if prosocial == -1, ms(none) mla(ps2) mlabpos(1)mlabcolor(pink) || scatter meanzero country if prosocial == 0, ms(none) mla(ps3) mlabpos(1) mlabcolor(green) aspectratio(0.7) // subtitle("Treatment Differences") 
 
graph save a`p', replace
graph export a`p'.png, replace 
 
restore


*		Extensive Margin with Zero-payment - Treatment.

preserve
g byte zero = change == 0

drop country
rename treatment country
collapse (mean) meangive = give meantake = take meanzero = zero (sebinomial) setake = take segive = give sezero = zero, by (country)


// FIDDLING. 
save temp, replace
append using temp
save temp, replace 
append using temp

keep in 1/15

g meanchange = meangive
g sechange = segive

local j = 1
forvalues i = 6/10{
	replace meanchange = meantake[`j'] in `i'
	replace sechange = setake[`j'] in `i'
	local j = `j' + 1
}

local j = 1
forvalues i = 11/15{
	replace meanchange = meanzero[`j'] in `i'
	replace sechange = sezero[`j'] in `i'
	local j = `j' + 1
}


g prosocial = 1
replace prosocial = -1 in 6/10
replace prosocial = 0 in 11/15
replace country = country - 0.25 if prosocial==1
replace country = country + 0.25 if prosocial == -1
 
gen dpucl = meanchange + sechange
gen dplcl = meanchange - sechange
	
bysort country: gen ps1 = strofreal(meanchange, "%03.2f") if prosocial == 1
bysort country: gen ps2 = strofreal(meanchange, "%03.2f") if prosocial == -1	
bysort country: gen ps3 = strofreal(meanchange, "%03.2f") if prosocial == 0	

cap graph drop *	
graph twoway (bar meanchange country if prosocial==1,  barwidth(.4) color(blue) fintensity(60) ysc(r(0 .4)) ytick(0.1(0.1)0.5) ylabel(0.1(0.1)0.5)) ///
 (bar meanzero country if prosocial==0, barwidth(.4) color(green) fintensity(45)) ///
 (bar meanchange country if prosocial==-1, barwidth(.4) color(red) fintensity(30)) ///
 (rcap dpucl dplcl country if prosocial==0, blwid(thin) blcolor(black) ) ///
 (rcap dpucl dplcl country if prosocial==1, blwid(thin) blcolor(black) ) ///
 (rcap dpucl dplcl country if prosocial==-1, blwid(thin) blcolor(black) ) , ///
  xlabel(1 "Anarchy" 2 "Tax" 3 "UBI" 4 "Effort" 5 "Luck", labsize(medium) axis(1)) ///
  xtitle("") ///
  ytitle("", size(medium)) ///
 legend(order(1 "Prosocial" 2 "Antisocial" ) cols(3) size(medium)) graphregion(color(white)) ///
 name(b) || scatter meanchange country if prosocial == 1, ms(none) mla(ps1) mlabpos(11) mlabcolor(blue) legend(off) || scatter meanchange country if prosocial == -1, ms(none) mla(ps2) mlabpos(1)mlabcolor(pink) legend(off) || scatter meanzero country if prosocial == 0, ms(none) mla(ps3) mlabpos(1) mlabcolor(green) legend(off) aspectratio(0.7) //   subtitle("Treatment Differences") 
 
graph save b`p', replace
graph export b`p'.png, replace 
 
restore




*		Intensive Margin  - Country.

preserve 
drop if prosocial == 0
replace change = abs(change)
collapse (mean) meanchange = change (sd) sdchange = change (count) n=change, by (country prosocial)			 
gen dpucl = meanchange + invttail(n-1,0.025)*(sdchange/sqrt(n))
gen dplcl = meanchange - invttail(n-1,0.025)*(sdchange/sqrt(n))
replace country = country - 0.3 if prosocial==1
bysort country: gen ps1 = strofreal(meanchange, "%03.2f") if prosocial == 1
bysort country: gen ps2 = strofreal(meanchange, "%03.2f") if prosocial == -1	
graph twoway (bar meanchange country if prosocial==1,  barwidth(.5) color(blue) fintensity(50) ysc(r(0 4)) ytick(1(1)4) ylabel(1(1)4)) ///
 (bar meanchange country if prosocial==-1, barwidth(.5) color(red) fintensity(40)) ///
 (rcap dpucl dplcl country if prosocial==1, blwid(thin) blcolor(black) ) ///
 (rcap dpucl dplcl country if prosocial==-1, blwid(thin) blcolor(black) ) , ///
  xlabel(1 "Germany" 2 "India" 3 "Indonesia" 4 "USA", labsize(medium) axis(1)) ///
  xtitle("") ///
  ytitle("", size(medium)) ///
 legend(order(1 "Prosocial" 2 "Antisocial" ) cols(3) size(medium) pos(6)) graphregion(color(white)) name(c) || scatter meanchange country if prosocial == 1, ms(none) mla(ps1) mlabpos(11) mlabcolor(blue)  || scatter meanchange country if prosocial == -1, ms(none) mla(ps2) mlabpos(1)aspectratio(0.7) //   subtitle("Treatment Differences") 
 
 graph save c`p', replace
 graph export c`p'.png, replace
restore

*		Extensive Margin with Zero-payment - Treatment.

preserve 
drop if prosocial == 0
replace change = abs(change)
drop country
rename treatment country
collapse (mean) meanchange = change (sd) sdchange = change (count) n=change, by (country prosocial)
gen dpucl = meanchange + invttail(n-1,0.025)*(sdchange/sqrt(n))
gen dplcl = meanchange - invttail(n-1,0.025)*(sdchange/sqrt(n))
replace country = country - 0.3 if prosocial==1
bysort country: gen ps1 = strofreal(meanchange, "%03.2f") if prosocial == 1
bysort country: gen ps2 = strofreal(meanchange, "%03.2f") if prosocial == -1	
graph twoway (bar meanchange country if prosocial==1,  barwidth(.5) color(blue) fintensity(50) ysc(r(0 4)) ytick(1(1)4) ylabel(1(1)4)) ///
 (bar meanchange country if prosocial==-1, barwidth(.5) color(red) fintensity(40)) ///
 (rcap dpucl dplcl country if prosocial==1, blwid(thin) blcolor(black) ) ///
 (rcap dpucl dplcl country if prosocial==-1, blwid(thin) blcolor(black) ) , ///
  xlabel(1 "Anarchy" 2 "Tax" 3 "UBI" 4 "Effort" 5 "Luck", labsize(medium) axis(1)) ///
  xtitle("") ///
  ytitle("", size(medium)) ///
 legend(order(1 "Prosocial" 2 "Antisocial" ) cols(3) size(medium)) graphregion(color(white)) ///
 name(d) || scatter meanchange country if prosocial == 1, ms(none) mla(ps1) mlabpos(11) mlabcolor(blue) legend(off) || scatter meanchange country if prosocial == -1, ms(none) mla(ps2) mlabpos(1) legend(off)  aspectratio(0.7) // subtitle("Country Differences")
 
 graph save d`p', replace
 graph export d`p'.png, replace
restore

}





*** Table 4: Transfer Frequency Relative to Anarchy. Part 1 

use replication_set, clear

g byte ad_ineq = inequality > 0
g byte disad_ineq = inequality < 0
g equal = inequality == 0	// comparison group, not used.

g equality = 1 if equal == 1
replace equality = 2 if disad_ineq == 1
replace equality = 3 if ad_ineq == 1

levelsof equality
label define eqlty 1 "equal" 2 "disad" 3 "ad"
label values equality eqlty


* The income controls aren't comparable across countries.
g income_control = .
forvalues i = 1/4{
	replace income_control = income_strata + `i'*100 if country == `i'
}

* There's multicolinearity b/w my country and income dummies. I want to keep the ibn.equality#ibn.country#ibn.treatment coefficients in the model. I don't care about the coefficients on the income dummies. So, I want stata to drop 1 income dummy from each country. I can't force it to do it nicely, so doing it grossly:

levelsof income_control, local(ic)
foreach l in `ic'{
	di `l'
	g dummy`l' = 0
	replace dummy`l' = 1 if income_control == `l'
}

* Drop 1 group from each country. 
drop dummy101 dummy201 dummy301 dummy401

* We use the time-taken variable to explain attrition. Therefore, we need to control for it in the regressions.
tostring duration, replace
replace timing = timing + ";" + duration
split timing, parse(";") generate(foo) destring
rename foo8 time_taken
drop foo*
	

* Give & Take frames; extensive and intensive; parts 1 & 2. Treatment effects.
// FE specification for the question order. i haven't interacted that with treatment. coz why.

forvalues p = 1/2{
foreach gt in give take {

	qui{
		
	reg `gt' ibn.equality#ibn.country#ibn.treatment wrong_answers c.wrong_answers#i.country female dummy* time_taken i.qorder if part == `p', vce(cluster id) nocons
	
	noi di "Part is: `p'; Variable is: `gt'"
	noi di "No. of obs is: `e(N)'; R-squared is: `e(r2)'"
	noi di "No. of clusters is: `e(N_clust)'"
	
	levelsof equality, local(ineq_levels)
	levelsof country, local(cntry_levels)
	levelsof treatment, local(trtmnt_levels)
	
	cap drop coeff
	cap drop pval
	cap drop stderr 
	g coeff = . 
	g pval = .
	g stderr = . 
	
	local i = 1
foreach e in `ineq_levels'{	
	foreach c in `cntry_levels'{		
		foreach t in `trtmnt_levels'{
			
			qui lincom `e'.equality#`c'.country#`t'.treatment - `e'.equality#`c'.country#1.treatment 
			qui replace coeff = `r(estimate)' in `i'
			qui replace pval = `r(p)' in `i'
			qui replace stderr = `r(se)' in `i'
			
* I need to extract the standard error even in the base case.			
			if `t' == 1 {
				qui lincom `e'.equality#`c'.country#`t'.treatment
				qui replace coeff = `r(estimate)' in `i' // Toggle this if you want to have 0's for Anarchy.
				qui replace stderr = `r(se)' in `i'
				
			}
			
			local i = `i' + 1
			
		}		
	}	
}

	}
	
outfile coeff stderr pval in 1/`i' using `gt'_reduced_`p', replace 
}
}



*** Table 5 ***

use replication_set, clear
keep if part == 1
drop iid
bysort id: gen iid = _n
g ipw = 1


* Giving and taking types. 
bysort id: egen sumgive = sum(give) 
bysort id: egen sumtake = sum(take) 
g giving_type = sumgive == 6 
g taking_type = sumtake == 6 

* Income maximisers
g income_maximisers = sumtake == sumgive & sumgive == 0 

* Initially, assuming everything as strict equalities.
g richer = income > p_income	
g poorer = income < p_income
	
g give_richer = .
replace give_richer = 1 if richer == 1 & change > 0
replace give_richer = 0 if richer == 1 & change <= 0

g take_poorer = .
replace take_poorer = 1 if poorer == 1 & change < 0
replace take_poorer = 0 if poorer == 1 & change >= 0 

* Now, to turn this into a type, aggregate to the individual level.
bysort id: egen count_richer = sum(richer)
bysort id: egen count_poorer = sum(poorer)

bysort id: egen count_give_richer = sum(give_richer)
bysort id: egen count_take_poorer = sum(take_poorer)


* The inequality is needed to eliminate the always poorer types
g ad_averse = count_richer == count_give_richer & count_richer > 0 // averse to advantageous inequality
g disad_averse = count_poorer == count_take_poorer & count_poorer > 0 // averse to disadvantageous inequality

* Averse to all types of inequality. Though, this may just be multiocolinear...
	* Note that if someone is inequality averse, they won't make a payment when there is perfect equality

g no_pay = 0
replace no_pay = 1 if income == p_income & change == 0
bysort id: egen nopay = max(no_pay)

g ineq_averse = ad_averse == disad_averse & disad_averse == 1 & nopay == 1	
	
di in red " To make these mutually exclusive, run the following two lines -- you f'd this up a while back, because of people who had 'no-pay' equal to 0... as it is now, it works"
replace ad_averse = 0 if disad_averse == 1 | ineq_averse == 1
replace disad_averse = 0 if ad_averse == 1 | ineq_averse == 1
	
egen ot = rowtotal(ad_averse disad_averse ineq_averse income_maximisers)
g other_type = (ot == 0)
drop ot
	

g int da = disad_averse == 1
summ da disad_averse


* * *  Now, explaining the differences by looking at the types across treatments. * * * 
* This needs to be run 4 times, changing the value of the local 'var' each time.


// local var = "income_maximiser"
// local var = "ineq_averse"
// local var = "disad_averse"
local var = "ad_averse"

keep if iid == 1

g variable = ""
local row = 1
foreach variable in Anarchy Tax UBI Effort Luck{
	replace variable = "`variable'" in `row'
	local row = `row' + 2
}

g variable_x = "&"


forvalues i = 1/4{
	
	di in red "Country is: `i'"
	
	g country`i' = . 
	format country`i' %9.2f
	g country`i'_x = "&"
	g pval_`i' = ""
	
	
	local row = 1
	local rowplus = `row' + 1
	summ `var' if country == `i' & treatment == 1
	replace country`i' = r(mean) in `row'
	replace country`i' = r(sd) in `rowplus'
		
	forvalues j = 2/5{
		
		local row = `row' + 2
		local rowplus = `rowplus' + 2
		
		summ `var' if country == `i' & treatment == `j'
		replace country`i' = r(mean) in `row'
		
		replace country`i' = r(sd) in `rowplus'

		prtest `var' if country == `i' & (treatment == 1 | treatment == `j'), by(treatment)		
		
		replace pval_`i' = "*" if r(p) <= 0.1 in `row'
		replace pval_`i' = "**" if r(p) <= 0.05 in `row'
		replace pval_`i' = "***" if r(p) <= 0.01 in `row'		
	}
}
replace country4_x = "\\"




keep in 1/10
forvalues i = 1/4{
	
	tostring country`i', replace force usedisplayformat
	replace country`i' = country`i' + pval_`i'

	forvalues j = 2(2)10{
		replace country`i' = "(" + country`i' + ")" in `j'
}	
	
}	


keep variable* country*
drop country









*** Figure 5 ***

// These numbers came from R.

import delimited "gini_part2_no_private_transfers", clear

drop v1 *theil* mean_value_gini_pre upper_ninety_percent_ci_gini_pre lower_ninety_percent_ci_gini_pre

rename mean_value_gini_post_tax gini_post_tax

rename lower_ninety_percent_ci_gini_pos lci_posttaxgini
rename upper_ninety_percent_ci_gini_pos uci_posttaxgini

g treatment_ = 1 if treatment == "anarchy"
replace treatment_ = 2 if treatment == "tax"
replace treatment_ = 3 if treatment == "ubi"
replace treatment_ = 4 if treatment == "effort"
replace treatment_ = 5 if treatment == "luck"
drop treatment

// These values calculated in R.
g gini_post_tax_theoretical = 0.42963 if treatment == 1
replace gini_post_tax_theoretical = 0.355105 if treatment == 2
replace gini_post_tax_theoretical = 0.379259 if treatment == 3
replace gini_post_tax_theoretical = 0.366475 if treatment == 4
replace gini_post_tax_theoretical = 0.366475 if treatment == 5

save gini_post_tax.dta, replace






import delimited "social_mobility_bootstrapped", clear

drop v1 *theil* 
g treatment_ = 1 if treatment == "anarchy"
replace treatment_ = 2 if treatment == "tax"
replace treatment_ = 3 if treatment == "ubi"
replace treatment_ = 4 if treatment == "effort"
replace treatment_ = 5 if treatment == "luck"
drop treatment

rename mean_value_gini gini_final

rename lower_ lci_finalgini
rename upper_ uci_finalgini

save gini_soc_mob.dta, replace

import delimited "anarchy_bootstrapped", clear

drop v1 *theil* 
g treatment_ = 1 if treatment == "anarchy"
replace treatment_ = 2 if treatment == "tax"
replace treatment_ = 3 if treatment == "ubi"
replace treatment_ = 4 if treatment == "effort"
replace treatment_ = 5 if treatment == "luck"
drop treatment

rename mean_value_gini gini_final

rename lower_ lci_finalgini
rename upper_ uci_finalgini

append using "gini_soc_mob.dta"

merge 1:1 country treatment using gini_post_tax
drop _merge
save gini_final.dta, replace


* Now, the version using the thoeretical ginis to show cleanest comparisons.
use "gini_final.dta", clear

rename treatment_ treatment

label def treatment 1 "Anarchy" 2 "Tax" 3 "UBI" 4 "Effort" 5 "Luck"
label val treatment treatment
label def country 1 "Germany" 2 "India" 3 "Indonesia" 4 "USA"
label val country country

* We're now plotting the theoretical post tax gini, so that we get the cleanest comparison of ienquality levels (ie. random chance eg. in the no. of people going up the income ladder doesn't have any effect).

g percent_diff = 1 - (gini_post_tax / gini_final)
g percent_diff_lci = 1 - (gini_post_tax / lci_finalgini)
g percent_diff_uci = 1 - (gini_post_tax / uci_finalgini)

replace gini_post_tax = gini_post_tax_theoretical 
replace gini_final = gini_post_tax_theoretical * (1 + percent_diff)
replace lci_finalgini = gini_post_tax_theoretical * (1 + percent_diff_lci)
replace uci_finalgini = gini_post_tax_theoretical * (1 + percent_diff_uci)

g treatment_final = treatment + 0.2 // offset stuff a little.


twoway (scatter gini_post_tax treatment, ms(D) mc(emerald) lc(emerald)) /// 
(scatter gini_final treatment_final, ms(oh)  mc(cranberry) lc(cranberry)) ///
(rcap lci_finalgini uci_finalgini treatment_final, lc(cranberry)), ///
yline(.42963, lp(solid) lc(red) lw(thick)) ///
by(country, note("") legend( row(1) pos(6))) ///
xlabel(1(1)5, valuelabels) xtitle("Treatment") legend(order (1 "Gini Post Tax" 2 "Gini Final")  row(1)) text(.44 4 "Gini: Initial", size(small))

graph export  "gini_theoretical_distn.png", replace

