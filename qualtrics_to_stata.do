*** THIS FILE TAKES THE RAW QUALTRICS OUTPUT AND TRANSFORMS IT INTO WORKABLE DATA. ***
*** We share this cleaning code for transparency purposes, though note that it cannot be run without access to the raw data.

* UNTIL THE PAPER IS ACCEPTED, WE PREFER NOT TO SHARE THE RAW DATA (fear of being scooped), SO THIS FILE ALSO:
	* REMOVES ALL COLUMNS THAT ARE NOT NECESSARY FOR THE REPLICATION OF THE MAIN RESULTS
	* ADDS RANDOM NOISE TO PARTICIPANT RESPONSES.





cls 


* Take raw QUALTRICS files, save as .dta files. 
foreach country in "India" "Indonesia" "US" "Germany"{
qui{
clear all
usespss "input/`country'.sav" // these SPSS files were downloaded directly from qualtrics.
drop if QID166 == 1 // bots.
missings dropvars, force
mata usespss_dates()
g country = "`country'"
save "input/`country'", replace
}
di "number of variables in `country': " c(k)
}


use input/india.dta, clear
qui{
	append using input/us.dta
	append using input/germany.dta
	append using input/indonesia.dta
	}
	
* Remove everything that is not a "complete" response (e.g., didn't consent or exceeded the gender or income quota)
g final_sample = 1
rename QID4 consent
replace final_sample = 0 if consent != 1 
replace final_sample = 0 if exceeded_quota == "1"
replace final_sample = 0 if Finished != 1

keep if final_sample == 1

* Now that we've just got the completes: because we had to re-open the survey to people who had already *attempted* the survey (an issue with the quotas, meant that all those who came to the survey page were told the quota was exceeded), we may have duplicate respondent-IDs. This is usually not a problem since they were not able to complete the survey at the first attempt. Nevertheless, it is possible that an individual completed the survey twice, so we keep only their first response.
bysort ppid (StartDate): gen ii = _n
keep if ii == 1
drop ii
drop exceeded_quota final_sample

* Now that we've just got completes, can destring:
foreach var in income_initial income_1 income_2 wrong_answers pages_viewed score time_part1 time_part2 mobile{
destring `var', replace
}

* Remove test runs etc. for payment purposes, all panel members were given an id beginning with "rid-".
drop if substr(ppid, 1, 4) != "rid-"

* Using variable names that are dumb, but let me go from wide to long later.
* variable name of type: part_partner_type (part 1 or 2 + partner type)

* Changing income question for part 1:
local questions "Q363_1 Q368_1 Q369_1 Q370_1 Q371_1 Q372_1" 
local monies "one_200pa_v20 one_100a_v17 one_60na_v18 one_50pb_v10 one_25b_v12 one_15nb_v14" //pa suffix: positive shock, a type. nb suffix: negative shock, b type.

local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `monies'
	rename `a' change`b'
}

* Changing income question for part 2, control:
local questions "Q450_1 Q391_1 Q451_1 Q392_1 Q393_1 Q394_1" 
local monies "180pa_v47 90a_v42 54na_v43 62pb_v36 37b_v46 27nb_v48"

local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `monies'
	rename `a' changectrl_`b'
}

* Changing income question for part 2, UBI:
local questions "Q463_1 Q464_1 Q465_1 Q466_1 Q467_1 Q468_1" 
local monies "186pa_v56 96a_v57 60ba_v51 56pb_v52 31b_v53 21nb_v54"


local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `monies'
	rename `a' changeubi_`b'
}

* Changing income question for part 2, luck:
local questions "Q471_1 Q472_1 Q473_1 Q474_1 Q475_1 Q476_1 Q412_1 Q413_1 Q414_1" 
local monies "180pa_v58 90a_v59 54na_v65 180pb_v66 90b_v67 54nb_v60 50pb_v62 25b_v68 15nb_v70"


local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `monies'
	rename `a' changeluck_`b'
}

* Changing income question for part 2, effort:
local questions "Q483_1 Q484_1 Q485_1 Q486_1 Q487_1 Q488_1 Q489_1 Q490_1 Q491_1" 
local monies "180pa_v79 90a_v78 54na_v80 180pb_v81 90b_v75 54nb_v76 50pb_v77 25b_v71 15nb_v83"

local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `monies'
	rename `a' changeeffort_`b'
}

* Changing income question for part 2, charity:
local questions "Q442_1 Q443_1 Q444_1_0 Q445_1_0 Q446_1 Q447_1" 
local monies "200pa_v32 100a_v34 60na_v24 50pb_v26 25b_v28 15nb_v30"
local n : word count `questions'


forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `monies'
	rename `a' changecharity_`b'
}


replace condition = "ubi" if condition == "UBI"
replace condition = "luck" if condition == "tax - luck"
replace condition = "charity" if condition == "pure control"
replace condition = "effort" if condition == "tax - effort"
rename condition treatment

* The Qualtrics 'high effort' marker was set based on Mturk workers who were much better at the counting zeros task than was our population. Search "score" for details below...	
replace effort = "high" if StartDate > date("07oct2022", "DMY") & country == "Germany" & score >= 32
replace effort = "high" if StartDate > date("07oct2022", "DMY") & country != "Germany" & score >= 36


* Income variables - we have 5.
* Real world incomes
* income_initial: 25 or 100 (Group A or B)
* income_1: part 1, post shock.
* income_2: part 2, post shock, pre tax
* income_2_post: part 2, post shock, post redistribution

g i2 = income_2

// note that 'pure_contol' is the ANARCHY treatment in the paper, and 'control', is our progressive tax treatment.
// # pure control. income_2 is unchanged.
// # control: tax A's 10%, give B's 12 units
// # UBI: tax a's 10%, give ALL 6 units.
// # Effort: give high-effort b's 4*their income; tax people with income > 50 10%
// # Luck is similar to effort

replace i2 = 0.9*i2 if treatment == "control" & group == "A"
replace i2 = i2 + 12 if treatment == "control" & group == "B"
replace i2 = 0.9*i2 if treatment == "ubi" & group == "A"
replace i2 = i2 + 6 if treatment == "ubi"
replace i2 = i2*4 if treatment == "effort" & group == "B" & effort == "high"
replace i2 = 0.9*i2 if treatment == "effort" & i2 > 50
replace i2 = i2*4 if treatment == "luck" & group == "B" & luck == "lucky"
replace i2 = 0.9*i2 if treatment == "luck" & i2 > 50
rename i2 income_2_post	


* Did they move up the ladder?
g moved_up = .

replace moved_up = 1 if lucky == "lucky" & treatment == "luck" & group == "B"
replace moved_up = 0 if lucky == "unlucky" & treatment == "luck" & group == "B"

replace moved_up = 1 if effort == "high" & treatment == "effort" & group == "B"
replace moved_up = 0 if effort == "low" & treatment == "effort" & group == "B"

* Hate the way these IDs look:
duplicates drop ppid, force // should be zero
sort ppid
g id = _n
* drop ppid // yep. would love to, but months later using this later


reshape long change, i(id) j(part_p_type) string
drop if missing(change)

split part_p_type, p("_v")

rename part_p_type2 questionid
destring questionid, replace

replace part_p_type = part_p_type1
drop part_p_type1


* Change income - extensive margin.
g change_ext = 0
replace change_ext = 1 if change > 0
replace change_ext = -1 if change < 0

g give = change > 0
g take = change < 0

encode country, gen(nation)
encode shock_1, gen(schock1)
encode shock_2, gen(schock2)
encode group, gen(type)
encode treatment, gen(condition)
encode effort, gen (eff)

drop country shock_1 shock_2 group treatment effort
rename nation country
rename condition treatment
rename eff effort

rename QID57 birthday
rename Q332 gender
rename Q333 income
destring birthday, replace
g byte female = gender == 2
drop gender


******* These need to be ripped off the part_p_type variable.
* Partner Income
g p_income = real(regexs(1)) if regexm(part_p_type,"([0-9]+)")

* partner type:
g p_type = "B"
replace p_type = "A" if substr(part_p_type, -1,.) == "a"

* partner moved up:
g p_moved_up = .
replace p_moved_up = 0 if p_type == "B" & (treatment ==  3 | treatment == 4) & p_income <= 50 // how is this better than treatment == "luck". stupid me.
replace p_moved_up = 1 if p_type == "B" & (treatment ==  3 | treatment == 4) & p_income > 50 // how is this better than treatment == "luck". stupid me.

* partner shock:
g p_shock = "neutral"
replace p_shock = "positive" if substr(part_p_type, -2, 1) == "p"
replace p_shock = "negative" if substr(part_p_type, -2, 1) == "n"

* Part
g part = substr(part_p_type, 1, 4) == "one_"
replace part = 2 if part == 0

* Drop part_p_type now that i've ripped all the info from it.
drop part_p_type

* Gonna have to do something about the money questions - weird commas, dots etc.
* Real world income. Needs to be treated differently by country.

g income_stripped = income
g comma = .
g stop = .


// USA & India
* Rules: 1. remove commas. 2. '.' should indicate cents in the US, but I need to look at these by hand. 3. After looking at them by hand, I choose to: replace ".000" with "000" (ie, they're using a . to separate thousands)

// keep if country == 2 | country == 4
// duplicates drop income_stripped , force
// bro income income_stripped comma stop

replace comma = strpos(income_stripped, ",") if country == 2 | country == 4
sort comma 
replace income_stripped = subinstr(income_stripped, ",","",.) if country == 2 | country == 4
replace stop = strpos(income_stripped, ".") if country == 2 | country == 4
sort stop 
replace income_stripped = subinstr(income_stripped, ".000","000",.) if country == 2 | country == 4
replace income_stripped = subinstr(income_stripped, ".00","",.) if country == 2 | country == 4


// Indonesia
// keep if country == 3
// duplicates drop income_stripped , force
// bro income income_stripped comma stop

replace comma = strpos(income_stripped, ",") if country == 3
sort comma 
replace income_stripped = substr(income_stripped, 1, length(income_stripped)-3) if (substr(income_stripped, -3, .) == ",00" | substr(income_stripped, -3, .) == ".00" | substr(income_stripped, -4, .) == ", 00" ) & country == 3
replace income_stripped = subinstr(income_stripped, ",","",.) if country == 3
replace stop = strpos(income_stripped, ".") if country == 3
sort stop 

// rip out stops and spaces
replace income_stripped = subinstr(income_stripped, ".","",.) if country == 3
replace income_stripped = subinstr(income_stripped, " ","",.) if country == 3


// Germany
// keep if country == 1
// duplicates drop income_stripped , force
// bro income income_stripped comma stop

replace comma = strpos(income_stripped, ",") if country == 1
sort comma 
replace income_stripped = substr(income_stripped, 1, length(income_stripped)-3) if substr(income_stripped, -3, .) == ",00" | substr(income_stripped, -3, .) == ".00" & country == 1
replace income_stripped = subinstr(income_stripped, ",","",.) if country == 1
replace stop = strpos(income_stripped, ".") if country == 1
sort stop 

// rip out stops and spaces
replace income_stripped = subinstr(income_stripped, ".","",.) if country == 1
replace income_stripped = "31000" if income_stripped == "30000 32000" & country == 1
replace income_stripped = subinstr(income_stripped, " ","",.) if country == 1
destring income_stripped, g(inc) force // some clever german bloke/blokess tricked my regex in java to force a response by going "..."
drop income stop comma income_stripped
rename inc income



* Label the stuff
local variables "income_2_post moved_up change_ext country female p_income p_type p_moved_up p_shock income change"
local labels "post-redistribution_income b_type_moved_up_ladder change_extensive_margin country female==1 partner_income partner_type partner_moved_up partner_shock real_world_income payment_to_change_partner_income"
local n : word count `variables'

forvalues i = 1/`n' {
    local a : word `i' of `variables'
    local b : word `i' of `labels'
	label variable `a' "`b'"
}

rename Q529_1_1 rank_ubi
rename Q529_1_2 rank_luck
rename Q529_1_3 rank_control
rename Q529_1_4 rank_effort
rename Q529_1_5 rank_charity

label variable rank_ubi "rank ubi"
label variable rank_luck "rank luck"
label variable rank_control "rank control"
label variable rank_effort "rank effort"
label variable rank_charity "rank charity"

// g rank_preference_own = tostring(rank_ubi) + tostring(rank_luck) eugh. bad stata.
g rank_preference_own = 10000*rank_ubi + 1000*rank_luck + 100*rank_control + 10*rank_effort + rank_charity

rename Q321_1_1 rank_ubi_others
rename Q321_1_2 rank_luck_others
rename Q321_1_3 rank_control_others
rename Q321_1_4 rank_effort_others
rename Q321_1_5 rank_charity_others

g rank_preference_other = 10000*rank_ubi_ + 1000*rank_luck_ + 100*rank_control_ + 10*rank_effort_ + rank_charity_

label variable rank_preference_own "ranking of policies"
label variable rank_preference_other "ranking of policies - others"

// drop rank_ubi rank_luck rank_control rank_effort rank_charity rank_ubi_others rank_luck_others rank_control_others rank_effort_others rank_charity_others



* Drop other stuff that's superfluous.
drop Progress Finished Status EndDate RecordedDate 

* Drop other stuff - these might be needed at some point, but so messry right now.
drop IPAddress LocationLatitude LocationLongitude DistributionChannel UserLanguage Q356_Browser Q356_Version Q356_Operating_System Q356_Resolution // ResponseId -- keeping it, coz i need it later.


rename income income_real
label variable income_real "real world income"

g income = income_1
replace income = income_2_post if part == 2

g income_diff = income - p_income
g inequality = income_diff / income

cap drop wrong_answers
g wrong_answers = 0
// replace wrong_answers += 1 // jking it's stata after all
label variable wrong_answers "No. of incorrect comprehension questions"

* Page 1
local questions "QID10 QID177_1 Q138 Q139 Q428_1 Q429_1 Q429_2 Q429_3 Q429_4 Q429_5 Q430_1 Q430_2 Q430_3 Q430_4 Q430_5 Q431_1 Q431_2 Q431_3 Q431_4 Q431_5 Q432_1 Q432_2 Q432_3 Q432_4 Q432_5" 
local answers "3 2 6 2 1 2 1 1 1 2 2 1 1 1 2 2 1 1 1 1 2 1 1 1 1"
local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `answers'
	di in red "Word number is: `i'"
	tab `a'
	levelsof `a'
	di "Q. no. is: `a'"
	di "Answer is: `b'"
	di in red " - "
	di in red " - "
	di in red " - "
	replace wrong_answers = wrong_answers + 1 if `a' != `b' & !missing(`a')
}

* Page 2: Now, if you start page 2, and get none wrong:
local questions "Q213 Q193_1 Q233 Q197 Q498_1 Q499_1 Q499_2 Q499_3 Q499_4 Q499_5 Q500_1 Q500_2 Q500_3 Q500_4 Q500_5 Q501_1 Q501_2 Q501_3 Q501_4 Q501_5 Q502_1 Q502_2 Q502_3 Q502_4 Q502_5" 
local answers "3 2 6 2 1 2 1 1 1 2 2 1 1 1 2 2 1 1 1 1 2 1 1 1 1"
local n : word count `questions'

forvalues i = 1/`n' {
    local a : word `i' of `questions'
    local b : word `i' of `answers'
	di in red "Word number is: `i'"
	tab `a'
	levelsof `a'
	di "Q. no. is: `a'"
	di "Answer is: `b'"
	di in red " - "
	di in red " - "
	di in red " - "
	replace wrong_answers = wrong_answers + 1 if `a' != `b' & !missing(`a')
}


//////// I have come back later, and cleaned this up well ahead //////
* Correct error in 'moved_up'

// So, after the soft-launch, I recalibrated the cut-off level for high-types to be country specific. I correctly changed the survey so that the respondents saw what they were meant to. However, I didn't change the embedded data. 

// Again, it does NOT matter for what the participants see. it does mean that AFTER the soft-launch, I can no longer use the embedded data to determine who moved up. The change to the cut off level happened on the night of the 7th.

tab StartDate if treatment == 3 & type == 2 // this is really nice. clean cut pre/post after the 7th. lots of clean air.

* Cut-offs post 7th Oct: US:>=36 ; India:>=36 ; Germany:>=32 ; Indonesia:>=36 

// wrangling dates
gen startdate = dofc(StartDate)
format %td startdate

tab moved_up if startdate >= td(7oct2022)
tab StartDate if startdate <= td(7oct2022) & treatment == 3 & type == 2
tab StartDate if startdate > td(7oct2022) & treatment == 3 & type == 2

replace effort = 1 if score >= 36 & startdate > td(7oct2022)
replace effort = 1 if score >= 32 & startdate > td(7oct2022) & country == 1

replace moved_up = 1 if effort == 1 & treatment == 3 & type == 2 & startdate > td(7oct2022)


tab effort country if treatment == 3 // deis bloody germans! everyone in fact. has done me over. the move-up rates are quite different from what i calculated in the soft-launch data. Across the board, in all countries. Maybe that is because I looked at the numbers for all conditions, and the effort condition have put in more effort... ??

graph bar score , over(treatment) by(type country) bargap(-30) // it's not even that. Strange. Ahhh well.




* Real world income at PPP // for us to stratify on real-world income in multi-country regressions, I guess it has to be comparable across countries (though perhaps not given we use country FE).
frame create ppp
frame change ppp
import delimited "input/ppp conversion factor/ppp conversion.csv", clear
bro v3 v16
keep if inlist(v3, "United States", "India", "Indonesia", "Germany")
frame change default
frame drop ppp

g income_real_ppp = .
replace income_real_ppp = 12*income_real/0.74 if country == 1 // germany: "What is your average monthly income? (€)"
replace income_real_ppp = 12*income_real/23.2 if country == 2 // india: "What is your average monthly income? (Rs.)"
replace income_real_ppp = 12*income_real/4759 if country == 3 // indonesia: "What is your average monthly income? (Rp)"
replace income_real_ppp = income_real/1 if country == 4 // usa: "What is your total annual household income before tax? (USD)"

label variable income_real_ppp "Real world income, annualised and ppp adjusted"


// Random stuff i should have done before:
rename p_type ptype
encode ptype, gen(p_type)
tab p_type
drop ptype

* Must have stuffed this up earlier.
tab schock1
tab schock2 
g shock2 = 1 
replace shock2 = -1 if inlist(schock2,1,2,3)
replace shock2 = 0 if inlist(schock2,4,5)
g shock1 = 1 
replace shock1 = -1 if inlist(schock1,1,2,3)
replace shock1 = 0 if inlist(schock1,4,5)
label define shck -1 "negative" 0 "neutral" 3 "positive"
label values shock2 shck
label values shock1 shck
drop schock1 schock2


bysort id: g iid = _n
bysort id part: g iid_part = _n



* Extracting the display orders for randomised blocks.
// https://community.qualtrics.com/survey-platform-before-march-2021-56/recover-order-of-randomly-presented-questions-within-a-block-12176#:~:text=As%20you%20mentioned%2C%20the%20order,presented%20within%20any%20randomized%20blocks.


foreach var in income_initial income_1 income_2 wrong_answers pages_viewed score time_part1 time_part2 mobile{
destring `var', replace
}

drop if substr(ppid, 1, 4) != "rid-"


* 'timing' variable is blowing up here for my mac but not the pc. so odd...

quietly describe, varlist
local vars `r(varlist)'
local omit timing _v51 // one of the sliders was the same position for all participants.
local all_vars : list vars - omit
display "`all_vars'"

foreach var in `all_vars' {
qui levelsof `var'
if r(r) == 1 & !missing(`var'){
di "`var'"
drop `var'
}
}



* Tax-Effort Sliders

desc _v*
forvalues i = 71(1)86{
	di in red `i'
	qui levelsof _v`i'
	di r(r) 
	if r(r) == 1{
		drop _v`i'
	}
}

* Tax-Luck Sliders

desc _v*
forvalues i = 58(1)70{
	di in red `i'
	qui levelsof _v`i'
	di r(r)
	if r(r) == 1 & `i' != 58{ 
		drop _v`i'
	}	
}


* Control (progressive tax) - sliders.

desc _v*
forvalues i = 35(1)48{
	di in red `i'
	qui levelsof _v`i'
	di r(r)
	if r(r) == 1 { 
		drop _v`i'
	}	
}


* Pure Control (Charity) - sliders.

desc _v*
forvalues i = 21(1)34{
	di in red `i'
	qui levelsof _v`i'
	di r(r)
	if r(r) == 1 { 
		drop _v`i'
	}	
}




* Relabel the treatments.
g trtmnt = treatment
replace trtmnt = 3 if treatment == 5
replace trtmnt = 4 if treatment == 3
replace trtmnt = 5 if treatment == 4
label define trt 1 "Charity Only" 2 "Tax" 3 "UBI" 4 "Effort" 5 "Luck"
label values trtmnt trt
tab trtmnt
tab treatment
drop treatment
rename trtmnt treatment

* Include controls for question order. 
qui levelsof questionid if part == 1
di in red r(r)
forvalues i = 1/5{
	qui levelsof questionid if part == 2 & treatment == `i'
	di in red r(r)
}


* save combined_long, replace
use input/combined_long, clear

* Keep just the variables needed for the main analysis. 
keep equality_effort private_government competition_good_harmful effort_luck left_right government_individual age female hh_size income_ladder mobile part income_real_ppp change iid give take country treatment inequality income_strata duration timing id wrong_answers qorder income income_2 income_2_post p_income type p_type

* Adding random noise to responses: add +1, -1 or 0 to the transfer decision.

* a = -1, b = 1
* generate ui = floor((b–a+1)*runiform() + a)

g ui = floor((2-0+1)*runiform() + 0) - 1
replace change = change + ui 
drop ui
save replication_set, replace
