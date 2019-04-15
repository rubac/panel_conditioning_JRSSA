//Define globals, etc.
version 14
set more off, perm
clear all

sysdir set PERSONAL "XXXX"
global dir "XXXX"

global d_orig "$dir\data-orig"
global d_work "$dir\data-work"
global d_add "$dir\data-add"
global d_ADMINCOMP "$d_orig\IEBPASS_complete_orig"

global code "$dir\code"
global results "$dir\results"

global pass "$dir\data-orig\PASS"
global ieb "$dir\data-orig\IEB"

global date: di %d date(c(current_date),"DMY")

cd "$dir"
set rmsg on, perm


**************************************************************************
//Link household identifiers (hnr) in PASS survey data to 
//household identifiers in administrative data (PSD_BGNR) and individual
//level identifiers in administrative data (PRS_ID)

clear all

//Group individuals in admin data into households using individual level
//IDs (PSD_PERSNR) that contain also household level information (PSD_BGNR)
use "$d_ADMINCOMP\hhp2006_schluessel_prs_id_treff.dta", replace
destring PSD_PERSNR, gen(psd_persnr) ignore("T B P")
nsplit psd_persnr , digits(1)
drop psd_persnr1  // indicates that sampled for wave 1

//first six digits of person ID identify households
gen PSD_BGNR = 1*psd_persnr6 + 10*psd_persnr5 + 100*psd_persnr4 + 1000*psd_persnr3 + 10000*psd_persnr2

//Identifies household heads: 01 if person is household head
gen myPid  = 1*psd_persnr8 + 10*psd_persnr7

keep PSD* psd_persnr prs_id pnr myPid


//Link to household identifiers used in PASS survey data (HNR)
merge m:1 PSD_BGNR using "$d_ADMINCOMP\hnr_psd_bgnr.dta"
drop if _m==2
drop _m
sort HNR myPid
//first individual within household is household head
bysort HNR : gen HH_HEAD = 1 if _n==1
tab  HH
unique HNR

//this data allows linkage between admin data and survey data
save "$d_ADMINCOMP\XWALK_PASS_IEB.dta", replace
	preserve
		keep prs_id HNR
		bysort prs_id: gen X=_n
		tab X
		bysort HNR prs: gen X2 = _n		
		save "$d_ADMINCOMP\all_IEB_ID_PASS.dta", replace
	restore

//data set that contains only household heads. Used later to merge
// household response status from survey data to admin data
keep if HH_HEAD==1
unique HNR
save "$d_ADMINCOMP\XWALK_PASS_IEB_HEADONLY.dta", replace
exit

**************************************************************************
//Get response status information from PASS paradata
use "$pass/hh_register", replace

//Only recipient sample
keep if sample==1

//only those that were drawn for the first wave of PASS (2006)
keep if jahrsamp==2006
unique uhnr				//6,804 hh


rename uhnr HNR
bysort HNR: keep if _n==1
replace HNR = HNR-10000000

//Variable indicating household response status in wave x
foreach v of numlist 1/3 {
gen wave`v'=1 if hnettok`v'==1
}

foreach v of varlist wave1-wave3 {
label var `v' "response in `v' ?"
recode `v' (.=0)
}

egen wave_total=rowtotal(wave1 wave2 wave3)
tab wave_total
tab1 wave*		
label var wave_total "number of waves R responded"
********************************************************************************
//Connect respondents with their admin IDs
merge 1:m HNR using "$d_ADMINCOMP\XWALK_PASS_IEB_HEADONLY.dta"
tab _m if wave_t!=0 & wave_t!=.

//this sample is PASS sample, thus treatment group ("group assigned to treatment" Z==1)
gen treat=1
label var treat "PASS SAMPLE"

//All cases that were linked (i.e., have PASS survey data) are actual respondents (D==1)
rename _m RESPONDENT
label def NOA 1"PASS ONLY" 2"IEB ONLY (nonresp)" 3"linked=respondent" 
label val RESPONDENT NOA
drop if RESPONDENT==1
recode RESPONDENT (2=0) (3=1)
tab RESPONDENT
label def resp 1"Respondent" 0"Nonrespondent"

keep  HNR wave1 wave2 wave3 wave_total psd_persnr prs_id PSD_BGNR myPid HH_HEAD treat RESPONDENT
********************************************************************************
tab wave_t
compress
tab RESPONDENT
	preserve
		keep prs_id HH wave* treat RESPONDENT HNR

		save "$d_work/prs_ids_PASSHH", replace
	restore


//Assign household response status to all individuals within household
merge 1:m HNR prs_id using "$d_ADMINCOMP\all_IEB_ID_PASS.dta", nogen
tab wave_t		
tab RESPONDENT
keep prs_id HH wave* treat RESPONDENT HNR
bysort HNR: egen respondent = max(RESPONDENT)
drop RESPONDENT
rename respondent RESPONDENT
label var RESPONDENT "member of interviewed hh"

foreach v of varlist wave1 wave2 wave3 wave_t {
bysort HNR: egen `v'_m = max(`v')
drop `v'
rename `v'_m `v'
}
tab wave_t
tab wave_t if HH==1

label var HH "Case is HH-head"


//dataset containing individual level admin IDs with household level response status
save "$d_work/prs_ids_PASSALL", replace

//is there actually admin data for everyone?
merge m:m prs_id using "$d_ADMINCOMP\a014161_ieb_v11_auszug_final.dta"
assert _m==3
clear all
exit 
********************************************************************************
//Prepare admin data ("IEB") for PASS sample (Z==1)
//Here, we mainly recode missings and other values that are non-informative
//for our purposes and start with first preparations of outcome data (Y) and
//control variables (X)

clear all

use "$d_ADMINCOMP\a014161_ieb_v11_auszug_final.dta", replace
gen long XY=ieb_fall_id
drop ieb_fall_id
rename XY ieb_fall_id


tab ieb_ber_mf_leistbez
recode ieb_ber_mf_leistbez (9999=.)

tab ieb_abg, mis		//-7 NA -5 empty
recode ieb_abg (-7=.)(-5=.)

tab ieb_erw_stat, mis
recode ieb_erw_stat (-8=.)(99=.)(999=.)

tab ieb_zug_gr, mis
recode ieb_zug_gr (9997=.)

tab ieb_sna_id, mis
recode ieb_sna_id (9999=.)(97=.)

//ieb_geb_dat: quality order LEH LHG BEH ASU XASU MTH
rename ieb_geb_dat birthday

//ieb_sex_id: quality order: LEH LHG BEH ASU XASU MTH
recode ieb_sex_id (2=0), gen(male)
drop ieb_sex_id

tab ieb_bsb, nol mis
recode ieb_bsb (9997=.)(9998=.)

tab ieb_sbs
recode ieb_sbs (9997=.)

tab ieb_staat, mis
recode ieb_staat (-7=.)(-5=.)(999=.) 

tab gleitzone, mis nol
recode gleitzone (-9=.)(-5=.)

rename ieb_berufstellg ieb_ber_stellg
rename ieb_beruf_kons ieb_ber_kons

tab ieb_beruf, mis 
recode ieb_beruf (-9=.)(-8=.)(-7=.)(-5=.)

tab ieb_ber_kons, mis
recode ieb_ber_kons (-9=.)(-8=.)(-7=.)(-5=.)

tab ieb_ber_ste, mis nol
recode ieb_ber_stel (-9=.)(-8=.)(-7=.)(-5=.)(5=.)(6=.)

sum ieb_tag_entg

tab ieb_w73, mis
recode ieb_w73 (-5=.)

tab ieb_w03 if ieb_w03<1
recode ieb_w03 (-5=.)

tab ieb_w08  if ieb_w08<1
recode ieb_w08 (-5=.)

tab ieb_w93  if ieb_w93<1
recode ieb_w93 (-5=.)

sum ieb_ao_gem
recode ieb_ao_gem (-5=.)

tab ieb_ao_gst if ieb_ao_gst <1
recode ieb_ao_gst (-9=.)(-5=.)

tab ow_knz
recode ow_knz (-5=.)

tab ieb_wo_gem if ieb_wo_gem<1
recode ieb_wo_gem (-6=.)(-5=.)

tab ieb_wo_gst if ieb_wo_gst<1
recode ieb_wo_gst (-9=.)(-5=.)

 
//Add response indicator to IEB data
merge m:m prs_id using "$d_work/prs_ids_PASSALL", nogen


save "$d_work\2110PASS", replace
exit

********************************************************************************
//We run the same preparations for the control group sample drawn from the
//same administrative data (IEB) -> Z==0

clear all

use "$ieb\a013937_sample_c.dta", replace
append using "$ieb\a012321_sample_b.dta"

gen long XY=ieb_fall_id
drop ieb_fall_id
rename XY ieb_fall_id
 
tab ieb_ber_mf_leistbez
recode ieb_ber_mf_leistbez (9999=.)

tab ieb_abg, mis		//-7 NA -5 empty
recode ieb_abg (-7=.)(-5=.)

tab ieb_erw_stat, mis
recode ieb_erw_stat (-8=.)(99=.)(999=.)

tab ieb_zug_gr, mis
recode ieb_zug_gr (9997=.)

tab ieb_sna_id, mis
recode ieb_sna_id (9999=.)(97=.)
 
//ieb_geb_dat: quality order LEH LHG BEH ASU XASU MTH
rename ieb_geb_dat birthday

//ieb_sex_id: quality order: LEH LHG BEH ASU XASU MTH
recode ieb_sex_id (2=0), gen(male)
drop ieb_sex_id

tab ieb_bsb, nol mis
recode ieb_bsb (9997=.)(9998=.)

tab ieb_sbs
recode ieb_sbs (9997=.)

tab ieb_staat, mis
recode ieb_staat (-7=.)(-5=.)(999=.) 

tab gleitzone, mis nol
recode gleitzone (-9=.)(-5=.)

rename ieb_berufstellg ieb_ber_stellg
rename ieb_beruf_kons ieb_ber_kons

tab ieb_beruf, mis 
recode ieb_beruf (-9=.)(-8=.)(-7=.)(-5=.)

tab ieb_ber_kons, mis
recode ieb_ber_kons (-9=.)(-8=.)(-7=.)(-5=.)

tab ieb_sgb_ii_tr_art, mis
recode ieb_sgb_ii_tr_art (9997=.)

tab ieb_ber_ste, mis nol
recode ieb_ber_stel (-9=.)(-8=.)(-7=.)(-5=.)(5=.)(6=.)

sum ieb_tag_entg

tab ieb_w73, mis
recode ieb_w73 (-5=.)

tab ieb_w03
recode ieb_w03 (-5=.)

tab ieb_w08
recode ieb_w08 (-5=.)

tab ieb_w93
recode ieb_w93 (-5=.)

sum ieb_ao_gem
recode ieb_ao_gem (-5=.)

sum ieb_ao_gst
recode ieb_ao_gst (-9=.)(-5=.)

sum ieb_ao_aa
recode ieb_ao_gst (-9=.)(-5=.)

tab ieb_ao_krs if ieb_ao_krs<0
recode ieb_ao_krs (-5=.)

tab ow_knz
recode ow_knz (-5=.)

tab ieb_rtyp06
recode ieb_rtyp06 (-9=.)(-7=.)(-5=.)

tab ieb_rtyp09
recode ieb_rtyp09 (-9=.)(-7=.)(-5=.)

sum ieb_sgb_ii_tr_dst
recode ieb_sgb_ii_tr_dst (-9=.)(-8=.)(-7=.)(-5=.)

sum ieb_wo_gem
recode ieb_wo_gem (-6=.)(-5=.)

sum ieb_wo_gst
recode ieb_wo_gst (-9=.)(-5=.)

sum ieb_wo_gst
recode ieb_wo_gst (-9=.)(-5=.)

sum ieb_wo_krs
recode ieb_wo_krs (-9=.)(-8=.)(-7=.)(-6=.)(-5=.)

sum ieb_wo_aa
recode ieb_wo_aa (-9=.)(-8=.)(-7=.)(-5=.)

compress

save "$d_work\unselected", replace

exit

********************************************************************************
//Here, we prepare outcomes (Y) and control variables (X) for Z==1
//The IEB comes in spell format (a type of panel data on individual level)
//for our analysis, however, we need one observation per person (for X variables)
//for the outcomes, we prepare the outcome data at four different points in time
//Before wave one, after wave one, but before wave two, after wave two, but before
//wave three and after wave three, but before wave four


//We get the first date of fieldwork for each wave from the survey paradata
use "$pass/PENDDAT", replace
	foreach v of numlist 1/6 {
		generate intdat_`v'=mdy(pintmon,pinttag,pintjahr) if welle==`v'
		format intdat_`v' %td
		bysort welle: egen min_intdat_`v'=min(intdat_`v')
		format min_intdat_`v' %td
		rename min_intdat_`v' beg_w_`v'
		drop intdat_`v'
		label var beg_w_`v' "date of 1st int in wave `v'"
		egen X_`v'= max(beg_w_`v')
		replace beg_w_`v'= X_`v' if beg_w_`v'==.
		drop X_`v'
	}

//date the sample was drawn
gen samplingdate=date("20060719", "YMD")
label var samplingdate "Day of sampling of w1 PASS"
format sampling %td
keep beg_* samplingdate
keep in 1
save "$d_work/int_dats", replace


//add these dates to the IEB data (Z==1)
append using "$d_work\2110PASS"

foreach v of varlist beg_* samplingdate {
egen t_`v' = mean(`v')
replace `v' = t_`v'
drop t_`v'
}
drop in 1
////////////////////////////////////////////////////////////////////////////////
//Preparation of Y for Z==1

//some unnecessary variables
capture drop pnr _merge

//Data for outcomes at different points described above
	
//all IDs
	preserve
		bysort prs_id: keep if _n==1
		sum prs_id
		keep prs_id
		tempfile sel_IDs
		save `sel_IDs'
	restore

	preserve
		keep if ieb_beg_orig > beg_w_1 & ieb_beg_orig<=beg_w_2 
		save "$d_work/sel_post_t_w1", replace
	restore
	preserve
		keep if ieb_beg_orig > beg_w_1 & ieb_beg_orig<=beg_w_3 
		save "$d_work/sel_post_t_w2", replace
	restore
	preserve
		keep if ieb_beg_orig > beg_w_1 & ieb_beg_orig<=beg_w_4 
		save "$d_work/sel_post_t_w3", replace
	restore
	
//For falsification test: Does the model show no effect when there should be
//no effect? Keep ALMP take-up before the survey started
	preserve
		keep if ieb_beg_orig < 	beg_w_1
		merge m:1 prs_id using `sel_IDs'
		drop _merge
		tempfile sel_sensitivity
		save `sel_sensitivity'
	restore

//IEB data consists of different data products. We get flags for each of them
//as spell content will differ by data source within IEB
gen lhgspell  								= (ieb_quellv==4) 
gen behspell  								= (ieb_quellv==1)
gen lehspell  								= (ieb_quellv==2)
gen asuspell  								= (ieb_quellv==8)
gen xasuspell 								= (ieb_quellv==16)
gen mthspell  								= (ieb_quellv==32)

////////////////////////////////////////////////////////////////////////////////

//outcome: days until new job after start of the first wave of PASS
preserve
	keep if ieb_beg_orig				> beg_w_1

	tempvar a
	bysort prs_id: egen `a' = min(ieb_beg_orig) if behspell==1 & ieb_erw_stat==101
	tempvar b
	bysort prs_id: egen `b' = mean(`a')
	gen t_n_job = `b' - beg_w_1
	tempvar c
	gen `c'=date("20121231", "YMD")- beg_w_1
	replace t_n_job =`c' if t_n_job==.
	label var t_n_job "Days until new job"
	bysort prs_id: keep if _n==1
	keep prs_id t_n_job
	save "$d_work\sel_new_job", replace
restore



//////////////////////////////////////////////////////////////////////////////////////////////////
//here, we create the control variables. We consider only spells that ended before the first
//day of fieldwork of PASS as our control variables may otherwise be affected by the treatment
//keep only pre-treatment data 	
keep if ieb_beg_orig 						< 	beg_w_1
keep if ieb_end_orig 						< 	beg_w_1

********************************************************************************
		preserve
			//UBII receipt 
			keep if lhgspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
***********************************************************************************
			//ALG2 Receipt (ieb_erw_stat further differs between spells: 1voll_erw 2mind_erw 5 rent_erw)
			bysort prs_id: egen ALG2_total 	   = total(lhgspell)
gen ALG2_dur	  = ieb_end_orig - ieb_beg_orig +1 if lhgspell==1
bysort prs_id: egen ALG2_tot_dur   = total(ALG2_dur)
			bysort prs_id: egen ALG2_m_dur = mean(ALG2_dur)
			drop ALG2_dur
		
			label var ALG2_total	 "No of times of ALG2 receipt"
			label var ALG2_tot_dur 	 "Total duration of ALG2 receipt"
			label var ALG2_m_dur	 "Mean duration of ALG2 receipt"		
					
			keep prs_id ALG2_total ALG2_tot_dur ALG2_m_dur
			tempfile sel_ALG2_spells
			save `sel_ALG2_spells'
		restore

merge m:m prs_id using `sel_ALG2_spells'
drop _merge

foreach v of varlist ALG2_total ALG2_tot_dur ALG2_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
			//UBI receipt
			keep if lehspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
			//LEH Spells (Ieb_erw_stat further differs between spells: 1ALG1 2ALHI 3UHG 5PFL)
bysort prs_id: egen LEH_total	 = total(lehspell)
gen 	 LEH_dur		= ieb_end_orig - ieb_beg_orig +1 if lehspell==1
bysort prs_id: egen LEH_tot_dur = total(LEH_dur)
bysort prs_id: egen LEH_m_dur	 = mean(LEH_dur)
			drop LEH_dur

			label var LEH_total		"No of times of LEH"
			label var LEH_tot_dur 		"Total duration of LEH"
			label var LEH_m_dur		"Mean duration of LEH"						
			keep prs_id LEH_total LEH_tot_dur LEH_m_dur
			tempfile sel_LEH_spells
			save `sel_LEH_spells'
		restore

merge m:m prs_id using `sel_LEH_spells'
drop _merge

foreach v of varlist LEH_total LEH_tot_dur LEH_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		Preserve
//employment information
			keep if behspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
			******************************************************************************
			//Employment history, BEH spells. ieb_erw_stat: 
			bysort prs_id: egen emp_total 				= total(behspell)				
gen emp_dur		= ieb_end_orig - ieb_beg_orig +1 if behspell==1 
			bysort prs_id: egen emp_tot_dur		= total(emp_dur)				 
			bysort prs_id: egen emp_m_dur		= mean(emp_dur)					 
			drop emp_dur
		
			label var emp_total			"No of times employed"
			label var emp_tot_dur		"Total duration of employment"
			label var emp_m_dur			"Mean duration of employment"
								
			keep prs_id emp_total emp_tot_dur emp_m_dur
			tempfile sel_BEH_spells
			save `sel_BEH_spells'
		restore

merge m:m prs_id using `sel_BEH_spells'
drop _merge

foreach v of varlist emp_total emp_tot_dur emp_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
				//Unemployment  spells
				keep if asuspell==1
				//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
				duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
				//Unemployment history (Ieb_erw_stat further differs between spells: 31  32  33  34  35  )

				bysort prs_id: egen unemp_total= total(asuspell)
				gen 	unemp_dur= ieb_end_orig - ieb_beg_orig +1 if asuspell==1
				bysort prs_id: egen unemp_tot_dur	= total(unemp_dur)
				bysort prs_id: egen unemp_m_dur= mean(unemp_dur)
				drop unemp_dur

				label var unemp_total	"No of times unemployed"
				label var unemp_tot_dur "Total duration of unemployment"
				label var unemp_m_dur	 "Mean duration of unemployment"
	
				keep prs_id unemp_total unemp_tot_dur unemp_m_dur
				tempfile sel_ASU_spells
				save `sel_ASU_spells'
		restore

merge m:m prs_id using `sel_ASU_spells'
drop _merge

foreach v of varlist unemp_total unemp_tot_dur unemp_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
			//ALMP Spells (control, not outcome). Outcome ALMP will be created later
			keep if mthspell==1
			//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
			//ALMP history

			bysort prs_id: egen almp_total		= total(mthspell)
			gen almp_dur		= ieb_end_orig - ieb_beg_orig +1 if mthspell==1
			bysort prs_id: egen almp_tot_dur	= total(almp_dur)
			bysort prs_id: egen almp_m_dur	= mean(almp_dur)
			drop almp_dur

			label var almp_total 			"No of participations in ALMP"
			label var almp_tot_dur			"Total duration of ALMP"
			label var almp_m_dur			"Mean duration of ALMP"

			keep prs_id almp_total almp_tot_dur almp_m_dur
			tempfile sel_MTH_spells
			save `sel_MTH_spells'
		restore

merge m:m prs_id using `sel_MTH_spells'
drop _merge

foreach v of varlist almp_total almp_tot_dur almp_m_dur {
recode  `v' (.=0)
}

//Gender
tab male
********************************************************************************
//Birthday
sum birthday

gen age = (ieb_beg_orig - birthday)/365.25

********************************************************************************
//Education
bysort prs_id: egen maxedu					= max(ieb_bsb)
rename maxedu edu
label var edu		"Max. education of person"
label define EDU 1"Kein Hauptschulabschluss" 3"Hauptschulabschluss" 5"Mittlere Reife" 6"Fachhochschulreife" 7"Abitur/Hochschulreife"
label values edu EDU
*Change in education?
bysort prs_id ieb_bsb: gen XX=_n if ieb_bsb!=.
bysort prs_id: egen YY=total(XX) if ieb_bsb!=.  & XX==1
bysort prs_id: egen maxYY=max(YY)
gen chan_edu =1 if maxYY>1 & maxYY<200
recode chan_edu (.=0) if maxYY==1
drop XX YY maxYY
label var chan_edu "change in education in the past"


********************************************************************************
//Nationality
//ieb_staat --- has many categories. Collapse into a few. Germany, Turkish (biggest non-German group)
//, and EU/nonEU
tab ieb_staat, mis
							
tab ieb_staat	//Take the most recent value available if ieb_staat !=.

bysort prs_id: egen newestcitiz			= max(ieb_beg_orig) if ieb_staat!=.
bysort prs_id: gen citiz				= ieb_staat if newestcitiz==ieb_beg_orig
bysort prs_id: egen mcitiz			= max(citiz)
drop citiz newestcitiz

rename mcitiz citiz
label var citiz 				"newest citizenship"
tab citiz
gen german				= 1 if citiz == 0
gen turkey				= 1 if citiz == 163
gen europe				= 1 if citiz <= 199 & citiz >= 121 & citiz!=163
gen africa 				= 1 if citiz <= 299 & citiz >= 221
gen americas				= 1 if citiz >= 321 & citiz <= 399
gen ocean				= 1 if citiz >= 521 & citiz <= 599
gen asia				= 1 if citiz >= 421 & citiz <= 499
replace asia = ocean if ocean==1
gen cit_other				= 1 if citiz == 7   | citiz >= 996 | citiz == .
drop ocean

foreach v of varlist german turkey europe africa americas asia cit_other {
recode `v' (.=0)
}
	
********************************************************************************	
//INformation on education and vocational training

encode ieb_ausb, gen(IEB_AUSB)
drop ieb_ausb
rename IEB_AUSB ieb_ausbildung
gen schule 	= 1		if ieb_ausb==0	& ieb_quellve==1
replace schule	= 1		if ieb_ausb==1	& ieb_quellve==1
replace schule	= 1		if ieb_ausb==3	& ieb_quellve==1
replace schule	= 2		if ieb_ausb==2	& ieb_quellve==1
replace schule	= 2		if ieb_ausb==4	& ieb_quellve==1
replace schule	= 11		if ieb_ausb==5	& ieb_quellve==1
replace schule	= 12	if ieb_ausb==6	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==7	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==8	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==9	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==-7 & ieb_quellve==1
replace schule	= 9998	if ieb_ausb==-8 & ieb_quellve==1
replace schule	= 9999	if ieb_ausb==-9 & ieb_quellve==1

gen ausbildung		= 1		if ieb_ausb==6	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 2		if ieb_ausb==2	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 2		if ieb_ausb==1	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 3		if ieb_ausb==3	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 4		if ieb_ausb==4	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 5		if ieb_ausb==7	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 6		if ieb_ausb==5	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 9997	if ieb_ausb==-9	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 9997	if ieb_ausb==-7	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 9997	if ieb_ausb==-5	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32


label define SCHULE 1"ohne abgeschl. Berufsausb." 2"betriebl./ausserbetriebl. Ausb." 11"FH ohne nähere Spezifikation" 12"Hochschulabschluss ohne nähere Spezifikation" 9997"Keine Angabe" 9998"Fehler im Ursprungswert" 9999"Keine Zuordnung möglich"
label values schule SCHULE

label define AUSBILDUNG 1"ohne abgeschl. Berufsausb." 2"betriebl./ausserbetriebl. Ausb." 3"Berufsfachschule" 4"Fachschule" 5"Fachchochschule" 6"Universität" 9997"Keine Angabe"
label values ausbildung AUSBILDUNG

//Imputation of Education information (following Fitzenberger et al 2005)
//For details on the following imputation procedure, see Fitzenberger et al 2005


********************************************************************************
* Education before Imputation
********************************************************************************
tab schule, m
tab ausbildung ,m

gen schuleFitz = schule
gen ausbildungFitz = ausbildung


********************************************************************************
* Translate ASU, XASU and MTH Ausbildung into BeH Ausbildung
********************************************************************************
replace ausbildungFitz = 11 if ausbildungFitz == 5			// FH --> FH
replace ausbildungFitz = 12 if ausbildungFitz == 6			// Uni --> Uni

replace ausbildungFitz = 2 if inlist(ausbildungFitz, 3, 4)	// Berufsfachschule, Fachschule --> betr./auÃŸerbetr. Ausbildung

replace ausbildungFitz = 1 if inlist(ausbildungFitz, 7, 8)	// nicht anerkannt ausl. Ausb./Uni --> ohne Ausbildung

tab ausbildung ausbildungFitz, m


********************************************************************************
* Step 1: No Educational Degree if Age < 18
********************************************************************************
replace ausbildungFitz = 1 if age < 18

********************************************************************************
* Step 4: Additional Adjustments (not possible anymore; variable "stib" does no longer exist --> no information if someone is Meister, Polier or Facharbeiter)
********************************************************************************
tab ieb_ber_stellg, nol

bysort prs_id ieb_beg_orig: replace schuleFitz		= 2 if ieb_ber_stellg==2 & schuleFitz==. |schuleFitz==9997|schuleFitz==9998|schuleFitz==9999 
bysort prs_id ieb_beg_orig: replace schuleFitz		= 2 if ieb_ber_stellg==3 & schuleFitz==. |schuleFitz==9997|schuleFitz==9998|schuleFitz==9999 

bysort prs_id ieb_beg_orig: replace ausbildungFitz		= 2 if ieb_ber_stellg==2 & ausbildungFitz==. |ausbildungFitz==9997 
bysort prs_id ieb_beg_orig: replace ausbildungFitz		= 2 if ieb_ber_stellg==3 & ausbildungFitz==. |ausbildungFitz==9997 

********************************************************************************
* Step 2: Forward Extrapolation
********************************************************************************
replace schuleFitz = 1 if inlist(schuleFitz, 9997, 9998, .)
replace ausbildungFitz = 1 if inlist(ausbildungFitz, 9997, .)

sort prs_id ieb_beg_orig

global obs = _N
cap {
	forvalues n = 1/$obs {
		* forward extrapolation
		replace schuleFitz = schuleFitz[`n'-1] if schuleFitz[`n'-1] > schuleFitz[`n'] & prs_id[`n'] == prs_id[`n'-1] in `n'
		replace ausbildungFitz = ausbildungFitz[`n'-1] if ausbildungFitz[`n'-1] > ausbildungFitz[`n'] & prs_id[`n'] == prs_id[`n'-1] in `n'
		
		* status
		disp "status: " round(100*`n'/$obs , 0.01) "% done ..."
	}
}

tab schuleFitz schule, m
tab ausbildungFitz ausbildung, m
count if ausbildung == 2 & ausbildungFitz == 1 & age < 18		// downgrades are due to age
count if ausbildung == 11 & ausbildungFitz == 1 & age < 18		// downgrades are due to age
count if ausbildung == 12 & ausbildungFitz == 1 & age < 18		// downgrades are due to age

* Education after Second Step
tab schuleFitz, m
tab ausbildungFitz ,m


********************************************************************************
* Step 3: Backward Extrapolation
********************************************************************************

*** marker for first jahr with new degree

* dummies for levels of education
xi i.schuleFitz, prefix(d) noomit
char _dta[__xi__Vars__Prefix__]			// prevend stata from dropping previous dummies
char _dta[__xi__Vars__To__Drop__]		// prevend stata from dropping previous dummies
xi i.ausbildungFitz, prefix(d) noomit

* mark first appearance of each level of education
sort prs_id ieb_beg_orig
foreach var of varlist dschuleFi* dausbildung* {
	by prs_id: gen byte first`var' = sum(`var' == 1) == 1  & sum(`var'[_n - 1] == 1) == 0  
}


* statistics (especially median)
foreach var of varlist first* {
	disp "Statistics on Age if `var' = 1:"
	sum age if `var' == 1 , detail
	global m`var' = r(p50)
}

* statistics on age at first appearance
gen byte firstApp = 0
by prs_id: replace firstApp = 1 if _n == 1
sum age if firstApp == 1, detail


/*
	Comment:
	
	In (Fitzenberger, Osikominu & VÃ¶lter 2005), they use the median age as limits for the backward extrapolation.
	Since the median ages we calculated are relatively large, we use the age limits proposed by Fitzenberger et al.
	The reason why the median ages calculated with the IEB data set are too high is the high share of observations
	starting at relatively old ages.
	
	The used age limits are:
	
		Ausbildung: 29 (Uni), 27 (FH), 20 (Ausbildung)
		Schule: 21 (Abi), 23 (Fachabi) [not reported in Fitzenberger et al.], 20 (Mittlere Reife, Hauptschule) [not reported in Fitzenberger et al.]
*/

* actual backward extrapolation

gsort prs_id -ieb_beg_orig		// reversed sorting

cap {
	forvalues n = 1/$obs {		// caution: loop has to be ordered by age limits!
		* forward extrapolation
		replace ausbildungFitz = 2 if ausbildungFitz[`n'-1] == 2 & prs_id[`n'] == prs_id[`n'-1] & age[`n'] >= 20 in `n'		// Ausbildung
		replace ausbildungFitz = 11 if ausbildungFitz[`n'-1] == 11 & prs_id[`n'] == prs_id[`n'-1] & age[`n'] >= 27 in `n'	// FH
		replace ausbildungFitz = 12 if ausbildungFitz[`n'-1] == 12 & prs_id[`n'] == prs_id[`n'-1] & age[`n'] >= 29 in `n'	// Uni
		
		
		replace schuleFitz = 4 if schuleFitz[`n'-1] == 4 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 20 in `n'				// Hauptschule
		replace schuleFitz = 5 if schuleFitz[`n'-1] == 5 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 20 in `n'				// Hauptschule
		replace schuleFitz = 6 if schuleFitz[`n'-1] == 6 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 20 in `n'				// Mittlere Reife
		replace schuleFitz = 9 if schuleFitz[`n'-1] == 9 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 21 in `n'				// Abi
		replace schuleFitz = 8 if schuleFitz[`n'-1] == 8 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 23 in `n'				// Fachabi
		
		* status
		disp "status: " round(100*`n'/$obs , 0.01) "% done ..."
	}
}

sort prs_id ieb_beg_orig


* Education after Third Step
tab schuleFitz, m
tab ausbildungFitz ,m




********************************************************************************
* Comparison: original education & imputed education
********************************************************************************
tab schuleFitz schule, m
tab ausbildungFitz ausbildung, m


********************************************************************************
* Generating a simple Education Variable
********************************************************************************
gen educFitz = .
replace educFitz = 3 if ausbildungFitz == 11 | ausbildungFitz == 12	// Uni & FH
replace educFitz = 2 if ausbildungFitz == 2							// Ausbildung
replace educFitz = 1 if ausbildungFitz == 1							// without Ausbildung or Uni/FH

label define educ 1 "1 No Vocational Training" 2 "2 Vocational Training" 3 "3 University or Technical College"
label values educFitz educ
label variable educFitz "Education (Imputed Fitzenberger)"

tab educFitz ausbildung, m
tab educFitz ausbildungFitz, m


********************************************************************************
* Labels
********************************************************************************
label variable schuleFitz "Schulausbildung (Fitzenberger Imputation)"
label variable ausbildungFitz "Ausbildung (Fitzenberger Imputation)"

label values schuleFitz schule_de
label values ausbildungFitz ausbildung_de


********************************************************************************
* Cleaning up
********************************************************************************
drop  dschule* dausbildung* first* 


preserve
	replace schule =. if schule >12
	bysort prs_id: egen max_s = max(schule)
	replace ausbildung =. if ausbildung >6
	bysort prs_id: egen max_a = max(ausbildung)
		
	bysort prs_id: keep if _n==1
	
	tab schule schuleF, mis
	tab ausbildung ausbildungF, mis
	replace schule=ausbildung if schule==.
	tab schule schuleF, mi
	gen s_mis=1 if schule==.
	replace s_mis =0 if s_mis==.
	gen r=rnormal()
	sort r
	keep if _n<=42150
	tab s_mis
restore
********************************************************************************
//Handycap status. Variable not used in later analysis

bysort prs_id: egen newesthcap				= max(ieb_beg_orig) if ieb_sbs!=.		//most recent spell with nonmissing ieb_sbs
bysort prs_id: gen  hcap 					= ieb_sbs if newesthcap==ieb_beg_orig	//var that has that value
bysort prs_id: egen  mhcap 					= max(hcap)
drop hcap newesthcap
rename mhcap hcap
label var hcap 								"newest handicap status of person"
tab ieb_sbs hcap
label define HCAP 1"Anerkannt" 2"Gleichgestellt" 3"Gleichstellung moegl." 4"nicht schwerbehindert"
label values hcap HCAP

********************************************************************************
// share and number of different employers worked for in the past
//total number of behspells= emp_total
bysort prs_id betnr: gen XX=_n
bysort prs_id: egen YY=total(XX) if betnr!=. & XX==1
bysort prs_id: gen diff_empyer=YY/emp_total
bysort prs_id: egen difff=max(diff_empyer)
drop diff_empyer XX YY
rename difff diff_empyer
label var diff_empyer "share of different employers in total employments"

********************************************************************************
// days since last job if unemployed
bysort prs_id: egen maxBEH = max(ieb_end_orig) if behspell==1
format maxBEH %tdD_m_Y
gen last_job = samplingdate - maxBEH
label var last_job "Days since end of last job"
drop maxBEH
gen sq_lastjob =last_job^2
label var sq_lastjob "sq. number of days since last job"

********************************************************************************
//  income ... variable not used in later analysis due to quality issues
preserve
	keep if behspell==1
	duplicates drop prs_id ieb_beg_orig ieb_end_orig ieb_tag_entg, force
	gen d_inc=ieb_tag_entg
	label var d_inc "daily income"
	bysort prs_id: egen m_inc	= mean(d_inc)
	label var m_inc "mean income"
	bysort prs_id: egen sd_inc	= sd(d_inc)
	label var sd_inc "SD income"
	keep prs_id d_inc m_inc sd_inc
	tempfile sel_income
	save `sel_income'
restore

merge m:m prs_id using `sel_income'
drop _merge

foreach v of varlist d_inc m_inc sd_inc {
recode `v' (.=0)
}

********************************************************************************
// how many times has a person moved in the past?
bysort prs_id ieb_wo_gem: gen XX=_n
bysort prs_id: egen YY=total(XX) if ieb_wo_gem!=. & XX==1
bysort prs_id: egen moves=max(YY)
replace moves = moves - 1
label var moves "number of moves in the past"
tab moves, mis
drop XX YY

*** Set moves to 0 if missing
recode moves(.=0)

********************************************************************************
//east west Germany ... Place of residence
tab ow_kn
recode ow_kn (1=1)(2=0), gen (west)
tempvar bula
gen `bula'= round(ieb_wo_gem_num/1000000)
tempvar WEST
gen `WEST' = 0 if inlist(`bula', 11,12,13,14,15,16)
replace `WEST' = 1 if inlist(`bula', 1,2,3,4,5,6,7,8,9,10)
replace west = `WEST' if west==.
bysort prs_id west: gen XX=_n if west!=.
bysort prs_id: egen YY=total(XX) if west!=. & XX==1
gen west2 =.
replace west2 =west if YY==1
bysort prs_id: egen ZZ=max(west2)

bysort prs_id: egen newest_west= max(ieb_beg_orig) if west!=.	//most recent spell with nonmissing west
bysort prs_id: gen  west3 = west if newest_west==ieb_beg_orig	//var that has that value
bysort prs_id: egen west4= max(west3)
replace ZZ=west3 if YY!=1 & YY!=.
bysort prs_id: egen maxYY=max(YY)

drop west XX YY west2 ZZ newest_west west3 maxYY
rename west4 west

label var west "person lives in west germany, most recent spell"
********************************************************************************
qui: compress

preserve
	bysort prs_id: keep if _n==1
	tab treat, mis
restore


save "$d_work\selected_2", replace


clear all
********************************************************************************
//PREPARE THE DATA FOR THE OUTCOME VARIABLE OF INTEREST (ALMP participation)
//Treatment starts with the first day of fieldwork
********************************************************************************

foreach F in sel_post_t_w1 sel_post_t_w2 sel_post_t_w3 {
use "$d_work/`F'", replace
gen mthspell  			= (ieb_quellv==32)
keep if mthspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
bysort prs_id: egen post_almp_total				= total(mthspell)
gen post_almp_dur	= ieb_end_orig - ieb_beg_orig +1 if mthspell==1
bysort prs_id: egen post_almp_tot_dur	= total(post_almp_dur)
bysort prs_id: egen post_almp_m_dur	= mean(post_almp_dur)
drop post_almp_dur

label var post_almp_total 						"No of participations in ALMP"
label var post_almp_tot_dur						"Total duration of ALMP"
label var post_almp_m_dur						"Mean duration of ALMP"
********************************************************************************

keep post_almp_total post_almp_tot_dur prs_id

bysort prs_id: egen XX=max(post_almp_total)
drop post_almp_total
rename XX Y_t_`F'
		
bysort prs_id: egen XX=max(post_almp_tot_dur)
drop post_almp_tot_dur
rename XX Y_d_`F'
			
bysort prs_id: gen XY=_n
keep if XY==1
drop XY

//merge this data with all prs_ids and give those who can't be merged (they don't have post
// treatment almp data meaning they did not participate in ALMP after Treatment) a zero
merge 1:1  prs_id using `sel_IDs'

recode Y_d_`F' (.=0) if _merge==2
recode Y_t_`F' (.=0) if _merge==2
label var Y_d_`F' "LONG Post Treat ALMP take up duration" 
label var Y_t_`F' 	"LONG # of Post Treat ALMP take up"

cap drop _merge
tempfile `F'_2
save ``F'_2', replace
clear all
}

use `sel_post_t_w1_2', replace
merge 1:1 prs_id using `sel_post_t_w2_2'
drop _merge
merge 1:1 prs_id using `sel_post_t_w3_2'
drop _merge
rename Y_t_sel_post_t_w1 Y_t_w1
rename Y_d_sel_post_t_w1 Y_d_w1
rename Y_t_sel_post_t_w2 Y_t_w2
rename Y_d_sel_post_t_w2 Y_d_w2
rename Y_t_sel_post_t_w3 Y_t_w3
rename Y_d_sel_post_t_w3 Y_d_w3
save "$d_work/sel_post_t_2", replace
********************************************************************************
//PREPAIR THE FALSIFICATION DATA FOR THE OUTCOME VARIABLE OF INTEREST (ALMP participation)
//In the same way
********************************************************************************
use `sel_sensitivity'
gen mthspell  			= (ieb_quellv==32)

keep if mthspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
//ALMP history

bysort prs_id: egen post_almp_total				= total(mthspell)
gen post_almp_dur			= ieb_end_orig - ieb_beg_orig +1 if mthspell==1
bysort prs_id: egen post_almp_tot_dur			= total(post_almp_dur)
prs_id: egen post_almp_m_dur				= mean(post_almp_dur)
drop post_almp_dur

label var post_almp_total 		"No of participations in ALMP"
label var post_almp_tot_dur						"Total duration of ALMP"
label var post_almp_m_dur						"Mean duration of ALMP"
					
keep post_almp_total post_almp_tot_dur prs_id
					
bysort prs_id: egen XX=max(post_almp_total)
drop post_almp_total
rename XX p_almp_t_long
		
bysort prs_id: egen XX=max(post_almp_tot_dur)
drop post_almp_tot_dur
rename XX p_almp_d_long
			
bysort prs_id: keep if _n==1
									

//merge this data with all prs_ids and give those who can't be merged (they don't have almp data) a zero
merge 1:1  prs_id using `sel_IDs'
recode p_almp_d_long (.=0) if _merge==2
recode p_almp_t_long (.=0) if _merge==2

drop _merge
rename p_almp_d_long Y_d_FALS
rename p_almp_t_long Y_t_FALS
									
label var Y_d_FALS   "Duration ALMP 2 prior to Intw" 
label var Y_t_FALS	"# of ALMP prior to Intw"
save "$d_work\sel_Sensitivity_data", replace
								
exit

********************************************************************************
//Prepare IEB data for Control group (Z==0) in the same way
clear all
use "$d_work/int_dats", replace

append using "$d_work\unselected"

foreach v of varlist beg_* samplingdate {
egen t_`v' = mean(`v')
replace `v' = t_`v'
drop t_`v'
}
drop in 1
*drop some unneccessary variables
capture drop stpr_jahr ieb_beg_epi ieb_end_epi ieb_spell ieb_level_1 ieb_level_2 ieb_nlevel_1 ieb_nlevel_2 ieb_nspell ieb_comb_quelle ieb_rtyp*

preserve
	bysort prs_id: keep if _n==1
	sum prs_id
restore

 	preserve
		bysort prs_id: keep if _n==1
		keep prs_id
		tempfile unsel_IDs
		save `unsel_IDs', replace
	restore

	preserve
		keep if ieb_beg_orig				> beg_w_1 & ieb_beg_orig<=beg_w_2 
		save "$d_work/unsel_post_t_w1", replace
	restore
	preserve
		keep if ieb_beg_orig				> beg_w_1 & ieb_beg_orig<=beg_w_3 
		save "$d_work/unsel_post_t_w2", replace
	restore
	preserve
		keep if ieb_beg_orig				> beg_w_1 & ieb_beg_orig<=beg_w_4 
		save "$d_work/unsel_post_t_w3", replace
	restore
//preprogram test: Does the model show no effect when there should be
//no effect? Keep ALMP take-up from before the survey started
	preserve
		keep if ieb_beg_orig				< 	beg_w_1
		merge m:1 prs_id using `unsel_IDs'
		drop _merge
		tempfile unsel_sensitivity
		save `unsel_sensitivity'
	restore

//get flags for different spell types
gen lhgspell  								= (ieb_quellv==4) 
gen behspell  								= (ieb_quellv==1)
gen lehspell  								= (ieb_quellv==2)
gen asuspell  								= (ieb_quellv==8)
gen xasuspell 								= (ieb_quellv==16)
gen mthspell  								= (ieb_quellv==32)

preserve
	keep if ieb_beg_orig				> beg_w_1
	tempvar a
	bysort prs_id: egen `a' = min(ieb_beg_orig) if behspell==1 & ieb_erw_stat==101
	tempvar b
	bysort prs_id: egen `b' = mean(`a')
	gen t_n_job = `b' - beg_w_1
	tempvar c
	gen `c'=date("20121231", "YMD")- beg_w_1
	replace t_n_job =`c' if t_n_job==.
	label var t_n_job "Days until new job"
	
	bysort prs_id: keep if _n==1
	keep prs_id t_n_job
	save "$d_work\unsel_new_job", replace
restore

//keep only pre-treatment data for X variables
keep if ieb_beg_orig 						< 	beg_w_1
keep if ieb_end_orig 						< 	beg_w_1

********************************************************************************
		preserve
//keep only UBII spells
keep if lhgspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
//ALG2 Receipt (ieb_erw_stat further differs between spells: 1voll_erw 2mind_erw 5 rent_erw)
bysort prs_id: egen ALG2_total 		= total(lhgspell)
gen 		ALG2_dur= ieb_end_orig - ieb_beg_orig +1 if lhgspell==1
bysort prs_id: egen ALG2_tot_dur	= total(ALG2_dur)
bysort prs_id: egen ALG2_m_dur				= mean(ALG2_dur)
drop ALG2_dur
		
label var ALG2_total						"No of times of ALG2 receipt"
label var ALG2_tot_dur 						"Total duration of ALG2 receipt"
label var ALG2_m_dur						"Mean duration of ALG2 receipt"		
					
keep prs_id ALG2_total ALG2_tot_dur ALG2_m_dur
tempfile unsel_ALG2_spells
save `unsel_ALG2_spells'
		restore

merge m:m prs_id using `unsel_ALG2_spells'
drop _merge

foreach v of varlist ALG2_total ALG2_tot_dur ALG2_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
			//keep only UBI spells
			keep if lehspell==1
			//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
			//LEH Spells (Ieb_erw_stat further differs between spells: 1ALG1 2ALHI 3UHG 5PFL)
			bysort prs_id: egen LEH_total				= total(lehspell)
			gen 		LEH_dur	= ieb_end_orig - ieb_beg_orig +1 if lehspell==1
			bysort prs_id: egen LEH_tot_dur	= total(LEH_dur)
			bysort prs_id: egen LEH_m_dur			= mean(LEH_dur)
			drop LEH_dur

			label var LEH_total	"No of times of LEH"
			label var LEH_tot_dur 		"Total duration of LEH"
			label var LEH_m_dur		"Mean duration of LEH"	
					
			keep prs_id LEH_total LEH_tot_dur LEH_m_dur
			tempfile unsel_LEH_spells
			save `unsel_LEH_spells'
		restore

merge m:m prs_id using `unsel_LEH_spells'
drop _merge

foreach v of varlist LEH_total LEH_tot_dur LEH_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
			//keep only Employment spells
			keep if behspell==1
			//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
			//Employment history, BEH spells. ieb_erw_stat: 
			bysort prs_id: egen emp_total 				= total(behspell)				
			gen 		emp_dur= ieb_end_orig - ieb_beg_orig +1 if behspell==1 
			bysort prs_id: egen emp_tot_dur	= total(emp_dur)				 
			bysort prs_id: egen emp_m_dur		= mean(emp_dur)					 
			drop emp_dur
		
			label var emp_total	"No of times employed"
			label var emp_tot_dur	"Total duration of employment"
			label var emp_m_dur	"Mean duration of employment"
					
			keep prs_id emp_total emp_tot_dur emp_m_dur
			tempfile unsel_BEH_spells
			save `unsel_BEH_spells'
		restore

merge m:m prs_id using `unsel_BEH_spells'
drop _merge

foreach v of varlist emp_total emp_tot_dur emp_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
			//keep only Unemployment spells
			keep if asuspell==1
			//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
	
****************************************************************************
//Unemployment history (Ieb_erw_stat further differs between spells: 31  32  33  34  35  )		
bysort prs_id: egen unemp_total				= total(asuspell)
			gen 	unemp_dur	= ieb_end_orig - ieb_beg_orig +1 if asuspell==1
			bysort prs_id: egen unemp_tot_dur			= total(unemp_dur)
			bysort prs_id: egen unemp_m_dur= mean(unemp_dur)
			drop unemp_dur

			label var unemp_total	"No of times unemployed"
			var unemp_tot_dur		"Total duration of unemployment"
			label var unemp_m_dur		"Mean duration of unemployment"
	
			keep prs_id unemp_total unemp_tot_dur unemp_m_dur
			tempfile unsel_ASU_spells
			save `unsel_ASU_spells'
		restore

merge m:m prs_id using `unsel_ASU_spells'
drop _merge

foreach v of varlist unemp_total unemp_tot_dur unemp_m_dur {
recode  `v' (.=0)
}
********************************************************************************
********************************************************************************
		preserve
			//keep only ALMP spells(control var, not outcome)
			keep if mthspell==1
			//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
			duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
			//ALMP history

			bysort prs_id: egen almp_total	= total(mthspell)
			gen almp_dur	= ieb_end_orig - ieb_beg_orig +1 if mthspell==1
			bysort prs_id: egen almp_tot_dur	= total(almp_dur)
			bysort prs_id: egen almp_m_dur		= mean(almp_dur)
			drop almp_dur

			label var almp_total 				"No of participations in ALMP"
			label var almp_tot_dur				"Total duration of ALMP"
			label var almp_m_dur				"Mean duration of ALMP"

			keep prs_id almp_total almp_tot_dur almp_m_dur
			tempfile unsel_MTH_spells
			save `unsel_MTH_spells'
		restore

merge m:m prs_id using `unsel_MTH_spells'
drop _merge

foreach v of varlist almp_total almp_tot_dur almp_m_dur {
recode  `v' (.=0)
}

//male
tab male
********************************************************************************
//birthday
sum birthday

gen age = (ieb_beg_orig - birthday)/365.25

********************************************************************************
//Education
bysort prs_id: egen maxedu			= max(ieb_bsb)
rename maxedu edu
label var edu				"Max. education of person"
label define EDU 1"Kein Hauptschulabschluss" 3"Hauptschulabschluss" 5"Mittlere Reife" 6"Fachhochschulreife" 7"Abitur/Hochschulreife"
label values edu EDU

//Change in education?
bysort prs_id ieb_bsb: gen XX=_n if ieb_bsb!=.
bysort prs_id: egen YY=total(XX) if ieb_bsb!=.  & XX==1
bysort prs_id: egen maxYY=max(YY)
gen chan_edu =1 if maxYY>1 & maxYY<200
recode chan_edu (.=0) if maxYY==1
drop XX YY maxYY
label var chan_edu "change in education in the past"

********************************************************************************
//Nationality--- has many categories. Collapse into a few. Germany, Turkish, and EU/Non-EU
tab ieb_staat, mis
							
tab ieb_staat	//take the most recent value available if ieb_staat ==.

bysort prs_id: egen newestcitiz			    = max(ieb_beg_orig) if ieb_staat!=.
bysort prs_id: gen citiz					= ieb_staat if newestcitiz==ieb_beg_orig
bysort prs_id: egen mcitiz					= max(citiz)
drop citiz newestcitiz

rename mcitiz citiz
label var citiz 							"newest citizenship"
tab citiz
gen german									= 1 if citiz == 0
gen turkey									= 1 if citiz == 163
gen europe					= 1 if citiz <= 199 & citiz >= 121 & citiz!=163
gen africa 						= 1 if citiz <= 299 & citiz >= 221
gen americas					= 1 if citiz >= 321 & citiz <= 399
gen ocean						= 1 if citiz >= 521 & citiz <= 599
gen asia						= 1 if citiz >= 421 & citiz <= 499
replace asia = ocean if ocean==1
gen cit_other					= 1 if citiz == 7   | citiz >= 996 | citiz == .
drop ocean

foreach v of varlist german turkey europe africa americas asia cit_other {
recode `v' (.=0)
}

********************************************************************************	

gen schule 	= 1		if ieb_ausb==0	& ieb_quellve==1
replace schule	= 1		if ieb_ausb==1	& ieb_quellve==1
replace schule	= 1		if ieb_ausb==3	& ieb_quellve==1
replace schule	= 2		if ieb_ausb==2	& ieb_quellve==1
replace schule	= 2		if ieb_ausb==4	& ieb_quellve==1
replace schule	= 11	if ieb_ausb==5	& ieb_quellve==1
replace schule	= 12	if ieb_ausb==6	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==7	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==8	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==9	& ieb_quellve==1
replace schule	= 9997	if ieb_ausb==-7 & ieb_quellve==1
replace schule	= 9998	if ieb_ausb==-8 & ieb_quellve==1
replace schule	= 9999	if ieb_ausb==-9 & ieb_quellve==1

gen ausbildung		= 1		if ieb_ausb==6	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 2		if ieb_ausb==2	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 2		if ieb_ausb==1	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 3		if ieb_ausb==3	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 4		if ieb_ausb==4	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 5		if ieb_ausb==7	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 6		if ieb_ausb==5	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 9997	if ieb_ausb==-9	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 9997	if ieb_ausb==-7	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32
replace ausbildung	= 9997	if ieb_ausb==-5	& ieb_quellve==8 | ieb_quellve==16 | ieb_quellve==32

label define SCHULE 1"ohne abgeschl. Berufsausb." 2"betriebl./ausserbetriebl. Ausb." 11"FH ohne nähere Spezifikation" ///
12"Hochschulabschluss ohne nähere Spezifikation" 9997"Keine Angabe" 9998"Fehler im Ursprungswert" ///
9999"Keine Zuordnung möglich"
label values schule SCHULE

label define AUSBILDUNG 1"ohne abgeschl. Berufsausb." 2"betriebl./ausserbetriebl. Ausb." 3"Berufsfachschule" 4"Fachschule" 5"Fachchochschule" 6"Universität" 9997"Keine Angabe"
label values ausbildung AUSBILDUNG

//Imputation of Education information
********************************************************************************
// Education before Imputation
********************************************************************************
tab schule, m
tab ausbildung ,m

gen schuleFitz = schule
gen ausbildungFitz = ausbildung


********************************************************************************
* Translate ASU, XASU and MTH Ausbildung into BeH Ausbildung
********************************************************************************
replace ausbildungFitz = 11 if ausbildungFitz == 5			// FH --> FH
replace ausbildungFitz = 12 if ausbildungFitz == 6			// Uni --> Uni

replace ausbildungFitz = 2 if inlist(ausbildungFitz, 3, 4)	// Berufsfachschule, Fachschule --> betr./auÃŸerbetr. Ausbildung

replace ausbildungFitz = 1 if inlist(ausbildungFitz, 7, 8)	// nicht anerkannt ausl. Ausb./Uni --> ohne Ausbildung

tab ausbildung ausbildungFitz, m


********************************************************************************
* Step 1: No Educational Degree if Age < 18
********************************************************************************
replace ausbildungFitz = 1 if age < 18

********************************************************************************
* Step 4: Additional Adjustments (not possible anymore; variable "stib" does no longer exist --> no information if someone is Meister, Polier or Facharbeiter)
********************************************************************************
tab ieb_ber_stellg, nol

bysort prs_id ieb_beg_orig: replace schuleFitz		= 2 if ieb_ber_stellg==2 & schuleFitz==. |schuleFitz==9997|schuleFitz==9998|schuleFitz==9999 
bysort prs_id ieb_beg_orig: replace schuleFitz		= 2 if ieb_ber_stellg==3 & schuleFitz==. |schuleFitz==9997|schuleFitz==9998|schuleFitz==9999 

bysort prs_id ieb_beg_orig: replace ausbildungFitz		= 2 if ieb_ber_stellg==2 & ausbildungFitz==. |ausbildungFitz==9997 
bysort prs_id ieb_beg_orig: replace ausbildungFitz		= 2 if ieb_ber_stellg==3 & ausbildungFitz==. |ausbildungFitz==9997 

********************************************************************************
* Step 2: Forward Extrapolation
********************************************************************************
replace schuleFitz = 1 if inlist(schuleFitz, 9997, 9998, .)
replace ausbildungFitz = 1 if inlist(ausbildungFitz, 9997, .)

sort prs_id ieb_beg_orig

global obs = _N
cap {
	forvalues n = 1/$obs {
		* forward extrapolation
		replace schuleFitz = schuleFitz[`n'-1] if schuleFitz[`n'-1] > schuleFitz[`n'] & prs_id[`n'] == prs_id[`n'-1] in `n'
		replace ausbildungFitz = ausbildungFitz[`n'-1] if ausbildungFitz[`n'-1] > ausbildungFitz[`n'] & prs_id[`n'] == prs_id[`n'-1] in `n'
		
		* status
		disp "status: " round(100*`n'/$obs , 0.01) "% done ..."
	}
}

tab schuleFitz schule, m
tab ausbildungFitz ausbildung, m
count if ausbildung == 2 & ausbildungFitz == 1 & age < 18		// downgrades are due to age
count if ausbildung == 11 & ausbildungFitz == 1 & age < 18		// downgrades are due to age
count if ausbildung == 12 & ausbildungFitz == 1 & age < 18		// downgrades are due to age

* Education after Second Step
tab schuleFitz, m
tab ausbildungFitz ,m


********************************************************************************
* Step 3: Backward Extrapolation
********************************************************************************

*** marker for first jahr with new degree

* dummies for levels of education
xi i.schuleFitz, prefix(d) noomit
char _dta[__xi__Vars__Prefix__]			// prevend stata from dropping previous dummies
char _dta[__xi__Vars__To__Drop__]		// prevend stata from dropping previous dummies
xi i.ausbildungFitz, prefix(d) noomit

* mark first appearance of each level of education
sort prs_id ieb_beg_orig
foreach var of varlist dschuleFi* dausbildung* {
	by prs_id: gen byte first`var' = sum(`var' == 1) == 1  & sum(`var'[_n - 1] == 1) == 0  
}


* statistics (especially median)
foreach var of varlist first* {
	disp "Statistics on Age if `var' = 1:"
	sum age if `var' == 1 , detail
	global m`var' = r(p50)
}

* statistics on age at first appearance
gen byte firstApp = 0
by prs_id: replace firstApp = 1 if _n == 1
sum age if firstApp == 1, detail


/*
	Comment:
	
	In (Fitzenberger, Osikominu & VÃ¶lter 2005), they use the median age as limits for the backward extrapolation.
	Since the median ages we calculated are relatively large, we use the age limits proposed by Fitzenberger et al.
	The reason why the median ages calculated with the SIAB data set are too high is the high share of observations
	starting at relatively old ages.
	
	The used age limits are:
	
		Ausbildung: 29 (Uni), 27 (FH), 20 (Ausbildung)
		Schule: 21 (Abi), 23 (Fachabi) [not reported in Fitzenberger et al.], 20 (Mittlere Reife, Hauptschule) [not reported in Fitzenberger et al.]
*/

* actual backward extrapolation

gsort prs_id -ieb_beg_orig		// reversed sorting

cap {
	forvalues n = 1/$obs {		// caution: loop has to be ordered by age limits!
		* forward extrapolation
		replace ausbildungFitz = 2 if ausbildungFitz[`n'-1] == 2 & prs_id[`n'] == prs_id[`n'-1] & age[`n'] >= 20 in `n'		// Ausbildung
		replace ausbildungFitz = 11 if ausbildungFitz[`n'-1] == 11 & prs_id[`n'] == prs_id[`n'-1] & age[`n'] >= 27 in `n'	// FH
		replace ausbildungFitz = 12 if ausbildungFitz[`n'-1] == 12 & prs_id[`n'] == prs_id[`n'-1] & age[`n'] >= 29 in `n'	// Uni
		
		
		replace schuleFitz = 4 if schuleFitz[`n'-1] == 4 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 20 in `n'				// Hauptschule
		replace schuleFitz = 5 if schuleFitz[`n'-1] == 5 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 20 in `n'				// Hauptschule
		replace schuleFitz = 6 if schuleFitz[`n'-1] == 6 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 20 in `n'				// Mittlere Reife
		replace schuleFitz = 9 if schuleFitz[`n'-1] == 9 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 21 in `n'				// Abi
		replace schuleFitz = 8 if schuleFitz[`n'-1] == 8 & prs_id[`n'] == prs_id[`n'-1] & age[`n']>= 23 in `n'				// Fachabi
		
		* status
		disp "status: " round(100*`n'/$obs , 0.01) "% done ..."
	}
}

sort prs_id ieb_beg_orig


* Education after Third Step
tab schuleFitz, m
tab ausbildungFitz ,m




********************************************************************************
* Comparision: original education & imputed education
********************************************************************************
tab schuleFitz schule, m
tab ausbildungFitz ausbildung, m


********************************************************************************
* Generating a simple Education Variable
********************************************************************************
gen educFitz = .
replace educFitz = 3 if ausbildungFitz == 11 | ausbildungFitz == 12	// Uni & FH
replace educFitz = 2 if ausbildungFitz == 2							// Ausbildung
replace educFitz = 1 if ausbildungFitz == 1							// without Ausbildung or Uni/FH

label define educ 1 "1 No Vocational Training" 2 "2 Vocational Training" 3 "3 University or Technical College"
label values educFitz educ
label variable educFitz "Education (Imputed Fitzenberger)"

tab educFitz ausbildung, m
tab educFitz ausbildungFitz, m


********************************************************************************
* Labels
********************************************************************************
label variable schuleFitz "Schulausbildung (Fitzenberger Imputation)"
label variable ausbildungFitz "Ausbildung (Fitzenberger Imputation)"

label values schuleFitz schule_de
label values ausbildungFitz ausbildung_de


********************************************************************************
* Cleaning up
********************************************************************************
drop  dschule* dausbildung* first* 


preserve
	replace schule =. if schule >12
	bysort prs_id: egen max_s = max(schule)
	replace ausbildung =. if ausbildung >6
	bysort prs_id: egen max_a = max(ausbildung)
		
	bysort prs_id: keep if _n==1
	
	tab schule schuleF, mis
	tab ausbildung ausbildungF, mis
	replace schule=ausbildung if schule==.
	tab schule schuleF, mi
	gen s_mis=1 if schule==.
	replace s_mis =0 if s_mis==.
	gen r=rnormal()
	sort r
	keep if _n<=42150
	tab s_mis
restore

********************************************************************************
//ieb_sbs --> Handicapped NOT USED IN LATER ANALYSIS

bysort prs_id: egen newesthcap		= max(ieb_beg_orig) if ieb_sbs!=.	//most recent spell with nonmissing ieb_sbs
bysort prs_id: gen  hcap 		= ieb_sbs if newesthcap==ieb_beg_orig	//var that has that value
bysort prs_id: egen  mhcap 		= max(hcap)
drop hcap newesthcap
rename mhcap hcap
label var hcap 				"newest handicap status of person"
tab ieb_sbs hcap
label define HCAP 1"Anerkannt" 2"Gleichgestellt" 3"Gleichstellung moegl." 4"nicht schwerbehindert"
label values hcap HCAP

********************************************************************************		
		
tab ieb_sna_id

********************************************************************************
// Share and number of different employers
//total number of behspells= emp_total
bysort prs_id betnr: gen XX=_n
bysort prs_id: egen YY=total(XX) if betnr!=. & XX==1
bysort prs_id: gen diff_empyer=YY/emp_total
bysort prs_id: egen difff=max(diff_empyer)
drop diff_empyer XX YY
rename difff diff_empyer
label var diff_empyer "share of different employers in total employments"

********************************************************************************
// days since last job
bysort prs_id: egen maxBEH = max(ieb_end_orig) if behspell==1
format maxBEH %tdD_m_Y
gen last_job = samplingdate - maxBEH
label var last_job "Days since end of last job"
drop maxBEH
gen sq_lastjob =last_job^2
label var sq_lastjob "sq. number of days since last job"

********************************************************************************
// income --> ieb_tag_entg (Beh)NOT USED IN ANALYSIS DUE TO QUALITY ISSUES
preserve
	keep if behspell==1
	duplicates drop prs_id ieb_beg_orig ieb_end_orig ieb_tag_entg, force
	gen d_inc=ieb_tag_entg
	label var d_inc "daily income"
	bysort prs_id: egen m_inc	= mean(d_inc)
	label var m_inc "mean income"
	bysort prs_id: egen sd_inc	= sd(d_inc)
	label var sd_inc "SD income"
	keep prs_id d_inc m_inc sd_inc
	tempfile unsel_income
	save `unsel_income' 
restore

merge m:m prs_id using `unsel_income'
drop _merge

foreach v of varlist d_inc m_inc sd_inc {
recode `v' (.=0)
}

********************************************************************************
// how many times has a person moved in the past?
bysort prs_id ieb_wo_gem: gen XX=_n
bysort prs_id: egen YY=total(XX) if ieb_wo_gem!=. & XX==1
bysort prs_id: egen moves=max(YY)
replace moves = moves - 1
label var moves "number of moves in the past"
tab moves, mis
drop XX YY

// Set moves to 0 if missing
recode moves(.=0)

********************************************************************************
//east west Germany
tab ow_kn
recode ow_kn (1=1)(2=0), gen (west)
tempvar bula
gen `bula'= round(ieb_wo_gem_num/1000000)
tempvar WEST
gen `WEST' = 0 if inlist(`bula', 11,12,13,14,15,16)
replace `WEST' = 1 if inlist(`bula', 1,2,3,4,5,6,7,8,9,10)
replace west = `WEST' if west==.
bysort prs_id west: gen XX=_n if west!=.
bysort prs_id: egen YY=total(XX) if west!=. & XX==1
gen west2 =.
replace west2 =west if YY==1
bysort prs_id: egen ZZ=max(west2)

bysort prs_id: egen newest_west				= max(ieb_beg_orig) if west!=.			//most recent spell with nonmissing west
bysort prs_id: gen  west3 					= west if newest_west==ieb_beg_orig				//var that has that value
bysort prs_id: egen west4					= max(west3)
replace ZZ=west3 if YY!=1 & YY!=.
bysort prs_id: egen maxYY=max(YY)

drop west XX YY west2 ZZ newest_west west3 maxYY
rename west4 west

label var west "person lives in west germany, most recent spell"
********************************************************************************

qui: compress

preserve
	bysort prs_id: keep if _n==1
	sum prs_id
restore

save "$d_work\unselected_2", replace

clear all
********************************************************************************
//PREPARE THE DATA FOR THE OUTCOME VARIABLE OF INTEREST (ALMP participation)
//Treatment starts with the first day of fieldwork
********************************************************************************

foreach F in unsel_post_t_w1 unsel_post_t_w2 unsel_post_t_w3 {
	use "$d_work/`F'", replace
	
	gen mthspell  								= (ieb_quellv==32)
	keep if mthspell==1
	//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
	duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
	********************************************************************************
	bysort prs_id: egen post_almp_total				= total(mthspell)
	gen post_almp_dur								= ieb_end_orig - ieb_beg_orig +1 if mthspell==1
	bysort prs_id: egen post_almp_tot_dur			= total(post_almp_dur)
	bysort prs_id: egen post_almp_m_dur				= mean(post_almp_dur)
	drop post_almp_dur
	
	label var post_almp_total 						"No of participations in ALMP"
	label var post_almp_tot_dur						"Total duration of ALMP"
	label var post_almp_m_dur						"Mean duration of ALMP"
					
	keep post_almp_total post_almp_tot_dur prs_id
					
	bysort prs_id: egen XX=max(post_almp_total)
	drop post_almp_total
	rename XX Y_t_`F'
		
	bysort prs_id: egen XX=max(post_almp_tot_dur)
	drop post_almp_tot_dur
	rename XX Y_d_`F'
			
	bysort prs_id: keep if _n==1
				
	//merge this data with all prs_ids and give those who can't be merged (meaning they don't have post t almp data) a zero
	merge 1:1  prs_id using `unsel_IDs'

	recode Y_d_`F' (.=0) if _merge==2
	recode Y_t_`F' (.=0) if _merge==2

	label var Y_d_`F' "LONG Post Treat ALMP take up duration" 
	label var Y_t_`F'	"LONG # of Post Treat ALMP take up"
cap drop _merge
tempfile `F'_2
save ``F'_2', replace
clear all
}


use `unsel_post_t_w1_2', replace
merge 1:1 prs_id using `unsel_post_t_w2_2'
drop _merge
merge 1:1 prs_id using `unsel_post_t_w3_2'
drop _merge

rename Y_t_unsel_post_t_w1 Y_t_w1
rename Y_d_unsel_post_t_w1 Y_d_w1
rename Y_t_unsel_post_t_w2 Y_t_w2
rename Y_d_unsel_post_t_w2 Y_d_w2
rename Y_t_unsel_post_t_w3 Y_t_w3
rename Y_d_unsel_post_t_w3 Y_d_w3
save "$d_work/unsel_post_t_2", replace

********************************************************************************
//PREPARE THE FALSIFICATION DATA FOR THE OUTCOME VARIABLE OF INTEREST (ALMP participation)
//Treatment starts with the first day of fieldwork
********************************************************************************
use `unsel_sensitivity', replace

*get flags for different spell types

gen mthspell  		= (ieb_quellv==32)
keep if mthspell==1
//there are more than one spell with same start and end dates. Keep one spell that covers the same period of time
					duplicates drop prs_id ieb_beg_orig ieb_end_orig, force
					********************************************************************************
					//ALMP history

bysort prs_id: egen post_almp_total				= total(mthspell)
gen post_almp_dur		= ieb_end_orig - ieb_beg_orig +1 if mthspell==1
	bysort prs_id: egen post_almp_tot_dur			= total(post_almp_dur)
	bysort prs_id: egen post_almp_m_dur				= mean(post_almp_dur)
					drop post_almp_dur

	label var post_almp_total 		"No of participations in ALMP"
	label var post_almp_tot_dur		"Total duration of ALMP"
	label var post_almp_m_dur		"Mean duration of ALMP"
					
					keep post_almp_total post_almp_tot_dur prs_id
					
					bysort prs_id: egen XX=max(post_almp_total)
					drop post_almp_total
					rename XX p_almp_t_long
		
					bysort prs_id: egen XX=max(post_almp_tot_dur)
					drop post_almp_tot_dur
					rename XX p_almp_d_long
			
					bysort prs_id: keep if _n==1
									

//Merge this data with all prs_ids and give those who can't be merged (meaning they don't have post t almp data) a zero
					merge 1:1  prs_id using `unsel_IDs'

					recode p_almp_d_long (.=0) if _merge==2
					recode p_almp_t_long (.=0) if _merge==2

					drop _merge
					rename p_almp_d_long Y_d_FALS
					rename p_almp_t_long Y_t_FALS
									
					label var Y_d_FALS   "Duration ALMP 2 prior to Intw" 
					label var Y_t_FALS	"# of ALMP prior to Intw"
					save "$d_work\unsel_Sensitivity_data", replace
								
							
exit


********************************************************************************
//more data prep and putting pieces together
clear all

use "$d_work\selected_2", replace	//(Z==1)
//obsolete vars --> drop
drop d_inc citiz ieb_beg_alo_ges ieb_dau_alo_ges lehspell behspell lhgspell asuspell xasuspell mthspell  ieb_konto_zeile ieb_beg_orig ieb_end_orig ieb_end_plan ieb_quellverf_id ieb_zug_gr ieb_ber_mf_leistbez ieb_sna_id ieb_beg_alo ieb_dau_alo ieb_tag_entg ieb_rest_anspruch ieb_sbs_id ieb_bsb_id betnr ieb_mas_id lkv_id ieb_staat_num  ieb_wo_gst_num  ieb_ao_gst_num ieb_wo_gem_num  ieb_ao_gem_num ieb_ber_kons ieb_beruf_num  ieb_w73_num ieb_w93_num ieb_w03_num ieb_w08_num ieb_ber_stellg ieb_erw_stat_num ieb_abg_num gleitzone_num ow_knz_num ieb_fall_id ieb_sgb_II_tr_art ieb_sgb_II_tr_dst_num


//keep only one observation per person. The variables are constant within each person
bysort prs_id: keep if _n==1


//obsolete vars 
cap drop __000000
cap drop __000003

//share of different employers. if 0 then there has been only one employer
recode diff_e (.=0)


//If 0 someone just got a new job.
recode last_job (.=0)
recode sq_lastjob (.=0)

//chan_citiz (change in citizenship/nationality) is missing for cit_other==1 --> assume no change happened
gen CITIZ = 1 if german==1
replace CITIZ = 2 if turkey==1
replace CITIZ = 3 if europe==1
replace CITIZ = 4 if CITIZ==.
drop german turkey europe africa americas asia cit_other

// if hcap is missing assume that a person is not handicapped ---irrevelant, variable not used later
recode hcap (.=4)

//drop obsolete variables (vars are without imputed values)
drop schule ausbildung ausbildungFitz
tab educFitz

drop chan_edu

//some more age variables
gen age_at_sampl =(samplingdate - birthday ) / 365.25

replace west = 2 if west==.

compress
save  "$d_work\selected_3", replace

********************************************************************************
********************************************************************************
********************************************************************************
//PREPARE THE OTHER DATASET IN THE SAME WAY (Z==0)
clear all

use "$d_work\unselected_2", replace

//obsolete vars --> drop
drop d_inc citiz lehspell behspell lhgspell asuspell xasuspell mthspell ieb_konto_zeile ieb_beg_orig ieb_end_orig ieb_end_plan ieb_quellverf_id ieb_zug_gr ieb_ber_mf_leistbez ieb_sna_id ieb_beg_alo ieb_dau_alo ieb_tag_entg ieb_rest_anspruch ieb_sgb_ii_tr_art ieb_sbs_id ieb_bsb_id betnr ieb_mas_id lkv_id ieb_staat_num ieb_sgb_ii_tr_dst_num ieb_wo_gst_num ieb_wo_aa_num ieb_ao_gst_num ieb_ao_aa_num ieb_wo_gem_num ieb_wo_krs_num ieb_ao_gem_num ieb_ao_krs_num ieb_ber_kons ieb_beruf_num ieb_w73_num ieb_w93_num ieb_w03_num ieb_w08_num ieb_ber_stellg ieb_erw_stat_num ieb_abg_num ieb_ausbildung_num gleitzone_num ow_knz_num ieb_fall_id
 
bysort prs_id: keep if _n==1

cap drop __000003

recode diff_e (.=0)

recode last_job (.=0)
recode sq_lastjob (.=0)

gen CITIZ = 1 if german==1
replace CITIZ = 2 if turkey==1
replace CITIZ = 3 if europe==1
replace CITIZ = 4 if CITIZ==.
drop german turkey europe africa americas asia cit_other

recode hcap (.=4)

drop chan_edu

gen age_at_sampl =( samplingdate - birthday ) / 365.25

replace west =2 if west==.
compress

save  "$d_work\unselected_3", replace

use "$d_work\selected_3", replace


gen PASS=1
label var PASS "case is part of PASS sample" 	//Z==1

tempfile selected_4
save `selected_4', replace

use "$d_work\unselected_3", replace			
//Get a treatment var for the unselected and give it a zero
gen PASS=0					//Z==0

********************************************************************************
///put the two datasets together
********************************************************************************
append using `selected_4'

//obsolete variables
drop treat age schule ausbildung ausbildungFitz beg_w_1 beg_w_2 beg_w_3 beg_w_4 beg_w_5 beg_w_6 samplingdate wave_total 
drop __000006 
drop __000007 
cap drop __000004 
 
save "$d_work\final_wo_outcomes", replace      

exit

********************************************************************************
********************************************************************************
//Get the outcome variables --- put Z==1 and Z==0 together
********************************************************************************
clear all

use  "$d_work\sel_post_t_2", replace
append using "$d_work\unsel_post_t_2"

tempfile SHORT
save `SHORT', replace

merge 1:1 prs_id using "$d_work/final_wo_outcomes"
keep if _merge==3
drop _merge

********************************************************************************
//add in falsification data
********************************************************************************
merge 1:1 prs_id using "$d_work/sel_Sensitivity_data"
rename _merge merge_sel

//obsolete variables
capture drop XX YY

rename Y_t_FALS XXSE
rename Y_d_FALS YYSE

merge 1:1 prs_id using "$d_work/unsel_Sensitivity_data"
rename _merge merge_unsel

 keep if merge_sel==3 | merge_unsel==3 
replace Y_t_FALS = XXSE if Y_t_FALS==.
replace Y_d_FALS = YYSE if Y_d_FALS==.
drop XXSE YYSE merge_sel merge_unsel 


//Create binary outcome: Participation in any ALMP vs no participation
qui: gen Y_b_w1 = 0 if Y_t_w1==0
qui: recode Y_b_w1(.=1)
qui: label var Y_b_w1 "After w1 Zero vs. at least one ALMP"

qui: gen Y_b_w2 = 0 if Y_t_w2==0
qui: recode Y_b_w2 (.=1)
qui: label var Y_b_w2 "After w2 Zero vs. at least one ALMP"

qui: gen Y_b_w3 = 0 if Y_t_w3==0
qui: recode Y_b_w3 (.=1)
qui: label var Y_b_w3 "After w3 Zero vs. at least one ALMP"

//Same for the falsification test outcome 
qui: gen Y_b_FALS = 0 if Y_t_FALS==0
qui: recode Y_b_FALS (.=1)
qui: label var Y_b_FALS "Zero vs. at least one ALMP FALSIFICATION"

//Actual treatment indicator ==1 if participation in any wave
gen D = 1 if wave1==1|wave2==1|wave3==1

tab D PASS
rename PASS Z

cap drop birthday edu  hcap_* sq_age sq_moves sq_m_inc sq_sd_inc sq_diff_empyer sq_A_m sq_A_td sq_A_t sq_L_m sq_L_td sq_L_t sq_u_m sq_u_td sq_u_t sq_e_m sq_e_td sq_e_t sq_a_m sq_a_td sq_a_t


//recode durations from day to year
foreach v of varlist ALG2_tot_dur LEH_tot_dur emp_tot_dur unemp_tot_dur almp_tot_dur 		 ALG2_m_dur LEH_m_dur emp_m_dur unemp_m_dur almp_m_dur last_job {
replace `v' = `v'/365.25
}



//Put job search outcomes Z==1 and Z==0 together and add to other dataset
preserve 
	clear all
	use "$d_work\sel_new_job", replace
	append  using "$d_work\unsel_new_job"
	bysort prs_id: drop if _n==2
	tempfile job
	save `job', replace
restore

merge 1:1 prs_id using `job'
drop if _m==2
tempvar G
egen `G'=max(t_n_job)
replace t_n_job = `G' if t_n_job==.

//binary indicator if job found after beginning of survey
gen found=1 if t_n_job<`G'
replace found = 0 if found==.
sum t_n_job
drop _m
cap drop __000000 __000001
drop __000000
 
tab west
tempvar C
gen `C'=rnormal()
sort `C'
tempvar X
gen `X'=runiform(0,1) if west==2
tempvar X2
xtile `X2'=`X', n(3)
recode west (2=0) if `X2'==1
recode west (2=1) if `X2'==2|`X2'==3
tab west


label var male "Male==1; Female==0"
label var ALG2_total  "No of UBII spells"
label var LEH_total  "No of UBI spells"
label var emp_total  "No of BEH spells"
label var unemp_total  "No of ASU spells"
label var almp_total  "No of MTH spells"
label var schuleFitz  "Education"
label def SF 1"No degree" 2"Voc. training" 11"College degree" 12"University degree"
label val schuleFitz SF
label var hcap  "lega handicap status?"
recode hcap (3=2) (4=3)
label def HC 1"severly disabled (>=50%)" 2"disabled (<50%)" 3"not disabled"
label val hcap HC
label var west "i lives in W Germany"
label var CITIZ "nationality of i"
label def CI 1"German" 2"Turkish" 3"Other european" 4"Other non-european"
label val CITIZ CI
label var age_at_sampl "age at sampling date"
rename age_at age
label var Z "Instrument (Z==1 >> PASS)"
label var D  "Treatment (D==1 >> i is respondent)"
label var found  "i found a job after survey started"

rename Y_b_FALS Y_b_w0
rename Y_t_FALS Y_t_w0

gen waves= 1*wave1+10*wave2+100*wave3 
tab waves

//generate actual treatment indicators for every wave (Table 2 in paper)
gen D1=wave1
recode D1 (.=0)  
gen D2 = 1 if wave1==1 | wave2==1
recode D2 (.=0) 
gen D3 = 1 if wave1==1 | wave2==1 | wave3==1
recode D3 (.=0)  
mata: mata mlib index

keep prs_id HH RESP D1-D3 Z* male ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur unemp_total unemp_m_dur almp_total almp_m_dur CITIZ diff_empyer last_job west age schuleFitz  Y_* t_n_job found wave*


save "$d_work/IVdata", replace

exit


********************************************************************************

clear all
use  "$d_work/IVdata", replace
mata: mata mlib index



//restrict control group Z==0 to sample size of treatment assigned group: original control group larger
//thus, we subset it to be equal to Z==1
preserve
	keep if Z==1
	tempfile A
	save `A', replace
restore
set seed 10289
keep if Z==0
gen F=rnormal() if Z==0
sort F
keep if _n<=38350|Z==1			
	
append using `A'

save "$d_work/IVdata_final",  replace

// Response rate and linkage calculations for Table1 HH
preserve
	keep if HH==1
	tab wave1 if HH==1	
	tab wave2 if HH==1
	tab wave3 if HH==1

	egen wave_total=rowtotal(wave1 wave2 wave3)
	tab wave_to if Z==1 & wave_to!=0
	gen wAAA = wave3+10*wave2+100*wave1+9000
	tab wAAA
restore
//Table1 individuals
preserve
	tab wave1 
	tab wave2  
	tab wave3  

	egen wave_total=rowtotal(wave1 wave2 wave3)
	tab wave_to if Z==1 & wave_to!=0
	gen wAAA = wave3+10*wave2+100*wave1+9000
	tab wAAA
	//table response patterns
	tab wAAA if wAAA>=9100
restore


clear all 
use "$d_work/IVdata_final",  replace

//Randomization Figure in Appendix: Estimation
reg Z age male  west Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur 		 unemp_total unemp_m_dur diff_empyer last_job i.CITIZ i.schuleFitz
 		 
//some labelling for figure		 
label var male "Male"		 
label var ALG2_total "# of unemployment benefit II spells"	 
label var ALG2_m "Mean duration of unemployment benefit II spells"	
label var LEH_t "# of unemployment benefit I spells"
label var LEH_m "Mean duration of unemployment benefit I spells"
label var emp_t "# of employment spells"
label var emp_m "Mean duration of employment"
label var unemp_t "# of unemployment spells"
label var unemp_m "Mean duration of unemployment"
label var diff_empyer "Change of employer in the past"
label var west "Place of residence: Western Germany"
label var age "Age"
label var last "Years since last job"

label def REF2 1"No degree (ref. category)" 2"Voc. training" 11"College degree" 12"University degree"
label val schule REF2

label def REF1 1"German (ref. category)" 2"Turkish" 3"Other European" 4"Other non-European"
label val CIT REF1

//Actual figure
coefplot, drop(_cons) xline(0) omitted base headings( Y_t_w0 = "{bf:Labor market history}" 1.CITIZ = "{bf:Nationality}" 1.schuleFitz = "{bf:Education}" , labcolor(black)) scheme(s1mono) graphregion(margin(zero)) 
	 



***********ITT Estimates (Figure 1) plot*********************************************************************
clear all 
use "$d_work/IVdata_final",  replace


//Transform to percent
foreach v of varlist Y_b_w* {
replace `v'=`v'*100 
}


//ITT after wave 1 - count outcome
gen Z1=Z 
label var Z1 "After wave one"
reg Y_t_w1 Z1 age male west  Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer  last_job  i.CITIZ i.schuleFitz
estimates store W1
//ITT after wave 1 - binary outcome
reg Y_b_w1 Z1 age male west  Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer  last_job  i.CITIZ i.schuleFitz
estimates store Wb1


//ITT after wave 2 - count outcome
gen Z2=Z1	
label var Z2 "After wave two"
reg Y_t_w2 Z2 age male west Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer last_job  i.CITIZ i.schuleFitz	
estimates store W2
//ITT after wave 2 - binary outcome		 
reg Y_b_w2 Z2 age male west Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer  last_job  i.CITIZ i.schuleFitz	
estimates store Wb2	


//ITT after wave 3 - count outcome
gen Z3=Z2
label var Z3 "After wave three"
reg Y_t_w3 Z3 age male west  Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur unemp_total unemp_m_dur diff_empyer last_job  i.CITIZ i.schuleFitz	
estimates store W3
//ITT after wave 3 - binary outcome		 
reg Y_b_w3 Z3 age male west  Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur unemp_total unemp_m_dur diff_empyer last_job  i.CITIZ i.schuleFitz	
estimates store Wb3


//ITT before wave 1 (falsification test) - count outcome
gen Z0=Z
label var Z0 "Before wave one"
reg Y_t_w0 Z0 age male west  ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer last_job  i.CITIZ i.schuleFitz
estimates store W0
//ITT before wave 1 (falsification test) - binaryoutcome
reg Y_b_w0 Z0 age male west  ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur 	 unemp_total unemp_m_dur diff_empyer last_job  i.CITIZ i.schuleFitz
estimates store Wb0			 


//Actual ITT plot for count outcome - Figure 1 left panel
coefplot (W0 \ W1 \ W2 \ W3), drop(_cons age male Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur unemp_total unemp_m_dur diff_empyer last_job west 1.CITIZ 2.CITIZ 3.CITIZ 4.CITIZ 1.schuleFitz 2.schuleFitz 11.schuleFitz 12.schuleFitz) xline(0) scheme(s1mono) name(ITTnum, replace) title("Number of ALMP" "participations")  xlabel(-.1(.1).3) msize(medsmall)

//Actual ITT plot for binary outcome - Figure 1 right panel		  
coefplot (Wb0 \ Wb1 \ Wb2 \ Wb3), drop(_cons age male Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur unemp_total unemp_m_dur diff_empyer last_job west 1.CITIZ 2.CITIZ 3.CITIZ 4.CITIZ 1.schuleFitz 2.schuleFitz 11.schuleFitz 12.schuleFitz) ylabel(none) xline(0) aspect(1.48, placement(left))	scheme(s1mono) name(ITTbin, replace) title("ALMP participation indicator""(in percentage points)")	xlabel(-5(5)10) msize(medsmall)	  

//Combine the two panels	----Figure 1 in paper
graph combine ITTnum ITTbin, xsize(7) graphregion(margin(zero)) 


/////////////Coefplot for LARF estimates from R (R code for LARF estimation follows below) - Figure 2
clear all
use "$d_work/LARF_SE.dta", replace

//transform binary outcome to percent
foreach v of varlist b se ub lb {
replace `v'=`v'*100 in 5/8
}

//preparation for Figure 2
mkmat b, matrix(B)
mat rownames B = "Before wave one" "After wave one" "After wave two" "After wave three" "Before wave one" "After wave one" "After wave two" "After wave three" 
mkmat ub, matrix(UB)
mkmat lb, matrix(LB)
mat A = (B , LB , UB)		  
mat NUM = A[1..4,1...]		  
mat BIN = A[5..8,1...]	


//Figure 2 left panel (count outcome)
coefplot (matrix(NUM[,1]), ci((NUM[,2] NUM[,3]))),  xline(0) scheme(s1mono) name(LARFNUM, replace) title("Number of ALMP ""participations") 


//Figure 2 right panel (binary outcome)
coefplot (matrix(BIN[,1]), ci((BIN[,2] BIN[,3]))),  xline(0) ylabel(none) xline(0)  aspect(1.48, placement(left)) scheme(s1mono)  name(LARFBIN, replace) title("ALMP participation indicator""(in percentage points)") 	
	
//Combine the two panels	----Figure 2 in paper	
graph combine LARFNUM LARFBIN, xsize(7) graphregion(margin(zero)) 


//Weak Identification and partial R-sq test (Section 3.2)	 
mata: mata mlib index
use "$d_work/IVdata_final",  replace
ivreg2 Y_t_w3 age male west Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur unemp_total unemp_m_dur diff_empyer  last_job  i.CITIZ i.schuleFitz (D3=Z), robust first
mat list e(first)



//Results reported in Section 4.2 		 
//No effect on job search behavior	
//ITT estimates count ALMP

reg t_n_job Z age male west  Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer  last_job  i.CITIZ i.schuleFitz

//ITT estimates binary ALMP

replace found = found*100
reg found Z age male west  Y_t_w0 ALG2_total ALG2_m_dur LEH_total LEH_m_dur emp_total emp_m_dur  unemp_total unemp_m_dur diff_empyer  last_job  i.CITIZ i.schuleFitz
