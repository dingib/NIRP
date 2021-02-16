********************************************************************************
*
*   Stata script used for the paper:
*   "Accuracy of self-assessment of real-life functioning in schizophrenia: evidence from the Italian Network for Research on Psychoses"
*   published on npj Schizophrenia (https://www.nature.com/articles/s41537-021-00140-9)
*
*   2020-21 Dino Gibertoni, Department of Biomedical and Neuromotor Sciences, University of Bologna
*   dino.gibertoni2@unibo.it   dgibertoni@gmail.com
*
********************************************************************************




clear all
     
**** DATA LOADING
*** Given the way that data was recorded and distributed in the NIRP project, 
***   we had to load data from multiple datasets, as explained in the a. b. c. and d. steps
*** Thus, the above-mentioned steps, and the e. step (merging) aren't necessary per se.
*** To carry out the analyses, users should prepare a dataset able to reproduce commands from line 126 onwards.
*** Please refer to the dataset structure described in the codebook.log file

tempfile cg itemsABD MSCEIT

*** a. import the dataset where the type of caregiver was recorded
import excel "/Users/dinogibertoni/WORK/NIRP/dati/WAVE 2/2020-05-slof-CG.xlsx", sheet("Foglio1") firstrow clear

*** The caregiver info has been recorded freely with several different modalities
*** here we create the "cg_group" variable collapsing the "Caregiver" variable in 3 categories: 1 "Relative/Friend" 2 "Healthcare op." 3 "Clinician"
***   and store it in the temporary dataset cg
replace Caregiver=proper(Caregiver)
gen cg_group= cond(Caregiver=="Sorella" | Caregiver=="Nipote" | Caregiver=="Madre" | Caregiver=="Genitore" | Caregiver=="Ex Moglie" | Caregiver=="Cugin*" | Caregiver=="Convivente" | ///
	Caregiver=="Conquilino" | Caregiver=="Coniuge" | Caregiver=="Compagn*" | Caregiver=="Amico" | Caregiver=="Cognata" | Caregiver=="Familiare" | Caregiver=="Figlio" | ///
	Caregiver=="Fratello"   | Caregiver=="Padre",1, /// collapsing "Relative/Friend" category
		cond(Caregiver=="Aiuto Domestico" | Caregiver=="Assistente Sociale" | Caregiver=="Badante" | Caregiver=="Case Manager" | Caregiver=="Educatore" | Caregiver=="Infermiere" | ///
			 Caregiver=="Informatore" | Caregiver=="Operatore" | Caregiver=="Personale Del Gruppo Appartemento" | Caregiver=="Psicologa" | Caregiver=="Psicologo" | ///
			 Caregiver=="Psicoterapeuta Csm" | Caregiver=="Terapeuta Pz" | Caregiver=="Terp Comunit√†",2,cond(Caregiver!="",3,.))) 
label define cg 1 "Relative/Friend" 2 "Healthcare op." 3 "Clinician"
label values cg_group cg
save `cg'

*** b. import items from the SLOF dimensions (A, B and D)
import delimited "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/2020-04 slof-ABD.csv", case(preserve) clear 
save `itemsABD'

*** c. import MSCEIT variable from the MCCB scale of neurocognition
***   (this is only because the t-standardized MSCEIT was updated and provided subsequently to the main dataset of the project)
import excel "/Users/dinogibertoni/WORK/NIRP/dati/WAVE 2/nirp_fu_aget.xls", sheet("Foglio1") firstrow clear
keep RespondentID MSCEITME
ren RespondentID Soggetto
replace MSCEITME=. if MSCEITME==-99
save `MSCEIT'

*** d. import the second wave dataset (main dataset)
import delimited "/Users/dinogibertoni/WORK/NIRP/dati/WAVE 2/2019-01-07.csv", case(preserve) encoding(utf8) clear 

*** generates "Apathy" and "Blunted affect" variables adding up the respective items of the BNSS scale
foreach var of varlist bnss* { // assign missing value to the category "9"
	replace `var'=. if `var'==9
	}
gen BNSS_apa_fu= bnss_apatia_1_fu+ bnss_apatia_2_fu
gen BNSS_app_fu= bnss_appiattimento_affettivo_1_f + bnss_appiattimento_affettivo_2_f + bnss_appiattimento_affettivo_3_f
label var BNSS_apa_fu "Apathy at follow-up"
label var BNSS_app_fu "Blunted affect at follow-up"

*** generates the positive symptoms and disorganization variables using the items of the PANSS scale
egen PANSS_pos_fu = rowtotal(panss_p1_fu panss_p3_fu panss_p5_fu panss_g9_fu)
egen PANSS_dis_fu = rowtotal(panss_p2_fu panss_n5_fu panss_g11_fu)

ren (soggetto feit_corrette_fu tasit_sezione_1_totale_fu tasit_sezione_2_totale_fu tasit_sezione_3_totale_fu) (Soggetto FEIT_fu TASIT1_fu TASIT2_fu TASIT3_fu)

*** creates the unit identifier by reading into the patient identifier
gen centro=substr(Soggetto,1,2), a(Soggetto)
encode centro, gen(Centro)

*** date of enrolment in the study
gen data_recl=date(data_reclutamento_fu, "YMD"), b(data_reclutamento_fu)
format data_recl %tdDD.NN.CCYY

*** frequency of control
encode frequenza_visite_controllo, gen(freq_cont)
label define freq_cont 1 "Annuale" 2 "Semestrale" 3 "Trimestrale" 4 "Bimestrale" 5 "Mensile" 6 "2 volte/mese" 7 "3 volte/mese" 8 "Settimanale", replace

keep Soggetto Centro genere eta_fu data_recl scolarita_fu ricovero_corrente slof* PANSS_pos_fu PANSS_dis_fu BNSS_apa_fu BNSS_app_fu cdss* FEIT_fu TASIT*_fu upsa_b_totale_fu mccb* cpt_ip_totale_fu

*** e. merge the datasets created in the a to c steps with the main dataset by patient ID (Soggetto)
merge 1:1 Soggetto using `MSCEIT', nogen keep(match)
merge 1:1 Soggetto using `itemsABD', nogen
merge 1:1 Soggetto using `cg', nogen

*** merge with the dataset including data on patients' recovery
merge 1:1 Soggetto using "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/recovery.dta", nogen
ren rem_sx_fx_FU recovered

*** a bit of data cleaning
order Soggetto cg Caregiver genere eta_fu recovered PANSS* cdss_totale_fu mccb* cpt_ip_totale_fu upsa_b_totale_fu slof_pz* slof_cg*
ren *_fu *
gen females = genere=="F"

*** creates the 'processing speed' and 'working memory' items of MCCB Neurocognition
***   first, we need to reverse the scale of TMT in order to have higher values indicating higher cognitive functioning
***   then, the two variables are computed as the mean of the related items
gen TMT_inv= -(mccb_tmt-300)
egen proc_sp=rowmean(TMT_inv mccb_bacs_sc mccb_fluenza)
egen wrk_mem=rowmean(mccb_wms_iii mccb_lns)

*** creates indicators for each type of caregiver
gen relatives=cg_group==1
gen healthop=cg_group==2
gen clinicians=cg_group==3

*** neatly renames variables of the six SLOF scale's dimensions (a, b, c, d, e and f). Dimension assessed by both patient and caregiver.
foreach dim in a b c d e f {
	ren slof_cg_totale_`dim' cg`dim'
	ren slof_pz_totale_`dim' pz`dim'
	}
	
*** computes the differences between patient and caregiver (diff_*) on the same dimension and the mean of patient's and caregiver's score
foreach dim in a b c d e f {
	gen diff_`dim' = pz`dim' - cg`dim'
	gen mean_`dim' = (cg`dim' + pz`dim')/2
	}

**** END OF DATA LOADING AND CLEANING

*** summarises the SLOF items separately by patient and caregiver. Grouping variables by SLOF dimension
summ slof*a_*
summ slof*b_*
summ slof*c_*
summ slof*d_*
summ slof*e_*
summ slof*f_*

*** tabulates and tests dichotomous variables against caregiver group
foreach var of varlist females recovered ricovero_corrente  {
		tab `var' cg_group, chi exact col nokey
	}

**** computes correlations between continuous variables and SLOF dimensions assessed by caregivers
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		pwcorr `var' cga cgb cgc cgd cge cgf, sig
	}
	
**** computes correlations between continuous variables and SLOF dimensions assessed by patients
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		pwcorr `var' pza pzb pzc pzd pze pzf, sig
	}
	
**** computes correlations between continuous variables and the differences between patients and caregivers for each SLOF dimension
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		pwcorr `var' diff_a diff_b diff_c diff_d diff_e diff_f, sig
	}

**** one-way ANOVA for normally distributed continuous variables within caregiver groups
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		oneway `var' cg_group, tabulate
	}
	
*** as before, adding posthoc analyses only for variables significant at ANOVA
foreach var of varlist eta scolarita BNSS_app PANSS_dis mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale wrk_mem FEIT TASIT2 TASIT3 MSCEIT upsa_b_totale {
		oneway `var' cg_group, scheffe
	}

*** Kruskal-Wallis test for non-normally distributed continuous variables within caregiver groups
***   (this is a subscript that produces a simpler output)
foreach var of varlist PANSS_pos proc_sp TASIT1 {
		quietly kwallis `var', by(cg_group)
		scalar p = chi2tail(r(df), r(chi2))			// compute p-value
		local p2 : display %6.3f p					// create a macro in order to format p (and z) value
		display as text "`var':" _column(37) "chi2=" as result %5.2f `r(chi2)' as text ", p=" as result %5.3f `p2'
	}
	
*** post-hoc analysis for the variables significant at the previous Kruskal-Wallis test
conovertest PANSS_pos, by(cg_group) ma(holm)
conovertest TASIT1, by(cg_group) ma(holm)


***  ANALYSIS BY EACH SLOF DIMENSION
*** From here on, SLOF items grouped by SLOF dimension are compared between patient and caregiver
***  items  1-5: Physical functioning; 
***			6-12: Personal care skills;
***			13-19: Interpersonal relationships;
***			20-26: Social acceptability;
***			27-37: Activities;
***			38-43: Work skills

*** summary statistics
tabstat slof_cg_a*, statistics( count mean sd ) by(cg_group) nototal
tabstat slof_cg_b*, statistics( count mean sd ) by(cg_group) nototal
tabstat slof_cg_c*, statistics( count mean sd ) by(cg_group) nototal
tabstat slof_cg_d*, statistics( count mean sd ) by(cg_group) nototal
tabstat slof_cg_e*, statistics( count mean sd ) by(cg_group) nototal
tabstat slof_cg_f*, statistics( count mean sd ) by(cg_group) nototal


*** WILCOXON MATCHED-PAIRS TEST
forval a=1/5 {
		quietly signrank slof_pz_a_`a' = slof_cg_a_`a'
		local pi=cond(r(z)<0,r(z),-r(z))	 
		scalar p = normal(`pi')*2 			// compute p-value
		local p2 : display %6.3f p			// create a macro in order to format p (and z) value
		display as text "Ho: SLOF_pz = SLOF_cg item `a':" _column(37) "z =" as result %5.2f `r(z)' as text ", Prob > |z| =" as result %5.3f `p2'
	}

forval b=6/12 {
		quietly signrank slof_pz_b_`b' = slof_cg_b_`b'
		local pi=cond(r(z)<0,r(z),-r(z))	 
		scalar p = normal(`pi')*2			
		local p2 : display %6.3f p			
		display as text "Ho: SLOF_pz = SLOF_cg item `b':" _column(37) "z =" as result %5.2f `r(z)' as text ", Prob > |z| =" as result %5.3f `p2'
	}

forval c=13/19 {
		quietly signrank slof_pz_c_`c' = slof_cg_c_`c'
		local pi=cond(r(z)<0,r(z),-r(z))	 
		scalar p = normal(`pi')*2			
		local p2 : display %6.3f p			
		display as text "Ho: SLOF_pz = SLOF_cg item `c':" _column(37) "z =" as result %5.2f `r(z)' as text ", Prob > |z| =" as result %5.3f `p2'
	}

forval d=20/26 {
		quietly signrank slof_pz_d_`d' = slof_cg_d_`d'
		local pi=cond(r(z)<0,r(z),-r(z))	 
		scalar p = normal(`pi')*2			
		local p2 : display %6.3f p			
		display as text "Ho: SLOF_pz = SLOF_cg item `d':" _column(37) "z =" as result %5.2f `r(z)' as text ", Prob > |z| =" as result %5.3f `p2'
	}

forval e=27/37 {
		quietly signrank slof_pz_e_`e' = slof_cg_e_`e'
		local pi=cond(r(z)<0,r(z),-r(z))	
		scalar p = normal(`pi')*2			
		local p2 : display %6.3f p			
		display as text "Ho: SLOF_pz = SLOF_cg item `e':" _column(37) "z =" as result %5.2f `r(z)' as text ", Prob > |z| =" as result %5.3f `p2'
	}
	
forval f=38/43 {
		quietly signrank slof_pz_f_`f' = slof_cg_f_`f'
		local pi=cond(r(z)<0,r(z),-r(z))	
		scalar p = normal(`pi')*2			
		local p2 : display %6.3f p			
		display as text "Ho: SLOF_pz = SLOF_cg item `f':" _column(37) "z =" as result %5.2f `r(z)' as text ", Prob > |z| =" as result %5.3f `p2'
	}
	
**** Cohen kappa and GWET's Agreement Coefficient
forval a=1/5 {
	kappaetc slof_pz_a_`a' slof_cg_a_`a', benchmark
	}

forval b=6/12 {
	kappaetc slof_pz_b_`b' slof_cg_b_`b', benchmark wgt(ordinal)
	}

forval c=13/19 {
	kappaetc slof_pz_c_`c' slof_cg_c_`c', benchmark wgt(ordinal)
	}

forval d=20/26 {
	kappaetc slof_pz_d_`d' slof_cg_d_`d', benchmark wgt(ordinal)
	}

forval e=27/37 {
	kappaetc slof_pz_e_`e' slof_cg_e_`e', benchmark wgt(ordinal)
	}

forval f=38/43 {
	kappaetc slof_pz_f_`f' slof_cg_f_`f', benchmark wgt(ordinal)
	}


**** T-TEST performed on the overall patients and caregivers' scores of each SLOF dimension
foreach dim in a b c d e f {
	ttest pz`dim' == cg`dim'
	}

**** CONCORDANCE evaluation 
**** Bland-Altman analysis on the 6 dimensions (ref. slides Newson 2019)

*** scatterplots of observed data
twoway (scatter pza cga, sort msize(vsmall)) ///
	(scatteri 5 5 28 28, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)30, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)30, grid glwidth(medium) glpattern(tight_dot)) title("Physical functioning") legend(off) ///
	scheme(Mazzanti) name(scatter_a, replace) xsize(5) ysize(5) plotregion(lcolor(none))
twoway (scatter pzb cgb, sort msize(vsmall)) ///
	(scatteri 5 5 40 40, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)40, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)40, grid glwidth(medium) glpattern(tight_dot)) title("Personal care skills") legend(off) ///
	scheme(Mazzanti) name(scatter_b, replace) xsize(5) ysize(5) plotregion(lcolor(none))
twoway (scatter pzc cgc, sort msize(vsmall)) ///
	(scatteri 5 5 40 40, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)40, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)40, grid glwidth(medium) glpattern(tight_dot)) title("Interpersonal relationships") legend(off) ///
	scheme(Mazzanti) name(scatter_c, replace) xsize(5) ysize(5) plotregion(lcolor(none))
twoway (scatter pzd cgd, sort msize(vsmall)) ///
	(scatteri 5 5 40 40, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)40, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)40, grid glwidth(medium) glpattern(tight_dot)) title("Social acceptability") legend(off) ///
	scheme(Mazzanti) name(scatter_d, replace) xsize(5) ysize(5) plotregion(lcolor(none))
twoway (scatter pze cge, sort msize(vsmall)) ///
	(scatteri 5 5 60 60, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)60, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)60, grid glwidth(medium) glpattern(tight_dot)) title("Activities") legend(off) ///
	scheme(Mazzanti) name(scatter_e, replace) xsize(5) ysize(5) plotregion(lcolor(none))
twoway (scatter pzf cgf, sort msize(vsmall)) ///
	(scatteri 5 5 35 35, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)35, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)35, grid glwidth(medium) glpattern(tight_dot)) title("Work skills") legend(off) ///
	scheme(Mazzanti) name(scatter_f, replace) xsize(5) ysize(5) plotregion(lcolor(none))
graph combine scatter_a scatter_b scatter_c scatter_d scatter_e scatter_f, rows(2) xsize(6) scheme(Mazzanti) name(scatters, replace)


*** Bland-Altman
kappaetc pzc cgc, keep loa ///
	lineopts(lcol(red midblue midblue) lwi(thick thick thick) lpat(solid solid solid)) ///
	scatteropts(mlwidth(thin) msize(vsmall) msym(circle)) ///
	twowayopts(name(BAc, replace) scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) legend(off) ///
		title("SLOF Interpersonal Relationships") caption("") xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(7(7)35, grid glwidth(thin)) ///
		ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-20 -10(5)10 20, angle(hor) grid glwidth(thin)))
gen pz_undr_c= _pairdiff < _lowerloa									// generate a binomial variable to map patients above or under loa
gen pz_over_c= _pairdiff > _upperloa if _pairdiff<.

drop _pairmean _pairdiff _meandiff _lowerloa _upperloa
kappaetc pze cge, keep loa ///
	lineopts(lcol(red midblue midblue) lwi(thick thick thick) lpat(solid solid solid)) ///
	scatteropts(mlwidth(thin) msize(vsmall) msym(circle)) ///
	twowayopts(name(BAe, replace) scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) legend(off) ///
		title("SLOF Everyday Life Skills") caption("") xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(12(6)60, grid glwidth(thin)) ///
		ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-20 -10(5)10 20, angle(hor) grid glwidth(thin)))
gen pz_undr_e= _pairdiff < _lowerloa
gen pz_over_e= _pairdiff > _upperloa if _pairdiff<.

drop _pairmean _pairdiff _meandiff _lowerloa _upperloa
kappaetc pzf cgf, keep loa ///
	lineopts(lcol(red midblue midblue) lwi(thick thick thick) lpat(solid solid solid)) ///
	scatteropts(mlwidth(thin) msize(vsmall) msym(circle)) ///
	twowayopts(name(BAf, replace) scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) legend(off) ///
		title("SLOF Work Skills") caption("") xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(6(6)30, grid glwidth(thin)) ///
		ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-20 -10(5)10 20, angle(hor) grid glwidth(thin)))
gen pz_undr_f= _pairdiff < _lowerloa
gen pz_over_f= _pairdiff > _upperloa if _pairdiff<.
graph combine BAc BAf BAe, rows(2) ysize(5) scheme(Mazzanti) name(BA, replace) altshrink
graph export "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/grafici/Fig1.png", replace name(BA)
	
*** compute concordance with Somers' D and Kendall's Tau
somersd cga pza, taua transf(z) tdist
somersd cgb pzb, taua transf(z) tdist
somersd cgc pzc, taua transf(z) tdist
somersd cgd pzd, taua transf(z) tdist
somersd cge pze, taua transf(z) tdist
somersd cgf pzf, taua transf(z) tdist

scsomersd diff_a 0, transf(z) tdist
scsomersd diff_b 0, transf(z) tdist
scsomersd diff_c 0, transf(z) tdist
scsomersd diff_d 0, transf(z) tdist
scsomersd diff_e 0, transf(z) tdist
scsomersd diff_f 0, transf(z) tdist

somersd mean_c diff_c, taua transf(z) tdist
somersd mean_e diff_e, taua transf(z) tdist
somersd mean_f diff_f, taua transf(z) tdist


*** MULTIVARIABLE REGRESSION MODELS OF DIFFERENCES IN SLOF DIMENSIONS C, E, F

*** first, multiple imputation using chained equations was performed in order to use all available data
mi set wide
mi register imputed scolarita upsa_b_totale cdss_totale mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT BNSS_apa BNSS_app

set seed 19266
mi impute chained (pmm, knn(3)) scolarita upsa_b_totale cdss_totale mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT BNSS_apa BNSS_app ///
	= eta females Centro recovered pza pzb pzc pzd pze pzf, add(10) force

*** regression models were carried out using multivariable fractional polynomials on the imputed data sets
*** after the mfpmi command that performs multivariable regression, plots are obtained for the significant variables
mfpmi, alpha(0.05) select(.05, cgc:1): regr diff_c cgc females eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale
fracplot cgc, name(c_cgc, replace)  scheme(Mazzanti) plotregion(lcolor(none)) title("Caregiver score") xtit("") ytit("{bf:SLOF Interpersonal Relationships}" "Patient-caregiver difference", size(large)) ///
	ylab(-20(5)20, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
fracplot BNSS_apa, name(c_apa, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Avolition")  xtit("") ytit(" " " ") ///
	ylab(-20(5)20, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
fracplot PANSS_pos, name(c_pos, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Positive symptoms")  xtit("") ytit(" " " ")  ///
	ylab(-20(5)20, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
	
mfpmi, alpha(0.05) select(.05, cge:1): regr diff_e cge females eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale
fracplot cge, name(e_cge, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Caregiver score")  xtit("") ytit("{bf:SLOF Everyday Life Skills}" "Patient-caregiver difference", size(large)) ///
	ylab(-30(10)30, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
fracplot BNSS_app, name(e_app, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Expressive Deficit")  xtit("") ytit(" " " ") ///
	ylab(-30(10)30, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
fracplot PANSS_dis, name(e_dis, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Disorganization")  xtit("") ytit(" " " ") ///
	ylab(-30(10)30, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
	
mfpmi, alpha(0.05) select(.05, cgf:1): regr diff_f cgf females eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab  cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale
fracplot cgf, name(f_cgf, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Caregiver score")  xtit("") ytit("{bf:SLOF Work Skills}" "Patient-caregiver difference", size(large)) ///
	ylab(-15(5)15, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
fracplot proc_sp, name(f_psp, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Processing speed")  xtit("") ytit(" " " ") ///
	ylab(-15(5)15, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))
fracplot BNSS_apa, name(f_apa, replace) scheme(Mazzanti) plotregion(lcolor(none)) title("Avolition")  xtit("") ytit("  " "  ") ///
	ylab(-15(5)15, angle(hor) labs(large)) xlab(, labs(large)) yline(0, lcol(red)) msize(small) msym(Oh) mlw(vvthin) mcol(gs10) lineopts(lcol(blue)) ciopts(col(ltblue))

*** combined graphs obtained with and without the altshrink option
graph combine c_cgc c_apa c_pos e_cge e_app e_dis f_cgf f_apa f_psp, scheme(Mazzanti) plotregion(lcolor(none)) name(mfp1, replace) ysize(5) altshrink
graph combine c_cgc c_apa c_pos e_cge e_app e_dis f_cgf f_apa f_psp, scheme(Mazzanti) plotregion(lcolor(none)) name(mfp2, replace) ysize(5)

graph export "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/grafici/Fig2a.png", replace name(mfp1)
graph export "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/grafici/Fig2b.png", replace name(mfp2)

*** END OF SCRIPT


