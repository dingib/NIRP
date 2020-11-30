clear all

tempfile cg itemsABD MSCEIT

*** PRENDO IL DATO DEL TIPO CAREGIVER
import excel "/Users/dinogibertoni/WORK/NIRP/dati/WAVE 2/2020-05-slof-CG.xlsx", sheet("Foglio1") firstrow clear
replace Caregiver=proper(Caregiver)
gen cg_group= cond(Caregiver=="Sorella" | Caregiver=="Nipote" | Caregiver=="Madre" | Caregiver=="Genitore" | Caregiver=="Ex Moglie" | Caregiver=="Cugin*" | Caregiver=="Convivente" | ///
	Caregiver=="Conquilino" | Caregiver=="Coniuge" | Caregiver=="Compagn*" | Caregiver=="Amico" | Caregiver=="Cognata" | Caregiver=="Familiare" | Caregiver=="Figlio" | ///
	Caregiver=="Fratello"   | Caregiver=="Padre",1, ///
		cond(Caregiver=="Aiuto Domestico" | Caregiver=="Assistente Sociale" | Caregiver=="Badante" | Caregiver=="Case Manager" | Caregiver=="Educatore" | Caregiver=="Infermiere" | ///
			 Caregiver=="Informatore" | Caregiver=="Operatore" | Caregiver=="Personale Del Gruppo Appartemento" | Caregiver=="Psicologa" | Caregiver=="Psicologo" | ///
			 Caregiver=="Psicoterapeuta Csm" | Caregiver=="Terapeuta Pz" | Caregiver=="Terp Comunit√†",2,cond(Caregiver!="",3,.)))
label define cg 1 "Relative/Friend" 2 "Healthcare op." 3 "Clinician"
label values cg_group cg
save `cg'

*** PRENDO GLI ITEMS DELLA SLOF DIMENSIONI A B e D
import delimited "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/2020-04 slof-ABD.csv", case(preserve) clear 
save `itemsABD'

*** prendo la MSCEIT
import excel "/Users/dinogibertoni/WORK/NIRP/dati/WAVE 2/nirp_fu_aget.xls", sheet("Foglio1") firstrow clear
keep RespondentID MSCEITME
ren RespondentID Soggetto
replace MSCEITME=. if MSCEITME==-99
save `MSCEIT'

import delimited "/Users/dinogibertoni/WORK/NIRP/dati/WAVE 2/2019-01-07.csv", case(preserve) encoding(utf8) clear 

foreach var of varlist bnss* {
	replace `var'=. if `var'==9
	}
gen BNSS_apa_fu= bnss_apatia_1_fu+ bnss_apatia_2_fu
gen BNSS_app_fu= bnss_appiattimento_affettivo_1_f + bnss_appiattimento_affettivo_2_f + bnss_appiattimento_affettivo_3_f
label var BNSS_apa_fu "Apathy at follow-up"
label var BNSS_app_fu "Blunted affect at follow-up"

egen PANSS_pos_fu = rowtotal(panss_p1_fu panss_p3_fu panss_p5_fu panss_g9_fu)
egen PANSS_dis_fu = rowtotal(panss_p2_fu panss_n5_fu panss_g11_fu)
ren (soggetto feit_corrette_fu tasit_sezione_1_totale_fu tasit_sezione_2_totale_fu tasit_sezione_3_totale_fu) (Soggetto FEIT_fu TASIT1_fu TASIT2_fu TASIT3_fu)
gen centro=substr(Soggetto,1,2), a(Soggetto)
encode centro, gen(Centro)
gen data_recl=date(data_reclutamento_fu, "YMD"), b(data_reclutamento_fu)
format data_recl %tdDD.NN.CCYY
encode frequenza_visite_controllo, gen(freq_cont)
label define freq_cont 1 "Annuale" 2 "Semestrale" 3 "Trimestrale" 4 "Bimestrale" 5 "Mensile" 6 "2 volte/mese" 7 "3 volte/mese" 8 "Settimanale", replace
tab freq_cont

keep Soggetto Centro genere eta_fu data_recl scolarita_fu ricovero_corrente slof* PANSS_pos_fu PANSS_dis_fu BNSS_apa_fu BNSS_app_fu cdss* FEIT_fu TASIT*_fu upsa_b_totale_fu mccb* cpt_ip_totale_fu


merge 1:1 Soggetto using `MSCEIT', nogen keep(match)
merge 1:1 Soggetto using `itemsABD', nogen
merge 1:1 Soggetto using `cg', nogen
merge 1:1 Soggetto using "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/recovery.dta", nogen
ren rem_sx_fx_FU recovered
order Soggetto cg Caregiver genere eta_fu recovered PANSS* cdss_totale_fu mccb* cpt_ip_totale_fu upsa_b_totale_fu slof_pz* slof_cg*
ren *_fu *
gen females = genere=="F"
gen TMT_inv= -(mccb_tmt-300)
egen proc_sp=rowmean(TMT_inv mccb_bacs_sc mccb_fluenza)
egen wrk_mem=rowmean(mccb_wms_iii mccb_lns)

gen relatives=cg_group==1
gen healthop=cg_group==2
gen clinicians=cg_group==3

foreach let in a b c d e f {
	ren slof_cg_totale_`let' cg`let'
	ren slof_pz_totale_`let' pz`let'
	}
foreach dim in a b c d e f {
	gen diff_`dim' = pz`dim' - cg`dim'
	gen mean_`dim' = (cg`dim' + pz`dim')/2
	}


summ slof*a_*
summ slof*b_*
summ slof*c_*
summ slof*d_*
summ slof*e_*
summ slof*f_*

gen cg_miss=cg_group==.

foreach var of varlist females recovered ricovero_corrente  {
		tab `var' cg_group, chi exact col nokey
	}

**** CORRELAZIONI TRA PREDITTORI E SLOF CAREGIVER
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		pwcorr `var' cga cgb cgc cgd cge cgf, sig
	}
	
**** CORRELAZIONI TRA PREDITTORI E SLOF PAZIENTE
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		pwcorr `var' pza pzb pzc pzd pze pzf, sig
	}
	
**** CORRELAZIONI TRA PREDITTORI E DIFFERENZA PZ-CAREGIVER
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		pwcorr `var' diff_a diff_b diff_c diff_d diff_e diff_f, sig
	}
	
foreach var of varlist eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale {
		oneway `var' cg_group, tabulate
	}
	
*** post-hoc per le significative
foreach var of varlist eta scolarita BNSS_app PANSS_dis mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale wrk_mem FEIT TASIT2 TASIT3 MSCEIT upsa_b_totale {
		oneway `var' cg_group, scheffe
	}

foreach var of varlist PANSS_pos proc_sp TASIT1 {
		quietly kwallis `var', by(cg_group)
		scalar p = chi2tail(r(df), r(chi2))			// calcolo il p-value
		local p2 : display %6.3f p					// assegno il p (e lo z) a una macro per poterlo formattare
		display as text "`var':" _column(37) "chi2=" as result %5.2f `r(chi2)' as text ", p=" as result %5.3f `p2'
	}
conovertest PANSS_pos, by(cg_group) ma(holm)
conovertest TASIT1, by(cg_group) ma(holm)

*** statistiche per tipo caregiver
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
		scalar p = normal(`pi')*2			
		local p2 : display %6.3f p			
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


**** CONCORDANZE DELLE DIMENSIONI
*summ slof*totale*

foreach dim in a b c d e f {
	ttest pz`dim' == cg`dim'
	}


**** analisi Bland-Altman sulle 6 dimensioni (ref. slides Newson 2019

/*** scatterplot dati osservati
twoway (scatter pza cga, sort msize(vsmall)) ///
	(scatteri 5 5 28 28, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)30, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)30, grid glwidth(medium) glpattern(tight_dot)) title("Physical functioning") legend(off) ///
	scheme(Mazzanti) name(scatter_a, replace) xsize(5) ysize(5) plotregion(lcolor(none))

twoway (scatter slof_pz_totale_b slof_cg_totale_b_fu, sort msize(vsmall)) ///
	(scatteri 5 5 40 40, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)40, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)40, grid glwidth(medium) glpattern(tight_dot)) title("Personal care skills") legend(off) ///
	scheme(Mazzanti) name(scatter_b, replace) xsize(5) ysize(5) plotregion(lcolor(none))

twoway (scatter slof_pz_totale_c slof_cg_totale_c_fu, sort msize(vsmall)) ///
	(scatteri 5 5 40 40, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)40, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)40, grid glwidth(medium) glpattern(tight_dot)) title("Interpersonal relationships") legend(off) ///
	scheme(Mazzanti) name(scatter_c, replace) xsize(5) ysize(5) plotregion(lcolor(none))

twoway (scatter slof_pz_totale_d slof_cg_totale_d_fu, sort msize(vsmall)) ///
	(scatteri 5 5 40 40, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)40, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)40, grid glwidth(medium) glpattern(tight_dot)) title("Social acceptability") legend(off) ///
	scheme(Mazzanti) name(scatter_d, replace) xsize(5) ysize(5) plotregion(lcolor(none))

twoway (scatter slof_pz_totale_e slof_cg_totale_e_fu, sort msize(vsmall)) ///
	(scatteri 5 5 60 60, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)60, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)60, grid glwidth(medium) glpattern(tight_dot)) title("Activities") legend(off) ///
	scheme(Mazzanti) name(scatter_e, replace) xsize(5) ysize(5) plotregion(lcolor(none))

twoway (scatter slof_pz_totale_f slof_cg_totale_f_fu, sort msize(vsmall)) ///
	(scatteri 5 5 35 35, recast(connected) connect(direct) msymbol(point)), ///
	ytitle(Patient) ylabel(0(5)35, angle(horizontal) grid glwidth(medium) glpattern(tight_dot)) ///
	xtitle(Caregiver) xlabel(0(5)35, grid glwidth(medium) glpattern(tight_dot)) title("Work skills") legend(off) ///
	scheme(Mazzanti) name(scatter_f, replace) xsize(5) ysize(5) plotregion(lcolor(none))

graph combine scatter_a scatter_b scatter_c scatter_d scatter_e scatter_f, rows(2) xsize(6) scheme(Mazzanti) name(scatters, replace)
*/

*** Bland-Altman
drop _pairmean _pairdiff _meandiff _lowerloa _upperloa
kappaetc pzc cgc, keep loa ///
	lineopts(lcol(red midblue midblue) lwi(thick thick thick) lpat(solid solid solid)) ///
	scatteropts(mlwidth(thin) msize(vsmall) msym(circle)) ///
	twowayopts(name(BAc, replace) scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) legend(off) ///
		title("SLOF Interpersonal Relationships") caption("") xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(7(7)35, grid glwidth(thin)) ///
		ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-20 -10(5)10 20, angle(hor) grid glwidth(thin)))
gen pz_undr_c= _pairdiff < _lowerloa									// crea gli indicatori di paziente sopra/sotto i loa
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


/*concord pzc cgc, loa(lopts(lcol(midblue red midblue) lwi(thick thick thick))) ///
	xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(, grid glwidth(thin)) ///
	ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-20 -10(5)10 20, angle(hor) grid glwidth(thin)) ///
	mlwidth(medthin) legend(off) title("SLOF Interpersonal Relationships") caption("") scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) name(BAc, replace)
concord pze cge, loa(lopts(lcol(midblue red midblue) lwi(thick thick thick))) ///
	xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(, grid glwidth(thin)) ///
	ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-30 -20 -10(5)10 20 30, angle(hor) grid glwidth(thin)) ///
	mlwidth(medthin) legend(off) title("SLOF Everyday Life Skills") caption("") scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) name(BAe, replace)
concord pzf cgf, loa(lopts(lcol(midblue red midblue) lwi(thick thick thick))) ///
	xtitle("Mean of patient and caregiver scores", size(medlarge)) xlab(, grid glwidth(thin)) ///
	ytitle("Patient-Caregiver difference", size(medlarge)) ylab(-20 -10(5)10 20, angle(hor) grid glwidth(thin)) ///
	mlwidth(medthin) legend(off) title("SLOF Work Skills") caption("") scheme(Mazzanti) plotregion(lcolor(none)) xsize(5) ysize(5) name(BAf, replace)
*/
	
*** concordanza con tau di Kendall
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


*** REGRESSIONI SULLE DIFFERENZE

*** multiple imputation (altrimenti la regressione si limita a n=495)
*** l'imputation la faccio per tutte le variabili da usare come predittori nelle diverse regressioni
*** non faccio l'imputation delle dipendenti (le differenze nelle dimensioni)
mi set wide
mi register imputed scolarita upsa_b_totale cdss_totale mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT BNSS_apa BNSS_app

set seed 19266
mi impute chained (pmm, knn(3)) scolarita upsa_b_totale cdss_totale mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT BNSS_apa BNSS_app ///
	= eta females Centro recovered pza pzb pzc pzd pze pzf, add(10) force

*** multivariable fractional polynomials
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

graph combine c_cgc c_apa c_pos e_cge e_app e_dis f_cgf f_apa f_psp, scheme(Mazzanti) plotregion(lcolor(none)) name(mfp1, replace) ysize(5) altshrink
*graph combine c_cgc c_apa c_pos e_cge e_app e_dis f_cgf f_apa f_psp, scheme(Mazzanti) plotregion(lcolor(none)) name(mfp2, replace) ysize(5)

graph export "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/grafici/Fig2a.png", replace name(mfp1)
*graph export "/Users/dinogibertoni/WORK/NIRP/FOLLOW-UP/SLOF pazienti-caregivers/grafici/Fig2b.png", replace name(mfp2)

/*****  dimensioni che non abbiamo messo nel paper
mfpmi, alpha(0.05) select(.05, cga:1): regr diff_a cga females eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale if cga>15
fracplot cga, ylab(-15(5)15, angle(hor)) scheme(Mazzanti) plotregion(lcolor(none)) name(a_cga, replace) yline(0, lcol(red)) msize(small) msym(Oh) mlw(thin)
mfpmi, alpha(0.05) select(.05, cgb:1): regr diff_b cgb females eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale if cgb>10
fracplot cgb, ylab(-15(5)15, angle(hor)) scheme(Mazzanti) plotregion(lcolor(none)) name(b_cgb, replace) yline(0, lcol(red)) msize(small) msym(Oh) mlw(thin)
fracplot proc_sp, ylab(-15(5)15, angle(hor)) scheme(Mazzanti) plotregion(lcolor(none)) name(b_prsp, replace) yline(0, lcol(red)) msize(small) msym(Oh) mlw(thin)
fracplot upsa_b_totale, ylab(-15(5)15, angle(hor)) scheme(Mazzanti) plotregion(lcolor(none)) name(b_upsa, replace) yline(0, lcol(red)) msize(small) msym(Oh) mlw(thin)
mfpmi, alpha(0.05) select(.05, cgd:1): regr diff_d cgd females eta scolarita BNSS_apa BNSS_app PANSS_pos PANSS_dis cdss_totale ///
	mccb_hvlt_r_totale mccb_nab mccb_bvmt_r_totale cpt_ip_totale proc_sp wrk_mem FEIT TASIT1 TASIT2 TASIT3 MSCEIT upsa_b_totale
fracplot cgd, ylab(-15(5)15, angle(hor)) scheme(Mazzanti) plotregion(lcolor(none)) name(d_cgd, replace) yline(0, lcol(red)) msize(small) msym(Oh) mlw(thin)
*/
	
*** CHI ECCEDE I LIMITI DI AGREEMENT
tab pz_undr_c pz_over_c, cell
tab pz_undr_e pz_over_e, cell
tab pz_undr_f pz_over_f, cell

gen exc_LOA_c=cond(pz_undr_c==0 & pz_over_c==0,0,cond(pz_undr_c==1,1,cond(pz_over_c==1,2,.)))
gen exc_LOA_e=cond(pz_undr_e==0 & pz_over_e==0,0,cond(pz_undr_e==1,1,cond(pz_over_e==1,2,.)))
gen exc_LOA_f=cond(pz_undr_f==0 & pz_over_f==0,0,cond(pz_undr_f==1,1,cond(pz_over_f==1,2,.)))
label define exc 0 "agree" 1 "under" 2 "over"
label values exc* exc
tabstat cdss_totale_fu cpt_ip_totale_fu upsa_b_totale_fu wrk_mem_fu mccb_bvmt_r_totale_fu, statistics( count mean ) by(exc) nototal varwidth(9) format(%5.0g)
egen nwLOA=anycount(exc_LOA_c exc_LOA_e exc_LOA_f), v(0)


/**** metodo Newson con ridits - al momento non viene 
wridit slof_cg_totale_e_fu, percent gen(rid_cge)
levelsof rid_cge, local(levels)
polyspline rid_cge, power(3) refpts(`levels') gene(rs_) labprefix(Percent@)
rcentile slof_cg_totale_e_fu, centile(`levels') transf(asin)
regress slof_pz_totale_e rs_*, noconst vce(robust)





