cd "E:\workdata\707565\EA\data"

***************************
**# Nøgler
***************************

*branche
use "E:\Formater\SAS formater i Danmarks Statistik\STATA_datasaet\Brancher\c_db07_l2l5_k.dta", clear
ren DB branche
ren start gr127_db07
save branche10, replace

*uddannelse, over
use "E:\Formater\SAS formater i Danmarks Statistik\STATA_datasaet\Disced\n_audd_hoved_l1l5_k.dta", clear
ren start hfaudd
ren AUDD hfaudd_over
destring hfaudd_over, replace
save hfaudd_over, replace

*uddannelse, mellem
use "E:\Formater\SAS formater i Danmarks Statistik\STATA_datasaet\Disced\n_audd_hoved_l1l4_k.dta", clear
ren start hfaudd
ren AUDD hfaudd_mellem
destring hfaudd_mellem, replace
save hfaudd_mellem, replace

cd "E:\workdata\707565\EA\data"

***************************
**#FUI
***************************
cd "E:\workdata\707565\EA\data"
use "E:\workdata\707565\grunddata.dta\fui2020.dta", clear
keep cvr k_dk_gts rrdin p_forsk a_forsk u_total eco_* inno_pcs_prd inpdgd inpdsv a_total p_total inno_pcs_comm cw
ren inno_pcs_prd inpspd
save fui2020_v, replace

use "E:\workdata\707565\grunddata.dta\fui2018.dta", clear
ren *, lower
keep cvr k_dk_gts rrdin p_forsk a_forsk u_total inno_pcs_prd inpdgd inpdsv a_total p_total inno_pcs_comm cw
ren inno_pcs_prd inpspd
save fui2018_v, replace

forval x=18(2)20{
	use fui20`x'_v, clear
	ren * *`x'
	ren cvrnr`x' cvrnr 
	save fui20`x', replace
	duplicates drop cvr, force
	save fui20`x', replace
}

forval x=11/16{
	use "E:\workdata\707565\grunddata.dta\fuis20`x'.dta", clear 
	ren *, lower
	keep cvr k_dk_gts rrdin p_forsk a_forsk u_total inpdgd inpdsv inpspd a_total p_total cw
	ren * *`x'
	ren cvrnr`x' cvrnr 
	save fui20`x', replace
}

use fui2011, clear
forval x=11/20{
	if inlist(`x', 17, 19) continue
	merge 1:1 cvrnr using fui20`x', gen(mx)
	drop mx
}


global eco eco_eno eco_enu eco_ext eco_mat eco_pol eco_pos eco_rea eco_rec eco_rep eco_sub
reshape long a_forsk a_total inpdgd inpdsv inpspd k_dk_gts p_forsk p_total rrdin u_total cw inno_pcs_comm $eco, i(cvrnr) j(year)
recode k_dk (0=0) (nonmissing=1), gen(kgts)
ren cvrnr cvr

save fui2011_2020, replace

***********************
**#FIRM
***********************

*firm x
forval x = 08/19{
	local X : di %02.0f `x'
	use "E:\workdata\707565\grunddata.dta\firm20`X'x.dta", clear
	ren *, lower
	ren gf_* *
	*merge m:1 gr127_db07 using branche10 
	keep cvr aarsv ansatte eksp import oms vtv /*branche*/
	gen prod_oms = oms / aarsv
	*gen prod_res = aare / aarsv
	gen prod_vtv = vtv / aarsv
	gen eksp_oms = eksp / oms
	ren * *`X'
	ren cvr*`X' cvr
	*save firm20`X'x_d, replace
	duplicates drop cvr, force
	save firm20`X'x, replace
}

use firm2009x, replace
ren *09 *9
save firm2009x, replace
use firm2008x, replace
ren *08 *8
save firm2008x, replace

forval x = 08/19{
	local X : di %02.0f `x'
	merge 1:1 cvr using firm20`X'x, gen(m`X')
	drop m`X'
}
reshape long /*aare*/ /*udg*/ aarsv ansatte eksp import oms vtv /*branche*/ prod_oms /*prod_res*/ prod_vtv eksp_oms, i(cvr) j(year)
save firm2008_2019x, replace

*firm dst
forval x = 08/20{
	local X : di %02.0f `x'
	use "E:\workdata\707565\grunddata.dta\firm20`X'.dta", clear
	ren *, lower
	ren gf_* *
	merge m:1 gr127_db07 using branche10
	keep cvr aare aarsv ansatte eksp import oms vtv branche
	gen prod_oms = oms / aarsv
	gen prod_res = aare / aarsv
	gen prod_vtv = vtv / aarsv
	gen eksp_oms = eksp / oms
	ren * *`X'
	ren cvr*`X' cvr
	save firm20`X'_d, replace
	duplicates drop cvr, force
	save firm20`X', replace
}

use firm2009, replace
ren *09 *9
save firm2009, replace
use firm2008, replace
ren *08 *8
forval x = 09/20{
	local X : di %02.0f `x'
	merge 1:1 cvr using firm20`X', gen(m`X')
	drop m`X'
}
reshape long aare udg aarsv ansatte eksp import oms vtv branche prod_oms prod_res prod_vtv eksp_oms, i(cvr) j(year)
save firm2008_2020, replace

***********************
**#UDDA
***********************

forval x=08/18{
	local X : di %02.0f `x'
	use "E:\workdata\707565\grunddata.dta\udda20`X'.dta", clear 
	merge 1:1 pnr using "E:\workdata\707565\grunddata.dta\udda20`X'09.dta"
	drop _
	keep pnr hfaudd
	merge m:1 hfaudd using hfaudd_over
	drop if _!=3
	drop _
	merge m:1 hfaudd using hfaudd_mellem
	drop if _!=3
	drop _ hfaudd
	save udda20`X'_m, replace
}

forval x=19/20{
	use "E:\workdata\707565\grunddata.dta\udda20`x'09.dta", clear 
	keep pnr hfaudd
	merge m:1 hfaudd using hfaudd_over
	drop if _!=3
	drop _
	merge m:1 hfaudd using hfaudd_mellem
	drop if _!=3
	drop _ hfaudd
	save udda20`x'_m, replace
}

***********************
**# Merge UDDA + RAS
***********************

forval x=08/20{
	local X : di %02.0f `x'
	use "E:\workdata\707565\grunddata.dta\ras20`X'.dta", clear 
	keep pnr cvrnr
	keep if pnr!=""
	sort pnr
	save ras20`X', replace
}

forval y=08/20{
	local Y : di %02.0f `y'
	use ras20`Y', clear
	merge m:1 pnr using udda20`Y'_m
	drop if cvrnr=="" | (hfaudd_over==. & hfaudd_mellem==.)
	drop _

	*antal
	sort cvr
	gen temp0 = 1
	bys cvr: egen ans_udd=total(temp0)
	drop temp0

	recode hfaudd_over (02/39=1 "Kortere") (40=2 "kvu") (50 60=3 "mvu") (70=4 		"lvu") (80=5 "forsker") (90=6 "uoplyst"), gen(hoved_over)
	recode hfaudd_mellem (8020 8225 8030 7025 7030 7020 6030 6025 6020 5030 5025 	5024 5020 4030 4025 4024 4020 = 1 "hum") (8039 7039 6039 5039 5038 4038 = 2 	"samf") (8059 8080 7059 7075 7080 6059 6075 6080 5059 5058 5075 5080 8085 		4058 4075 4080 4085 = 3 "tek") (8035 7035 6035 = 4 "nat") (8090 7090 6090 		5089 4089 = 5 "sund"), gen(hoved_mellem)

	*længde
	local lenght kortere kvu mvu lvu forsker uoplyst
	forval x=1/6{
		gettoken len lenght : lenght
		gen `len' = hoved_over==`x'
	}

	*type
	local type hum samf tek nat sund
	forval x=1/5{
		gettoken typ type : type
		gen `typ' = hoved_mellem==`x'
	}

	collapse (mean) kortere kvu mvu lvu forsker uoplyst hum samf tek nat sund, by(cvr)

	ren * *`Y'
	ren cvrnr`Y' cvr
	
	save udda+ras20`Y', replace
}

use udda+ras2009, replace
ren *09 *9
save udda+ras2009, replace
use udda+ras2008, clear
ren *08 *8
forval x=09/20{
	local X : di %02.0f `x'
	merge 1:1 cvr using udda+ras20`X'
	drop _
}

reshape long kortere kvu mvu lvu forsker uoplyst hum samf tek nat sund, i(cvr) j(year)

save udda+ras2008_2020, replace

********************************
**# Merger + new variables
********************************

* for x
cd "E:\workdata\707565\EA\data"

use udda+ras2008_2020, clear
merge 1:1 cvr year using firm2008_2020x, gen(m1)
merge 1:1 cvr year using fui2011_2020, gen(m2)

sort cvr year

*size
gen micro = 1 if ansatte!=. | oms!=. | aare!=.
gen small=0
gen medium=0
gen large=0

replace micro = 0 if ansatte>9 | oms>15000000 | aare > 15000000
replace small = 1 if ansatte>9 | oms>15000000 | aare > 15000000

replace small =  0 if ansatte>49 | oms>75000000 | aare > 75000000
replace medium = 1 if ansatte>49 | oms>75000000 | aare > 75000000

replace medium = 0 if ansatte>249 | oms>375000000 | aare > 322500000
replace large =  1 if ansatte>249 | oms>375000000 | aare > 322500000

save firm_fui2008_2019x, replace 

***********************************************

* for dst
cd "E:\workdata\707565\EA\data"

use udda+ras2008_2020, clear
merge 1:1 cvr year using firm2008_2020, gen(m1)
merge 1:1 cvr year using fui2011_2020, gen(m2)

sort cvr year


***new variables***

*any innovation
gen inn = .
replace inn = 1 if inpspd==1 | inpdgd==1 | inpdsv==1
replace inn = 0 if inpspd==0 & inpdgd==0 & inpdsv==0

*brancher
destring branche, replace
local bra laskfi inraafo byganl hantra infkom finfor ejeudl erhv ofadsu kufrse uopl
forval x = 1/11{
	gettoken bran bra : bra
	recode branche (`x'=1) (nonmissing=0), gen(`bran')
}

*size
gen micro = 1 if ansatte!=. | oms!=. | aare!=.
gen small=0
gen medium=0
gen large=0

replace micro = 0 if ansatte>9 | oms>15000000 | aare > 15000000
replace small = 1 if ansatte>9 | oms>15000000 | aare > 15000000

replace small =  0 if ansatte>49 | oms>75000000 | aare > 75000000
replace medium = 1 if ansatte>49 | oms>75000000 | aare > 75000000

replace medium = 0 if ansatte>249 | oms>375000000 | aare > 322500000
replace large =  1 if ansatte>249 | oms>375000000 | aare > 322500000

gen size=0
replace size=1 if micro==1
replace size=2 if small==1
replace size=3 if medium==1
replace size=4 if large==1
*kontrol: egen temp=rowtotal(micro small medium large)

*fou 
gen rat_forsk_aarsv = a_forsk/aarsv
gen rat_fouper_aarsv = a_total/aarsv

*alle firmaer 
preserve
drop inpspd inpdgd inpdsv rrdin u_total inn rat_forsk_aarsv rat_fouper_aarsv a_forsk a_total kgts eco* m1 m2
save data_-fui, replace
restore

*komplet data
gen temp=kgts!=.
bysort cvr (year): gen temp2=sum(temp)
drop if temp2==0
drop temp2 temp m1 m2

*all years
bys cvr: egen gtscnt=total(kgts)
egen tag = tag(cvr)
tab gtscnt if tag & gtscnt>0
*in given year

*gts-treat
gen gts=0
replace gts=1 if kgts==1
replace gts=1 if gts[_n-1] == 1 & cvr[_n-1]==cvr
replace gts=. if kgts==.

gen temp=0
replace temp=year if gts==1
forval x=1/10{
	replace temp = temp[_n+1] if temp==0 & cvr==cvr[_n+1]
}
by cvr: egen group=min(temp)
drop temp

*fui observations
recode kgts (nonmissing=1), gen(fui)
bys cvr: egen fuicnt=total(fui)

*fui-pairs
gen fuipair=0
replace fuipair = 1 if fui!=. & (fui[_n-1]!=. & cvr==cvr[_n-1] | fui[_n+1]!=. & cvr==cvr[_n+1])

*fou-udgifter ratio
gen fouand = (u_total*1000) / oms

save data, replace

***missing firm2018***
keep if bra==. & inpspd!=.
save "E:\workdata\707565\EA\mangler_firm2018.dta", replace

**********************************************************
**#	Descriptive statistics
**********************************************************
cd "E:\workdata\707565\EA\data"
use data, clear

***Deskriptiv***

*gts
global vars "kortere kvu mvu lvu forsker uoplyst	hum samf tek nat sund 		aarsv ansatte eksp import oms aare vtv prod_oms prod_res prod_vtv micro small medium large 	inpspd inpdgd inpdsv rrdin u_total inn rat_forsk_aarsv rat_fouper_aarsv a_forsk a_total 	inraafo  hantra infkom erhv" 

local sheets buy nobuy nobuy-inno nobuy-noinno 
	foreach type in "kgts!=." "kgts==1" "kgts==0" "kgts==0 & inn==1" "kgts==0 & inn==0"{
	gettoken sheet sheets : sheets
	putexcel set "E:\workdata\707565\EA\ud\gts.xlsx", sheet(`sheet') modify
	local table = 1
	foreach x in mean n p50 sd{
		putexcel a`table'="`x'"
		tabstat	$vars [aweight=cw] if `type', by(year) statistics(`x') save
		forval y=1/8{
			local row = `table'+`y'
			putexcel a`row'=`r(name`y')'
			putexcel b`row'=matrix(r(Stat`y'))
		}
		local vars1 $vars
		forval y=1/46{
			local z = `y'+1
			gettoken vars2 vars1 : vars1
			mata: st_local("col", numtobase26(`z'))
			putexcel `col'`table' = "`vars2'"
		}
		local table = `table' + 11

	}
}

*alle
use data_-fui, replace
global vars "kortere kvu mvu lvu forsker uoplyst	hum samf tek nat sund 		aarsv ansatte eksp import oms aare vtv prod_oms prod_res prod_vtv micro small medium large 	inraafo  hantra infkom erhv"

putexcel set "E:\workdata\707565\EA\ud\gts.xlsx", sheet(alle_firm) modify

local table = 1
foreach x in mean n p50 sd{
	putexcel a`table'="`x'"
	tabstat	$vars, by(year) statistics(`x') save 
	forval y=1/8{
		local row = `table'+`y'
		putexcel a`row'=`r(name`y')'
		putexcel b`row'=matrix(r(Stat`y'))
	}
	
	local vars1 $vars
	forval y=1/46{
		local z = `y'+1
		gettoken vars2 vars1 : vars1
		mata: st_local("col", numtobase26(`z'))
		putexcel `col'`table' = "`vars2'"
	}
	
	local table = `table' + 11
}

**********In TABLE****************
cd "E:\workdata\707565\EA\data"
use data, clear 

local sheets fui buy nobuy nobuy-inno nobuy-noinno 
foreach type in "kgts!=." "kgts==1" "kgts==0" "kgts==0 & inn==1" "kgts==0 & inn==0"{
	gettoken sheet sheets : sheets
	putexcel set "E:\workdata\707565\EA\ud\gts_table.xlsx", sheet(`sheet') modify
	local table = 1
	foreach x in mean n{
		putexcel a`table'="`x'"
		table year [aweight=cw] if `type', stat(`x' $vars) nformat(%9.4g)
		putexcel b`table' = collect
		local table = `table' + 12
	}
}

use data_-fui, replace
putexcel set "E:\workdata\707565\EA\ud\gts_table.xlsx", sheet(alle) modify
local table = 1 
table year, stat(mean $vars) nformat(%9.4g)
putexcel b1 = collect
table year, stat(n $vars)
putexcel b12 = collect


**#*****Antal kunder ikke-vægtet + vægtet********
use data, clear

putexcel set "E:\workdata\707565\EA\ud\gts0607.xlsx", modify

table year kgts
table year kgts [aw=cw]
putexcel b1 = 0
putexcel c1 = 1
local num 17545 16528 18022 16855 17201 12955 9093 18410
forval x = 2/10 {
	gettoken in num : num
	putexcel b`x' = "`in'"
} 
local nums 117 149 127 160 160 189 198 209
forval x = 2/10 {
	gettoken in nums : nums
	putexcel c`x' = "`in'"
} 

***Økonomiske tal vægtet****
replace oms = round(oms/1000000)
replace prod_vtv = round(prod_vtv/1000)

*oms
table year [aw=cw] if kgts== 0, stat(median oms)
putexcel a10 = collect
table year [aw=cw] if kgts== 1, stat(median oms)
putexcel c10 = collect
putexcel f10 = "oms i mil"

*vtv
table year [aw=cw] if kgts== 0, stat(median prod_vtv)
putexcel a21 = collect
table year [aw=cw] if kgts== 1, stat(median prod_vtv)
putexcel c21 = collect
putexcel f21= "vtv i tus"

*eks
table year if kgts== 0 [aw=cw], stat(mean eksp_oms)
putexcel a33 = collect
table year if kgts== 1 [aw=cw], stat(mean eksp_oms)
putexcel c33 = collect
putexcel f33 = "eksp/oms"

*fou-udgifter - gnms < 95. percentil
putexcel f45 = "fou_total"
forval x = 11/20{
	if `x'==17 | `x'==19 continue
	local k = `x' + 34
	preserve 
	keep if year == `x'
	putexcel a`k' = "`x'"
	sum u_total [aw=cw] if kgts==0, detail
	sum u_total [aw=cw] if u_total<r(p95) & kgts==0
	local j = round(`r(mean)')
	putexcel b`k' = `j'
	sum u_total [aw=cw] if kgts==1, detail
	sum u_total [aw=cw] if u_total<r(p95) & kgts==1
	local j = round(`r(mean)')
	putexcel c`k' = `j'
	restore
}

*fou-udgifter - alm gnms. 
table year [aw=cw] if kgts== 0, stat(mean u_total)
putexcel a56 = collect
table year [aw=cw] if kgts== 1, stat(mean u_total)
putexcel c56 = collect



***********************
**# Medianer
***********************

cd "E:\workdata\707565\EA\data"
*størrelse i 18
use fui2011_2020, clear
merge 1:1 cvr year using firm2008_2019x
keep if _==3

gen micro = 1 if ansatte!=. | oms!=. 
gen small=0
gen medium=0
gen large=0

replace micro = 0 if ansatte>9 | oms>15000000 
replace small = 1 if ansatte>9 | oms>15000000 

replace small =  0 if ansatte>49 | oms>75000000 
replace medium = 1 if ansatte>49 | oms>75000000 

replace medium = 0 if ansatte>249 | oms>375000000 
replace large =  1 if ansatte>249 | oms>375000000 

gen size=0
replace size=1 if micro==1
replace size=2 if small==1
replace size=3 if medium==1
replace size=4 if large==1

collapse (mean) small aarsv [aw=cw], by(year kgts)
export excel using "E:\workdata\707565\EA\results\gts.xlsx", sheetreplace sheet(size18) firstrow(variables)

*omsætning, prod_res, u_toal, eksp
use fui2011_2020, clear
merge 1:1 cvr year using firm2008_2019x

keep if _==3 & kgts!=.
replace oms = round(oms/1000)
collapse (median) oms prod_vtv eksp_oms u_total, by(year kgts)
sort k y
export excel using "E:\workdata\707565\EA\results\gts.xlsx", sheet(econ_med) sheetreplace firstrow(variables)

*omsætning, prod_res, u_toal, eksp for 2020
use fui2011_2020, clear
merge 1:1 cvr year using firm2008_2020

keep if _==3 & kgts!=.
replace oms = round(oms/1000)
collapse (median) oms prod_vtv eksp_oms u_total, by(year kgts)
sort k y
keep if y ==20
export excel using "E:\workdata\707565\EA\results\gts.xlsx", sheet(econ20_med) sheetreplace firstrow(variables)

*omsætning, prod_res, u_toal, eksp
use fui2011_2020, clear
merge 1:1 cvr year using firm2008_2019x

keep if _==3 & kgts!=.
replace oms = round(oms/1000)
collapse (mean) oms prod_vtv eksp_oms u_total, by(year kgts)
sort k y
export excel using "E:\workdata\707565\EA\results\gts.xlsx", sheet(econ_mean) sheetreplace firstrow(variables)

*omsætning, prod_res, u_toal, eksp for 2020
use fui2011_2020, clear
merge 1:1 cvr year using firm2008_2020

keep if _==3 & kgts!=.
replace oms = round(oms/1000)
collapse (mean) oms prod_vtv eksp_oms u_total, by(year kgts)
sort k y
keep if y ==20
export excel using "E:\workdata\707565\EA\results\gts.xlsx", sheet(econ20_mean) sheetreplace firstrow(variables)



******************************
**# Assumptions in economic models
******************************
cd "E:\workdata\707565\EA\data"
use data, clear

*fairness af missing == 0
preserve
recode gtscnt (8=7) (6=5)
recode fuicnt (5=6) (3=4)
tab fuicnt gtscnt if tag, matcell(A)
putexcel set "E:\workdata\707565\EA\ud\gts2.xlsx", sheet(fui_gts) modify
putexcel a2 = "fuicnt"
putexcel b1 = "gtscnt"
putexcel a4 = "3-4"
putexcel a5 = "5-6"
putexcel g1 = "5-6"
putexcel h1 = "8-7"
putexcel b2 = matrix(A)

tab fuicnt if tag, matcell(D)
tab gtscnt if tag, matcell(E)
putexcel j1 = "fuicnt"
putexcel j2 = matrix(D)
putexcel k1 = "gtscnt"
putexcel k2 = matrix(E)
restore

tab year kgts, matcell(F)
putexcel b9=matrix(F)

*obs ved fuipair
count
putexcel k11=`r(N)'
putexcel j11="firma-aar"
count if tag 
putexcel k12=`r(N)'
putexcel j12="firma"
count if fuipair==1 
putexcel j14=`r(N)'
putexcel k14="firma-aar"
count if fuipair==1 & tag
putexcel j15=`r(N)'
putexcel k15="firma"

preserve
keep if fuipair==1
sort cvr
by cvr: gen temp= _n
by cvr: egen temp2=max(temp)
egen tag2=tag(cvr)
tab temp2 if tag2
tabstat temp2


************************************
**#Economic effects
************************************

****fuipair******

*only economic variables

cd "E:\workdata\707565\EA\results"
use "E:\workdata\707565\EA\data\data.dta", clear
keep if fuipair==1
destring cvr, replace
sysdir set PLUS "E:\workdata\707565\DO-filer"

global cov i.size i.branche kortere tek rrdin
global yvars prod_oms prod_vtv prod_res oms vtv aare

***naiv 
foreach y in $yvars{
	csdid `y', ivar(cvr) time(year) gvar(group) method(dripw) notyet 
	estat simple, estore(`y'_naiv_simple)
}

*simple
cd "E:\workdata\707565\EA\results"
coefplot /// PRODUKTIVITET
	(prod_oms_naiv_simple, aseq(Omsætning) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(prod_vtv_naiv_simple, aseq(Værditilvækst) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(prod_res_naiv_simple, aseq(Resultat) msymbol(O) mcolor(black) ciopts(lcol(black))), bylabel(Pr. årsværk) || /// VÆKST
	(oms_naiv_simple, aseq(Omsætning) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(vtv_naiv_simple, aseq(Værditilvækst) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(aare_naiv_simple, aseq(Resultat) msymbol(O) mcolor(black) ciopts(lcol(black))), bylabel(Vækst) || ///
	, bgcol(white) graphregion(col(white)) swapnames xline(0) ylab(, labsize(small)) xlab(, labsize(small)) byopts(xrescale imargin(*5)) legend(off) nokey subtitle(,size(small))

graph export "econ_v2.png", as(png) name("Graph") replace

xtset cvr year
foreach y in $yvars{
	xtreg `y' kgts, fe
	estimates store `y'_naiv_xtreg
}

*xtreg
cd "E:\workdata\707565\EA\results"
coefplot /// PRODUKTIVITET
	(prod_oms_naiv_xtreg, aseq(Omsætning) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(prod_vtv_naiv_xtreg, aseq(Værditilvækst) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(prod_res_naiv_xtreg, aseq(Resultat) msymbol(O) mcolor(black) ciopts(lcol(black))), bylabel(Pr. årsværk) || /// VÆKST
	(oms_naiv_xtreg, aseq(Omsætning) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(vtv_naiv_xtreg, aseq(Værditilvækst) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(aare_naiv_xtreg, aseq(Resultat) msymbol(O) mcolor(black) ciopts(lcol(black))), bylabel(Vækst) || ///
	, keep (*kgts) bgcol(white) graphregion(col(white)) swapnames xline(0) ylab(, labsize(small)) xlab(, labsize(small)) byopts(xrescale imargin(*5)) legend(off) nokey subtitle(,size(small))

*event
forval x=1/4{
	coefplot prod_oms_naiv_event prod_vtv_naiv_event prod_res_naiv_event, xline(0) keep(*`x')
	graph export "pair_naiv_event`x'_prod.png", as(png) name ("Graph") replace
	coefplot oms_naiv_event vtv_naiv_event aare_naiv_event, xline(0) keep(*`x')
	graph export "pair_naiv_event`x'_eco.png", as(png) name ("Graph") replace
}

***covariates **** excluded bc model is 0
/*
foreach y in prod_oms prod_vtv prod_res oms vtv aare{
	csdid `y' $cov, ivar(cvr) time(year) gvar(group) method(dripw) notyet 
	estat simple, estore(`y'_naiv_simple)
	estat event, window(0 4) estore(`y'_naiv_event)
}

*simple
coefplot prod_oms_naiv_simple prod_vtv_naiv_simple prod_res_naiv_simple, xline(0)
graph export "pair_cov_simple_prod.png", as(png) name ("Graph") replace
coefplot oms_naiv_simple vtv_naiv_simple aare_naiv_simple, xline(0)
graph export "pair_cov_simple_eco.png", as(png) name ("Graph") replace

*event
forval x=1/4{
	coefplot prod_oms_naiv_event prod_vtv_naiv_event prod_res_naiv_event, xline(0) keep(*`x')
	graph export "pair_cov_event`x'_prod.png", as(png) name ("Graph") replace
	coefplot oms_naiv_event vtv_naiv_event aare_naiv_event, xline(0) keep(*`x')
	graph export "pair_cov_event`x'_eco.png", as(png) name ("Graph") replace
}
*/


*****fui==. === gts=0*******

use "E:\workdata\707565\EA\data\data.dta", clear
sort cvr year
replace gts=gts[_n-1] if gts==. & gts[_n-1]!=. & cvr==cvr[_n-1]
destring cvr, replace
global yvars prod_oms prod_vtv prod_res oms vtv aare

***Economic variables***

***naive
foreach y in $yvars{
	csdid `y' , ivar(cvr) time(year) gvar(group) method(dripw) notyet 
	estat simple, estore(`y'_naiv_simple)
}

*event
forval x=1/4{
	coefplot prod_oms_naiv_event prod_vtv_naiv_event prod_res_naiv_event, xline(0) keep(*`x')
	graph export "imput_naiv_event`x'_prod.png", as(png) name("Graph") replace
	coefplot oms_naiv_event vtv_naiv_event aare_naiv_event, xline(0) keep(*`x')
	graph export "imput_naiv_event`x'_eco.png", as(png) name("Graph") replace
}

*excluded because naive is insignificant
/*
***covariates
foreach y in prod_oms prod_vtv prod_res oms vtv aare{
	csdid `y' $cov, ivar(cvr) time(year) gvar(group) method(dripw) notyet 
	estat simple, estore(`y'_cov_simple)
	estat event, window(0 4) estore(`y'_cov_event)
}

*simple
cd "E:\workdata\707565\EA\results"
coefplot prod_oms_simple prod_vtv_simple prod_res_simple, xline(0)
graph export "imput_cov_simple_prod.png", as(png) name("Graph") replace
coefplot oms_simple vtv_simple aare_simple, xline(0)
graph export "imput_cov_simple_eco.png", as(png) name("Graph") replace

*event
forval x=1/4{
	coefplot prod_oms_cov_event prod_vtv_cov_event prod_res_cov_event, xline(0) keep(*`x')
	graph export "imput_cov_event`x'_prod.png", as(png) name("Graph") replace
	coefplot oms_cov_event vtv_cov_event aare_cov_event, xline(0) keep(*`x')
	graph export "imput_cov_event`x'_eco.png", as(png) name("Graph") replace
}
*/

*****y+1*****

/*
use "E:\workdata\707565\EA\data\data.dta", clear
sort cvr year 
foreach y in $yvars {
	gen `y't2 = `y'-`y'[_n+1] if cvr==cvr[_n-1]
	reg `y't2 kgts i.year
	estimates store naiv_`y'
	reg `y't2 kgts oms aarsv kortere tek i.year 
	estimates store cov_`y'
}

coefplot cov_prod_oms cov_prod_vtv cov_prod_res, xline(0) keep(*kgts)
graph export "t+1_naiv_prod.png", as(png) name("Graph") replace
coefplot cov_oms cov_vtv cov_aare, xline(0) keep(*kgts)
graph export "t+1_naiv_eco.png", as(png) name("Graph") replace
coefplot cov_prod_oms cov_prod_vtv cov_prod_res, xline(0) keep(*kgts)
graph export "t+1_cov_prod.png", as(png) name("Graph") replace
coefplot cov_oms cov_vtv cov_aare, xline(0) keep(*kgts)
graph export "t+1_cov_eco.png", as(png) name("Graph") replace
*/

*************************************
**#Innovation effects
*************************************
cd "E:\workdata\707565\EA\results"
use "E:\workdata\707565\EA\data\data.dta", clear
recode eco* (2/3=0) (4=.), prefix(hard_)
recode eco* (2 = 1) (3=0) (4=.), prefix(soft_)
destring cvr, replace
xtset cvr year

*globals
global inn inn inpspd inpdgd inpdsv inno_pcs_comm
global inn2 inpspd inpdgd inpdsv
foreach x in hard soft{
	global eco_`x' `x'_eco_eno `x'_eco_ext `x'_eco_mat `x'_eco_pol `x'_eco_pos `x'_eco_rea `x'_eco_rec `x'_eco_rep `x'_eco_sub `x'_eco_enu
}

***innovation***
foreach y of varlist $inn2{
	xtlogit `y' kgts, fe
	estimates store naiv_`y'
	global naiv_`y'
	xtlogit `y' kgts aars oms kortere tek rrdin i.branche u_total, fe
	estimates store cov_`y'
}

local k = 1
foreach y of varlist $inn2 inno_pcs_comm{
	logit `y' kgts if year==20
	estimates store naiv_`y'_20
	logit `y' i.kgts aars oms kortere tek rrdin i.branche u_total if year==20 
	estimates store cov_`y'_20
	margins kgts, at(branche=4 rrdin=0) atmeans 
	putexcel set "E:\workdata\707565\EA\ud\gts1705.xlsx", sheet(margins) modify
	putexcel a`k' = "`y'"
	putexcel b`k' = matrix(r(b))
	local k = `k'+1
}


local k = 5
***eco***
foreach y of varlist $eco_hard $eco_soft{
	logit `y' kgts
	estimates store naiv_`y'
	global naiv_`y'
	logit `y' i.kgts aars oms kortere tek rrdin i.branche u_total
	estimates store cov_`y'
	margins kgts, at(branche=4 rrdin=0) atmeans 
	putexcel a`k' = "`y'"
	putexcel b`k' = matrix(r(b))
	local k = `k'+1
}

*eco_hard
foreach v of global eco_hard{
	local naiv `naiv' naiv_`v'
	local cov `cov' cov_`v'
}
global eco_hard_naiv `naiv'
global eco_hard_cov `cov'



*til kontrol af at margins er ok
coefplot ///
	(cov_inpdgd_20, aseq(Serviceydelser) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_inn_20, aseq(Varer) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_inpspd_20, aseq(Produktions- eller`=char(13)'`=char(10)' udviklingsmetoder) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	(cov_inno_pcs_comm_20, aseq(Metoder til databehandling `=char(13)'`=char(10)'eller kommunikation) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	, keep(*kgts) swapnames xline(0) xscale(range(-0.1(0.3)1.2)) legend(order(2 "Produktinnovation" 4 "Procesinnovation") off) nooffsets bgcolor(white) graphregion(color(white)) ylab(, labsize(small)) xlab(, labsize(small)) ysc(outergap(-35))

*til rapport
coefplot ///
	(cov_inpdgd, aseq(Serviceydelser) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_inn, aseq(Varer) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_inpspd, aseq(Produktions- eller`=char(13)'`=char(10)' udviklingsmetoder) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	(cov_inno_pcs_comm, aseq(Metoder til databehandling `=char(13)'`=char(10)'eller kommunikation) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	, keep(*kgts) swapnames xline(0) xscale(range(-0.1(0.3)1.2)) legend(order(2 "Produktinnovation" 4 "Procesinnovation") off) nooffsets bgcolor(white) graphregion(color(white)) ylab(, labsize(small)) xlab(, labsize(small)) ysc(outergap(-35))

graph export "inn.png", as(png) name("Graph") replace

*HARD
coefplot /// 
	(cov_hard_eco_rea, aseq(Lettere for slutbruger at genbruge produkter) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_pos, aseq(Reduceret forurening i slutbrugers forbrug) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_pol, aseq(Reduceret forurening i produktion) msymbol(O) mcolor(black) ciopts(lcol(black)))	///
	(cov_hard_eco_enu, aseq(Reduceret CO2-/energiforbrug i slutbrugers forbrug) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_mat, aseq(Reduceret materiale/vandforbrug i produktion) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_rec, aseq(Lettere at genbruge produktionsmaterialer) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_rep, aseq(Øget vedvarende energi i produktion) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_ext, aseq(Øget produktlevetid) msymbol(S) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_eno, aseq(Reduceret CO2-/energiforbrug i produktion) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	(cov_hard_eco_sub, aseq(Reduceret forurenende materialer i produktion) msymbol(O) mcolor(black) ciopts(lcol(black))) ///
	, keep(*kgts) swapnames xline(0) xscale(range(-0.1(0.1)1.2)) legend(off) nooffsets bgcolor(white) graphregion(color(white)) ylab(, labsize(small)) xlab(, labsize(small))

graph export "eco_hard.png", as(png) name("Graph") replace	

***DESC STAT***

putexcel set "E:\workdata\707565\EA\results\gts.xlsx", sheet(desk_stat, replace) modify
local k = 1
foreach x of varlist $inn $eco_soft $eco_hard{
	table year `x'
	putexcel a`k' = collect
	local k = `k' + 13
}
local k = 1
foreach x of varlist $inn $eco_soft $eco_hard{
	table year `x' if kgts==1
	putexcel i`k' = collect
	local k = `k' + 13
}
local k = 1
foreach x of varlist $inn $eco_soft $eco_hard{
	table year `x' if kgts==0
	putexcel n`k' = collect
	local k = `k' + 13
}

table fuicnt
putexcel f1 = collect

*testeren
coefplot ///
	(naiv_hard_eco_mat, aseq(Materiale/vandforbrug) msymbol(Oh) mcolor(blue) ciopts(lcol(blue))) ///
	(cov_hard_eco_mat, aseq(Materiale/vandforbrug) msymbol(O) mcolor(blue) ciopts(lcol(blue))) ///
		, keep(*kgts) swapnames xline(0) xscale(range(-0.2(0.1)1.2)) legend(off) bgcolor(white) 
	

*
end

*****************************************************
**# Order / Sort / Drop
*****************************************************
order cvr year kgts gts fuipair fui fuicnt group gtscnt

sort cvr year

drop group

**replace excel
putexcel set "E:\workdata\707565\EA\ud\gts2.xlsx", sheet(fui_gts) replace
putexcel a1= ""

putexcel set "E:\workdata\707565\EA\ud\gts.xlsx", sheet(buy) replace
putexcel a1= ""

*
tabstat $vars [aweight=cw] if kgts!=., by(year) statistics(mean) save format(%9.4f)

