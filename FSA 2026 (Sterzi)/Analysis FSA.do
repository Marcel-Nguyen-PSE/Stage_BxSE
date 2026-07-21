
//add similarity from Grazia//

/*preserve
import delimited "C:\Users\vsterzi\Dropbox\Ebay_Stefania\Data UPC\Maggio 2026\Grazia\ovh_similaritymay26.csv", clear
gen EP="EP"
egen Patentnumber_target=concat(EP target_patent)
egen Patentnumber=concat(EP publication_number)
keep Patentnumber similarity_score
bysort Patentnumber: egen similarity20=mean(similarity_score)
keep Patentnumber similarity20
duplicates drop
order Patentnumber similarity20
save "C:\Users\vsterzi\Dropbox\Applications\Overleaf\Blurring the Line\Empirics\similarity20.dta", replace
restore
*/


/*keyword business description*/
/*preserve
import excel "C:\Users\vsterzi\Dropbox\Applications\Overleaf\Blurring the Line\Empirics\Type_Chat\Classeur1_keyword_scores_updated20260715.xlsx", sheet("Feuil1") firstrow clear
drop if NACE4digitdescription=="Individual and Family Services"
gen Type=""
replace Type = "UPSTREAM"   if Technologymarketscore > Productmarketscore 
replace Type = "DOWNSTREAM" if Technologymarketscore < Productmarketscore
replace Type = "HYBRID"     if Technologymarketscore >= 1 & Productmarketscore >=1
replace Type = "PAE" if NACE4digitdescription=="NPE"
replace Type = "REVIEW" if Type ==""

gen Type2=""
replace Type2 = "UPSTREAM"   if Technologymarketscorenodedu > Productmarketscorenodedup 
replace Type2 = "DOWNSTREAM" if Technologymarketscorenodedu < Productmarketscorenodedup
replace Type2 = "HYBRID"     if Technologymarketscorenodedu >= 1 & Productmarketscorenodedup >=1
replace Type2 = "PAE" if NACE4digitdescription=="NPE"
replace Type2 = "REVIEW" if Type2 ==""


keep Claimants Type Technologymarketscore Productmarketscore
sort Claimants
save "C:\Users\vsterzi\Dropbox\Applications\Overleaf\FSA 2026\type_revised.dta", replace
restore 
*/

capture cd "C:\Users\vsterzi\Dropbox\Applications\Overleaf\FSA 2026\"
capture cd "C:\Users\vsterzi.PARC\Dropbox\Applications\Overleaf\FSA 2026\"

use "Data4Analysis_June26.dta", clear

preserve

* Keep only the variables of interest
keep Claimants Country_Claimants Defendants Country_Defendants

* Claimants
rename Claimants company
rename Country_Claimants country

tempfile claimants
save `claimants'

restore
preserve

* Defendants
keep Defendants Country_Defendants
rename Defendants company
rename Country_Defendants country

* Append claimants
append using `claimants'

* Remove empty observations
drop if missing(company)

* Remove duplicates (same company-country combination)
duplicates drop company country, force

* Optional: sort
sort company

* Export to CSV
export delimited company country using "company_list.csv", replace

restore





sort Patentnumber
merge Patentnumber using "C:\Users\vsterzi\Dropbox\Applications\Overleaf\Blurring the Line\Empirics\similarity20.dta"
/*pay attention here*/
egen mean_similarity=mean(similarity20)
replace similarity20=mean_similarity if _merge==1


drop _merge
sort Claimants
merge Claimants using "type_revised.dta"
keep if _merge==3
tab Type 
drop _merge
duplicates drop
replace Type="UNKNOWN" if Type =="REVIEW"

************************************
save "use Data_SEP_FSA.dta", replace
************************************




gen PAE=1 if Type=="PAE"
replace PAE=0 if PAE==.
gen age = year-filing
replace  family_size=20 if family_size>20
encode Courtdivision, gen(Courtdivision_n)


preserve
keep Claimants Type
duplicates drop
tab Type
restore

drop ID



*drop if Outcome=="Not available"
*keep if ICT==1

gen Age=year-filing
drop if Age==.
replace Age=20 if Age>20


egen patent_case = group(Patentnumber CaseNumber)

bysort Patentnumber patent_case: gen tag = (_n==1)

bysort Patentnumber: egen n_cases_patent = total(tag)

gen RepeatedPatent = n_cases_patent > 1

bysort Patentnumber: gen n_assertions = _N

gen transacted =1 if OriginalAssignee!= Claimants
replace transacted=0 if transacted==.

gen SEP2="Non-SEP" if SEP==0
replace SEP2="SEP-acquired" if SEP==1 & transacted==1
replace SEP2="SEP-filed" if SEP==1 & transacted==0

gen UPSTREAM=1 if Type=="UPSTREAM"
replace UPSTREAM=0 if UPSTREAM==.

gen HYBRID=1 if Type=="HYBRID"
replace HYBRID=0 if HYBRID==.

gen DOWNSTREAM=1 if Type=="DOWNSTREAM"
replace DOWNSTREAM=0 if DOWNSTREAM==.

tabstat PAE UPSTREAM HYBRID DOWNSTREAM age similarity20 family_size n_cases_patent n_assertions patent_scope Court_Germany transacted, by(SEP)
tabstat PAE UPSTREAM HYBRID DOWNSTREAM age similarity20 family_size n_cases_patent n_assertions patent_scope Court_Germany transacted if ICT==1, by(SEP)

tabstat PAE UPSTREAM HYBRID DOWNSTREAM age similarity20 family_size n_cases_patent n_assertions patent_scope Court_Germany, by(SEP2)

tabstat PAE UPSTREAM HYBRID DOWNSTREAM age similarity20 family_size n_cases_patent n_assertions patent_scope Court_Germany if ICT==1, by(SEP2)





preserve
keep Claimants Type
duplicates drop
tab Type
restore


preserve
keep  Type Claimants CaseNumber
duplicates drop
* Count cases by claimant and category
contract Type Claimants
rename _freq n_cases

* Order categories along the value chain
gen type_order = .
replace type_order = 1 if Type=="PAE"
replace type_order = 2 if Type=="UPSTREAM"
replace type_order = 3 if Type=="HYBRID"
replace type_order = 4 if Type=="DOWNSTREAM"

* Rank claimants within each type (ties broken alphabetically)
gsort type_order -n_cases Claimants
by type_order: gen rank = _n

* Keep top 5 per category
keep if rank <= 5

sort type_order rank

* Build LaTeX rows manually, with a \midrule-separated header per Type
* and \multirow-style label only on the first row of each block
gen str200 latex_row = ""
gen str50 type_label = ""
by type_order: replace type_label = Type if _n==1

forvalues i = 1/`=_N' {
    local t   = type_label[`i']
    local r   = rank[`i']
    local c   = Claimants[`i']
    local n   = n_cases[`i']
    replace latex_row = "`t' & `r' & `c' & `n' \\" in `i'
}

* Export to LaTeX with booktabs and \addlinespace between type blocks
file open tab using "top5_claimants_by_type.tex", write replace
file write tab "\begin{tabular}{@{}llp{6cm}r@{}}" _n
file write tab "\toprule" _n
file write tab "\textbf{Type} & \textbf{Rank} & \textbf{Claimant} & \textbf{Cases} \\" _n
file write tab "\midrule" _n

local N = _N
forvalues i = 1/`N' {
    local row = latex_row[`i']
    if `i' > 1 & rank[`i'] == 1 {
        file write tab "\addlinespace" _n
    }
    file write tab "`row'" _n
}

file write tab "\bottomrule" _n
file write tab "\end{tabular}" _n
file close tab

restore









preserve
keep CaseNumber Claimants Type
duplicates drop
* Distinct plaintiffs by type
egen tag_plaintiff = tag(Type Claimants)
* Distinct cases by type
egen tag_case = tag(Type CaseNumber)
collapse ///
    (sum) n_plaintiffs = tag_plaintiff ///
    (sum) n_cases = tag_case, ///
    by(Type)
list, clean
restore


* Standardize (z-scores)
egen z_Age        = std(Age)
egen z_Value      = std(Patent_Value)
egen z_family_size = std(family_size)
egen z_similarity20 = std(similarity20)
egen z_Assertions = std(n_assertions)






summ z_Age if PAE==1
scalar mu_ag = r(mean)

summ z_Value if PAE==1
scalar mu_v = r(mean)

summ z_family_size if PAE==1
scalar mu_f = r(mean)

summ z_Assertions if PAE==1
scalar mu_ass = r(mean)



*summ Acquired if NPE==1
*scalar mu_ac = r(mean)

summ Court_Germany if PAE==1
scalar mu_g = r(mean)


summ ICT if PAE==1


/*Mahalanobis distance*/
local vars Age family_size Court_Germany similarity20 n_assertions patent_scope transacted

mata:
    X = st_data(., tokens("`vars'"))
    pae = st_data(., "PAE")

    X_pae = select(X, pae:==1)
    mu = mean(X_pae)
    S = variance(X_pae)
    S_inv = invsym(S)

    diff = X :- mu
    dist = sqrt(rowsum((diff * S_inv) :* diff))

    st_store(., st_addvar("double", "dist_to_PAE"), dist)
end


*gen dist_to_PAE_euc = sqrt( (z_Age  - mu_ag)^2 + (z_family_size  - mu_f)^2 + (Court_Germany  - mu_g)^2 + (ICT  - mu_ict)^2 + (z_similarity20  - mu_sim)^2  + (z_Assertions  - mu_ass)^2)

********************************************************
/*Table centroid */
********************************************************


label variable Age "Patent age"
label variable family_size "Patent family size"
label variable similarity20 "Technological similarity"
label variable ICT "ICT technology"
label variable n_assertions "UPC assertion frequency"
label variable Court_Germany "German courts"
label variable patent_scope "Patent scope"
label variable transacted "Acquired patent"

gen pae = Type=="PAE"
eststo clear

estpost ttest Age family_size similarity20 Court_Germany n_assertions patent_scope transacted, by(pae)

esttab using centroid_table.tex, ///
    replace ///
    cells("mu_1(fmt(2)) mu_2(fmt(2)) b(star fmt(2))") ///
    collabels("Non-PAE" "PAE" "Difference") ///
    label ///
    booktabs ///
    nonumber
	
************************************************************************
/*FIGURE 1*/
************************************************************************

preserve
summ dist_to_PAE if PAE==1
summ dist_to_PAE if PAE==0
ttest dist_to_PAE, by(PAE)

twoway ///
    (kdensity dist_to_PAE if PAE == 1, ///
        recast(area) ///
        fcolor(navy%25) ///
        lcolor(navy) ///
        lwidth(medium) ///
        lpattern(solid)) ///
    (kdensity dist_to_PAE if PAE == 0, ///
        recast(area) ///
        fcolor(maroon%25) ///
        lcolor(maroon) ///
        lwidth(medium) ///
        lpattern(dash)), ///
    ///
    xscale(range(1 15)) ///
    xlabel(1(2)15, labsize(small) nogrid) ///
    ylabel(, labsize(small) angle(horizontal) nogrid format(%3.2f)) ///
    ///
    xtitle("Distance to the representative PAE behavioural profile", ///
        size(small) margin(medsmall)) ///
    ytitle("Kernel density", size(small) margin(medsmall)) ///
    ///
    title("", size(medium) color(black)) ///
    ///
    legend(order(1 "Patent Assertion Entities" 2 "Other Plaintiffs") ///
           cols(1) ///
           position(1) ring(0) ///
           size(small) ///
           symxsize(6) ///
           region(lcolor(none) fcolor(none)) ///
           bmargin(zero)) ///
    ///
    graphregion(color(white) margin(medium)) ///
    plotregion(color(white) lcolor(black) lwidth(thin)) ///
    ///
    xsize(6) ysize(4)
	   
graph export "C:\Users\vsterzi\Dropbox\Applications\Overleaf\Blurring the Line\Empirics\Kdensity.png", as(png) name("Graph") replace
restore  


ttest dist_to_PAE, by(PAE)

gen similarity = -dist_to_PAE

/*
roctab PAE similarity, graph ///
    title("") ///
    xtitle("False Positive Rate") ///
    ytitle("True Positive Rate") ///
    legend(off)
graph export "roc_curve.png", replace width(2000)	

roctab PAE similarity, graph ///
*/

preserve
logit PAE similarity

lroc, ///
    rlopts(lcolor(gs10) lpattern(dash)) ///
    title("") ///
    xtitle("False Positive Rate") ///
    ytitle("True Positive Rate")

graph export "roc_curve.png", replace width(2000)
restore

	
************************************************************************
/*FIGURE 2: Distance to the PAE Behavioural Centroid by Firm Type*/
************************************************************************
preserve
gen type_order = .
replace type_order = 1 if Type=="PAE"
replace type_order = 2 if Type=="UPSTREAM"
replace type_order = 3 if Type=="HYBRID"
replace type_order = 4 if Type=="DOWNSTREAM"

label define type_order ///
    1 "PAE" ///
    2 "UPSTREAM" ///
    3 "HYBRID" ///
    4 "DOWNSTREAM", replace
label values type_order type_order

set scheme s1color

graph box dist_to_PAE, ///
    over(type_order, label(labsize(small))) ///
    marker(1, msymbol(O) mcolor("29 71 119") msize(small)) ///
    title("", ///
        size(medium) color(black)) ///
    ytitle("Distance to PAE Behavioural Centroid", size(small)) ///
    ylabel(, labsize(small) angle(horizontal) grid glcolor(gs14) glwidth(thin)) ///
    box(1, color("29 71 119%30") lcolor("29 71 119")) ///
    box(2, color("29 71 119%30") lcolor("29 71 119")) ///
    box(3, color("29 71 119%30") lcolor("29 71 119")) ///
    box(4, color("29 71 119%30") lcolor("29 71 119")) ///
    plotregion(margin(zero) lcolor(none)) ///
    graphregion(color(white) margin(medium)) ///
    note("", size(vsmall) color(gs8)) ///
    name(Graph, replace)

graph export "C:\Users\vsterzi\Dropbox\Applications\Overleaf\Blurring the Line\Empirics\box.png", ///
    as(png) width(2400) height(1600) name("Graph") replace

restore



************************************************************************
/* Table baseline */
************************************************************************

capture drop Type_n
encode Type, gen(Type_n)

eststo clear

reg dist_to_PAE ib3.Type_n, vce(cluster Claimants)
eststo m1
estadd local Model "OLS"
estadd local FilingFE "No"

reg dist_to_PAE ib3.Type_n i.filing, vce(cluster Claimants)
eststo m2
estadd local Model "OLS"
estadd local FilingFE "Yes"

reg dist_to_PAE ib3.Type_n i.filing zPTI, vce(cluster Claimants)
eststo m3
estadd local Model "OLS"
estadd local FilingFE "Yes"

mixed dist_to_PAE ib3.Type_n || Claimants:, vce(cluster Claimants)
eststo m4
estadd local Model "Mixed"
estadd local FilingFE "No"

mixed dist_to_PAE ib3.Type_n i.filing || Claimants:, vce(cluster Claimants)
eststo m5
estadd local Model "Mixed"
estadd local FilingFE "Yes"

mixed dist_to_PAE ib3.Type_n i.filing zPTI || Claimants:, vce(cluster Claimants)
eststo m6
estadd local Model "Mixed"
estadd local FilingFE "Yes"

esttab m1 m2 m3 m4 m5 m6 using distance_types.tex, ///
    replace ///
    keep(1.Type_n 2.Type_n 4.Type_n zPTI) ///
    coeflabels( ///
        1.Type_n "Downstream" ///
        2.Type_n "Hybrid" ///
        4.Type_n "Upstream" ///
        zPTI "Patent transaction intensity") ///
    collabels(none) ///
    eqlabels(none) ///
    nomtitles ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(Model FilingFE N r2 ll, ///
          fmt(%s %s %9.0f %9.3f %9.3f) ///
          labels("Model" "Filing-year FE" "Observations" "R-squared" "Log pseudolikelihood")) ///
    booktabs ///
    label ///
    nonotes
	
	
/*T-test:*/
mixed dist_to_PAE ib3.Type_n i.filing zPTI || Claimants:, vce(cluster Claimants)
test 1.Type_n = 2.Type_n
test 1.Type_n = 3.Type_n
test 1.Type_n = 4.Type_n
test 2.Type_n = 3.Type_n
test 2.Type_n = 4.Type_n
test 3.Type_n = 4.Type_n
	
************************************************************************
/* Table Summary */
************************************************************************

* Estimation sample
reg dist_to_PAE ib3.Type_n i.filing zPTI, vce(cluster Claimants)

capture drop sample_reg
gen sample_reg = e(sample)

* Type dummies
capture drop typee*
tab Type_n if sample_reg, gen(typee)

label variable dist_to_PAE "Distance to PAE centroid"
label variable zPTI "Technology-market intensity"
label variable filing "Filing year"
label variable typee1 "Downstream"
label variable typee2 "Hybrid"
label variable typee3 "PAE"
label variable typee4 "Upstream"

* Summary stats
eststo clear

estpost summarize ///
    dist_to_PAE zPTI filing ///
    typee1 typee2 typee3 typee4 ///
    if sample_reg

esttab . using "summary_stats.tex", ///
    cells("count(fmt(0)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") ///
    label ///
    noobs ///
    nonumber ///
    nomtitle ///
    replace ///
    booktabs
	
************************************************************************
/*Interaction Table: Type x Transaction*/
************************************************************************


*------------------------------------------------------------
* Estimate interaction model
*------------------------------------------------------------
mixed dist_to_PAE ib3.Type_n##c.zPTI i.filing ///
    || Claimants:, vce(cluster Claimants)

* Store coefficients
matrix b = e(b)

local b_down = _b[1.Type_n]
local b_hyb  = _b[2.Type_n]
local b_up   = _b[4.Type_n]

local s_down = _b[1.Type_n#c.zPTI]
local s_hyb  = _b[2.Type_n#c.zPTI]
local s_up   = _b[4.Type_n#c.zPTI]

*------------------------------------------------------------
* Plot predicted differences relative to PAE
* Each line is: Type coefficient + Type#zPTI coefficient * zPTI
*------------------------------------------------------------
twoway ///
    (function y = `b_down' + `s_down'*x, range(-1.5 1.5) ///
        lcolor(maroon) lwidth(medthick)) ///
    (function y = `b_hyb' + `s_hyb'*x, range(-1.5 1.5) ///
        lcolor(orange) lwidth(medthick)) ///
    (function y = `b_up' + `s_up'*x, range(-1.5 1.5) ///
        lcolor(forest_green) lwidth(medthick)) ///
    , ///
    xtitle("Technology-market intensity (z-score)") ///
    ytitle("Predicted difference from PAE distance") ///
    xlabel(-1.5 "-1.5" -0.5 "-0.5" 0.5 "0.5" 1.5 "1.5", labsize(small)) ///
    ylabel(0(0.5)3.5, labsize(small)) ///
    legend(order(1 "Downstream - PAE" ///
                 2 "Hybrid - PAE" ///
                 3 "Upstream - PAE") ///
           rows(1) position(6) size(small) region(lcolor(none))) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    note("", ///
     size(vsmall))

*graph export "margins_TMI_distance.png", replace width(2400)


	
mixed dist_to_PAE ib3.Type_n##c.zPTI i.filing ///
    || Claimants:, vce(cluster Claimants)

margins Type_n, at(zPTI=(-1.5(1)1.5))
marginsplot
graph export "margins_TMI_distance.png", replace width(2400)



************************************************************************
/* Table: SEP status and technology-market intensity */
************************************************************************

capture drop Type_n
encode Type, gen(Type_n)

eststo clear

*-----------------------------------------------------------------------
* (1) OLS: additive SEP specification
*-----------------------------------------------------------------------

reg dist_to_PAE ib3.Type_n i.filing c.zPTI i.SEP, ///
    vce(cluster Claimants)

eststo m1
estadd local Model "OLS"
estadd local FilingFE "Yes"
estadd local SEPInteraction "No"

*-----------------------------------------------------------------------
* (2) OLS: SEP × technology-market intensity
*-----------------------------------------------------------------------
reg dist_to_PAE ib3.Type_n i.filing c.zPTI##i.SEP, ///
    vce(cluster Claimants)

eststo m2
estadd local Model "OLS"
estadd local FilingFE "Yes"
estadd local SEPInteraction "Yes"

*-----------------------------------------------------------------------
* (3) Mixed model: additive SEP specification
*-----------------------------------------------------------------------

mixed dist_to_PAE ib3.Type_n i.filing c.zPTI i.SEP ///
    || Claimants:, vce(cluster Claimants)

eststo m3
estadd local Model "Mixed"
estadd local FilingFE "Yes"
estadd local SEPInteraction "No"

*-----------------------------------------------------------------------
* (4) Mixed model: SEP × technology-market intensity
*-----------------------------------------------------------------------

mixed dist_to_PAE ib3.Type_n i.filing c.zPTI##i.SEP ///
    || Claimants:, vce(cluster Claimants)

eststo m4
estadd local Model "Mixed"
estadd local FilingFE "Yes"
estadd local SEPInteraction "Yes"

*-----------------------------------------------------------------------
* Export LaTeX table
*-----------------------------------------------------------------------

esttab m1 m2 m3 m4 using distance_SEP.tex, ///
    replace ///
    keep(1.Type_n 2.Type_n 4.Type_n ///
         zPTI 1.SEP 1.SEP#c.zPTI) ///
    order(1.Type_n 2.Type_n 4.Type_n ///
          zPTI 1.SEP 1.SEP#c.zPTI) ///
    coeflabels( ///
        1.Type_n       "Downstream" ///
        2.Type_n       "Hybrid" ///
        4.Type_n       "Upstream" ///
        zPTI           "Patent transaction intensity" ///
        1.SEP          "SEP" ///
        1.SEP#c.zPTI   "SEP $\times$ patent transaction intensity") ///
    collabels(none) ///
    eqlabels(none) ///
    nomtitles ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(Model FilingFE SEPInteraction N r2 ll, ///
          fmt(%s %s %s %9.0f %9.3f %9.3f) ///
          labels("Model" ///
                 "Filing-year FE" ///
                 "SEP $\times$ PTI" ///
                 "Observations" ///
                 "R-squared" ///
                 "Log pseudolikelihood")) ///
    booktabs ///
    label ///
    nonotes

*-----------------------------------------------------------------------
* Marginal effect of zPTI by SEP status
*-----------------------------------------------------------------------

reg dist_to_PAE ib3.Type_n i.filing c.zPTI##i.SEP, ///
    vce(cluster Claimants)
margins SEP, dydx(zPTI)

mixed dist_to_PAE ib3.Type_n i.filing c.zPTI##i.SEP ///
    || Claimants:, vce(cluster Claimants)
margins SEP, dydx(zPTI)

************************************************************************
/*FIGURE Appendix: Distance to the PAE Behavioural Centroid by Firm Type (1) */
************************************************************************
	
preserve
keep if PAE==0

collapse ///
    (median) med_dist=dist_to_PAE ///
    (mean) mean_dist=dist_to_PAE ///
    (count) n_cases=dist_to_PAE, ///
    by(Claimants Type)

keep if n_cases >= 5

gsort mean_dist
gen rank_close = _n
gsort -mean_dist
gen rank_far = _n

keep if rank_close <= 10 | rank_far <= 10

gen label = Claimants + " (" + Type + ")"

set scheme s1color   // or "plotplain" / "white_tableau" if installed

graph dot med_dist, ///
    over(label, sort(med_dist) descending ///
        label(labsize(small)) ///
        gap(*0.6)) ///
    title("Plaintiffs with at least 5 litigation cases", ///
        size(small) color(black)) ///
    ytitle("Mean Distance to PAE Centroid", size(small)) ///
    yscale(range(0 .)) ///
	    yline(2, lcolor(red) lpattern(dash) lwidth(medthin)) ///
    ylabel(, labsize(small) grid glcolor(gs14) glwidth(thin)) ///
    marker(1, msymbol(O) mcolor("29 71 119") msize(medium)) ///
    plotregion(margin(zero) lcolor(none)) ///
    graphregion(color(white) margin(medium)) ///
    name(Graph, replace)

graph export "C:\Users\vsterzi\Dropbox\Applications\Overleaf\Blurring the Line\Empirics\Like_PAEs.png", ///
    as(png) width(2400) height(1600) name("Graph") replace
restore


* Wasserstein robustness: compare distributions of dist_to_PAE
* PAE is the benchmark group

preserve
keep if !missing(dist_to_PAE, Type)

tempfile results
postfile handle str20 Type double W1 using `results', replace

* Store PAE quantiles
_pctile dist_to_PAE if Type=="PAE", nq(100)

matrix qpae = J(99,1,.)
forvalues q = 1/99 {
    matrix qpae[`q',1] = r(r`q')
}

* Compare each business model to PAE
levelsof Type if Type!="PAE", local(types)

foreach t of local types {

    _pctile dist_to_PAE if Type=="`t'", nq(100)

    scalar W = 0
    forvalues q = 1/99 {
        scalar W = W + abs(r(r`q') - qpae[`q',1])
    }

    scalar W = W/99

    post handle ("`t'") (W)
}

postclose handle
use `results', clear

sort W1
list, clean

restore

**************************************
mixed dist_to_PAE c.Technologymarketscore c.Productmarketscore zPTI i.filing if PAE != 1 || Claimants:, vce(cluster Claimants)
pwcorr Technologymarketscore Productmarketscore
**************************************



//*PLACEBO*//

egen quarter_n = group(quarter)
tab quarter_n quarter  // sanity check the mapping
reg quarter_n ib3.Type_n, vce(cluster Claimants)
mixed quarter_n ib3.Type_n || Claimants:, vce(cluster Claimants)



/* robustness check*/
mixed dist_to_PAE Technologymarketscore Productmarketscore i.filing zPTI || Claimants:, vce(cluster Claimants)

gen theta=Technologymarketscore/(Technologymarketscore+Productmarketscore)
mixed dist_to_PAE theta i.filing zPTI || Claimants:, vce(cluster Claimants)
replace theta=1 if PAE==1
mixed dist_to_PAE theta i.filing zPTI || Claimants:, vce(cluster Claimants)

