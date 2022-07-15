pwd
cap cd Stata

do "event study command.do"

use "datasets\sample.dta", clear

// declare panel data
xtset id year

// drop outliers
preserve
  drop if year<2005
  keep if avgbldfuncliving<0.25
  tab neighborhood
  levelsof id, local(ids)
restore
foreach id of local ids {
  drop if id == `id'
}

****************
*** Exposure ***
****************

drop if year<2005

// summary stats
qui hist fset, freq name(exp, replace) graphregion(color(white)) bgcolor(white)
qui graph display exp
graph export "output\figures\exphist.pdf", replace

// outliers
sum fset, d
scalar out = r(p50) + 3 * r(sd)
preserve
  keep if fset > out
  bysort neighborhood: sum
restore
preserve
  drop if avgstudents<0.15
  tab neighborhood
  levelsof id, local(ids)
restore
foreach id of local ids {
  drop if id == `id'
}

qui hist fset, freq name(exp, replace) graphregion(color(white)) bgcolor(white)
qui graph display exp
graph export "output\figures\exphistno.pdf", replace

egen st_fset = std(fset)
lab var st_fset "Standardised Exposure"

preserve
levelsof treatment, local(chrts)
local sums ""
foreach x of local chrts {
  eststo c`x' : qui estpost sum fset st_fset if treatment==`x'
  local sums `sums' c`x'
}
eststo total : qui estpost sum fset st_fset
local sums `sums' total
esttab `sums' using "output\tables\51exp.tex", label mtitles(Untreated 2006 2010 2014 2018 2021 Total) cells("mean(fmt(%9.2f))" "sd(fmt(%9.2f))") booktabs replace
restore

gen post = treatment<year & treatment!=0

label var femi "Relative Emigration"
reghdfe fset post femi, absorb(i.year i.id) vce(cluster id)
eststo expdid
reghdfe st_fset post femi, absorb(i.year i.id) vce(cluster id)
eststo expsdid
esttab expdid expsdid using "output\tables\51did.tex", ///
  booktabs label replace ///
  b(%9.4f) se(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  compress wide

eventstudy fset, treat(treatment) t(year) fe(year) cov(femi) plot(exptest) g(id) bins sto(exptest)
eventstudy st_fset, treat(treatment) t(year) fe(year) cov(femi) plot(expstest) g(id) bins sto(expstest) keep

estfe exptest expstest, labels(year "Year FE" id "Neighbourhood FE")
esttab exptest expstest using "output\tables\A1 exp.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k femi) ///
  indicate(`r(indicate_fe)', labels({Yes} {No})) ///
  compress wide

drop _est* cohort k


***********************
*** No Anticipation ***
***********************
lab var st_fset "Exposure"

// Model 1 - general model
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) sto(genmod1) plot(gen) bins noant

// Model 2 - general model with covariates
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) cov(popdens school) sto(genmod2) plot(gencov) bins noant

// Model 3 - general model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(genmod3) plot(genlin) bins noant

// Model 4 - general model with full baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) cov(popdens school) ti(avginclow avghsngsocial avgjobpopratio) sto(genmod4) plot(genfull) bins noant

// Model 5 - extended model
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) sto(extmod1) plot(ext) bins noant

// Model 6 - extended model with covariates
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(popdens school) sto(extmod2) plot(extcov) bins noant

// Model 7 - extended model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(extmod3) plot(extlin) bins noant

// Model 8 - extended model with full baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(popdens school) ti(avginclow avghsngsocial avgjobpopratio) sto(extmod4) plot(extfull) bins keep noant

estfe genmod* extmod*, labels(year#c.avginclow "Full baseline interaction")

// event time coef table
esttab genmod1 genmod2 genmod3 genmod4 ///
  extmod1 extmod2 extmod3 extmod4 ///
  using "output\tables\A2 kcoef.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k) ///
  indicate("Exposure = st_fset" "Controls = popdens" "Linear baseline interaction = *avginclow#c*" `r(indicate_fe)', labels({Yes} {No})) ///
  compress

estfe genmod* extmod*, restore
estfe extmod*, labels(year#c.avginclow "Full baseline interaction")

// exposure coefficients
esttab extmod1 extmod2 extmod3 extmod4 using "output\tables\A3 expcoef.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.st_fset st_fset) ///
  indicate("Controls = popdens" "Linear baseline interaction = *avginclow#c*" `r(indicate_fe)', labels({Yes} {No})) ///
  compress

local plotlab ""
qui sum k
local kmax = r(max)
qui levelsof k, local(kvals)
foreach i of local kvals {
  if `i'==0 {
    local plotlab `"`plotlab' 0.k="<" "'
  }
  else {
    if `i'==`kmax' {
      local plotlab `"`plotlab' `i'.k=">""'
    }
    else {
      local plotlab `"`plotlab' `i'.k="`=`i'-1'" "'
    }
  }
}

qui coefplot genmod1 genmod2 genmod3 genmod4, ///
  keep(*.k) vertical omit base ///
  yline(0, lcolor(gray)) xline(1.5 , lpattern(dash)) ///
  xtitle("Event time") ytitle("Log Crime Rate relative to event time") ///
  graphregion(color(white)) bgcolor(white) ///
  recast(connected) ciopts(recast(rarea) color(%15)) ///
  nooffsets rename(`plotlab') plotlabels("General" "Controls" "Linear Baseline" "Full Baseline")

graph export "output\figures\kplotgenna.pdf", replace

qui coefplot extmod1 extmod2 extmod3 extmod4, ///
  keep(*.k) vertical omit base ///
  yline(0, lcolor(gray)) xline(1.5 , lpattern(dash)) ///
  xtitle("Event time") ytitle("Log Crime Rate relative to event time") ///
  graphregion(color(white)) bgcolor(white) ///
  recast(connected) ciopts(recast(rarea) color(%15)) ///
  nooffsets rename(`plotlab') plotlabels("General" "Controls" "Linear Baseline" "Full Baseline")

graph export "output\figures\kplotextna.pdf", replace

local plotlab ""
qui sum k
local kmax = r(max)
qui levelsof k, local(kvals)
foreach i of local kvals {
  if `i'==0 {
    local plotlab `"`plotlab' 0.k#c.st_fset="<" "'
  }
  else {
    if `i'==`kmax' {
      local plotlab `"`plotlab' `i'.k#c.st_fset=">""'
    }
    else {
      local plotlab `"`plotlab' `i'.k#c.st_fset="`=`i'-1'" "'
    }
  }
}

qui coefplot extmod1 extmod2 extmod3 extmod4, ///
  keep(*.k#c.st_fset) vertical omit base ///
  yline(0, lcolor(gray)) xline(1.5 , lpattern(dash)) ///
  xtitle("Event time") ytitle("Log Crime Rate relative to event time") ///
  graphregion(color(white)) bgcolor(white) ///
  recast(connected) ciopts(recast(rarea) color(%15)) ///
  nooffsets rename(`plotlab') plotlabels("General" "Controls" "Linear Baseline" "Full Baseline")

graph export "output\figures\fsetplotna.pdf", replace

drop _est* cohort k

********************
*** DiD Estimate ***
********************

// Model 1
reghdfe lcrimer post##c.st_fset, absorb(i.year i.id) vce(cluster id)
eststo did1

// Model 2
reghdfe lcrimer post##c.st_fset popdens school, absorb(i.year i.id) vce(cluster id)
eststo did2

// Model 3
reghdfe lcrimer post##c.st_fset popdens school c.year#(c.avginclow c.avghsngsocial c.avgjobpopratio i.type), absorb(i.year i.id) vce(cluster id)
eststo did3

// Model 4
reghdfe lcrimer post##c.st_fset popdens school, absorb(i.year i.year#(c.avgjobpopratio c.avghsngsocial c.avginclow) i.year#i.type i.id) vce(cluster id)
eststo did4

estfe did*, labels(year#c.avginclow "Full baseline interaction")

esttab did1 did2 did3 did4 using "output\tables\61did.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  keep(1.post st_fset 1.post#c.st_fset) ///
  indicate("Controls = popdens" "Linear baseline interaction = c.year*" `r(indicate_fe)', labels({Yes} {No})) ///
  compress

estfe did*, restore

*************************
*** Residual Analysis ***
*************************

// event study
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) plot(extlin) bins noant res sto(nar1)

qui hist res, freq name(esreshist, replace) graphregion(color(white)) bgcolor(white) xtitle("Residuals")
qui graph display esreshist
graph export "output\figures\esreshist.pdf", replace

qui scatter res yhatd, name(esresplot, replace) graphregion(color(white)) bgcolor(white) xtitle("Fitted Values") ytitle("Residuals")
qui graph display esresplot
graph export "output\figures\esresplot.pdf", replace

xtqptest res, order(1)
xtistest res, lags(1)

drop res yhatd yhat d

// AR(1) term added
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(l.lcrimer popdens school) trend(avginclow avghsngsocial avgjobpopratio type) plot(ar1) bins noant res keep sto(ar1)

qui hist res, freq name(arreshist, replace) graphregion(color(white)) bgcolor(white) xtitle("Residuals")
qui graph display arreshist
graph export "output\figures\arreshist.pdf", replace

qui scatter res yhatd, name(arresplot, replace) graphregion(color(white)) bgcolor(white) xtitle("Fitted Values") ytitle("Residuals")
qui graph display arresplot
graph export "output\figures\arresplot.pdf", replace

xtqptest res, order(1)
xtistest res, lags(1)

// event time coef table
esttab nar1 ar1 ///
  using "output\tables\A4 kcoef.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k) ///
  compress

// exposure coefficients
esttab nar1 ar1 using "output\tables\A4 expcoef.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.st_fset st_fset) ///
  compress

drop res yhatd yhat d cohort k _est*

// AR(1) diff in diff
reghdfe lcrimer post##c.st_fset l.lcrimer popdens school c.year#(c.avginclow c.avghsngsocial c.avgjobpopratio i.type), absorb(i.year i.id) vce(cluster id)

eststo ardid

esttab ardid using "output\tables\72ardid.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  keep(1.post st_fset 1.post#c.st_fset *lcrimer*) ///
  compress

********************
*** Anticipation ***
********************

// Model 7 - extended model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(l.lcrimer popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(anttest) plot(anttest) bins keep

// event time coef table
esttab anttest using "output\tables\71 anttest.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k st_fset *lcrimer*) ///
  compress wide

// exposure coefficients
esttab anttest using "output\tables\71 anttestexp.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.st_fset) ///
  compress wide

drop _est* cohort k

*******************
*** No Binning  ***
*******************

// Model 7 - extended model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(st_fset) cov(l.lcrimer popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(bintest) plot(bintest) noant keep

// event time coef table
esttab bintest using "output\tables\72 bintest.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k st_fset *lcrimer*) ///
  compress wide

// exposure coefficients
esttab bintest using "output\tables\72 bintestexp.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.st_fset) ///
  compress wide

drop _est* cohort k

*********************************
*** Heterogenous Cohort bias  ***
*********************************

preserve
cap gen post = treatment<year & treatment!=0
replace treatment=0 if treatment==2021
reghdfe lcrimer post##c.st_fset##treatment /// dependent and treatment
  popdens school l.lcrimer /// time varying controls
  c.year#(c.avginclow c.avghsngsocial c.avgjobpopratio i.type), /// baseline
  absorb(i.id i.year) vce(cluster id) // FE and se estimation

eststo didchrt

esttab didchrt using "output\tables\71did.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  keep(1.post st_fset *lcrimer* 1.post#c.st_fset 1.post#*.treatment *.treatment#*) ///
  compress wide

restore
