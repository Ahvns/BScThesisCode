pwd
cap cd Stata

do "event study command.do"

use "datasets\sample.dta", clear

// declare panel data
xtset id year


drop if year<2005 // drop unnecessary data - only includes population & settlements

label var fset "Exposure"

****************
*** Exposure ***
****************

preserve
levelsof treatment, local(chrts)
local sums ""
foreach x of local chrts {
  eststo c`x' : qui estpost sum fset type_* if treatment==`x'
  local sums `sums' c`x'
}
esttab `sums' using "output\tables\41summ.tex", label mtitles(Untreated 2006 2010 2014 2018 2021) cells("mean(fmt(%9.2f))") booktabs replace
restore

gen post = treatment<year & treatment!=0
label var femi "Relative Emmigration"
reghdfe fset post femi, absorb(i.year i.id) vce(cluster id)
eststo expdid
esttab expdid using "output\tables\51did.tex", ///
  booktabs label replace ///
  b(%9.4f) se(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  compress wide

eventstudy fset, treat(treatment) t(year) fe(year) cov(femi) plot(exptest) g(id) bins sto(exptest) keep

estfe exptest, labels(year "Year FE" id "Neighbourhood FE")
esttab exptest using "output\tables\A1 exp.tex", ///
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

// Model 1 - general model
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) sto(genmod1) plot(gen) bins noant

// Model 2 - general model with covariates
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) cov(popdens school) sto(genmod2) plot(gencov) bins noant

// Model 3 - general model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(genmod3) plot(genlin) bins noant

// Model 4 - general model with full baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) cov(popdens school) ti(avginclow avghsngsocial avgjobpopratio) sto(genmod4) plot(genfull) bins noant

// Model 5 - extended model
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(fset) sto(extmod1) plot(ext) bins noant

// Model 6 - extended model with covariates
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(fset) cov(popdens school) sto(extmod2) plot(extcov) bins noant

// Model 7 - extended model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(fset) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(extmod3) plot(extlin) bins noant

// Model 8 - extended model with full baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(fset) cov(popdens school) ti(avginclow avghsngsocial avgjobpopratio) sto(extmod4) plot(extfull) bins keep noant

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
  indicate("Exposure = fset" "Controls = popdens" "Linear baseline interaction = *avginclow#c*" `r(indicate_fe)', labels({Yes} {No})) ///
  compress

estfe genmod* extmod*, restore
estfe extmod*, labels(year#c.avginclow "Full baseline interaction")

// exposure coefficients
esttab extmod1 extmod2 extmod3 extmod4 using "output\tables\A3 expcoef.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.fset fset) ///
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
  yscale(r(-.6 .4)) ylabel(-.6(.2).4) ///
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
    local plotlab `"`plotlab' 0.k#c.fset="<" "'
  }
  else {
    if `i'==`kmax' {
      local plotlab `"`plotlab' `i'.k#c.fset=">""'
    }
    else {
      local plotlab `"`plotlab' `i'.k#c.fset="`=`i'-1'" "'
    }
  }
}

qui coefplot extmod1 extmod2 extmod3 extmod4, ///
  keep(*.k#c.fset) vertical omit base ///
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
reghdfe lcrimer post##c.fset, absorb(i.year i.id) vce(cluster id)
eststo did1

// Model 2
reghdfe lcrimer post##c.fset popdens school, absorb(i.year i.id) vce(cluster id)
eststo did2

// Model 3
reghdfe lcrimer post##c.fset popdens school c.year#(c.avginclow c.avghsngsocial c.avgjobpopratio i.type), absorb(i.year i.id) vce(cluster id)
eststo did3

// Model 4
reghdfe lcrimer post##c.fset popdens school, absorb(i.year i.year#(c.avgjobpopratio c.avghsngsocial c.avginclow) i.year#i.type i.id) vce(cluster id)
eststo did4

estfe did*, labels(year#c.avginclow "Full baseline interaction")

esttab did1 did2 did3 did4 using "output\tables\61did.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  keep(1.post fset 1.post#c.fset) ///
  indicate("Controls = popdens" "Linear baseline interaction = c.year*" `r(indicate_fe)', labels({Yes} {No})) ///
  compress

estfe did*, restore

********************
*** Anticipation ***
********************

// Model 7 - extended model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(fset) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(anttest) plot(anttest) bins keep

// event time coef table
esttab anttest using "output\tables\71 anttest.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k) ///
  compress wide


// exposure coefficients
esttab anttest using "output\tables\71 anttestexp.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.fset fset) ///
  compress wide

drop _est* cohort k

*******************
*** No Binning  ***
*******************

// Model 7 - extended model with linear baseline
eventstudy lcrimer, treat(treatment) t(year) g(id) fe(year) int(fset) cov(popdens school) trend(avginclow avghsngsocial avgjobpopratio type) sto(bintest) plot(bintest) noant keep

// event time coef table
esttab bintest using "output\tables\72 bintest.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k) ///
  compress wide

// exposure coefficients
esttab bintest using "output\tables\72 bintestexp.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  refcat(0.k "\textbf{Event time}", nolabel) ///
  keep(*.k#c.fset fset) ///
  compress wide

drop _est* cohort k

*********************************
*** Heterogenous Cohort bias  ***
*********************************

preserve
cap gen post = treatment<year & treatment!=0
replace treatment=0 if treatment==2021
reghdfe lcrimer post##c.fset##treatment /// dependent and treatment
  popdens school /// time varying controls
  c.year#(c.avginclow c.avghsngsocial c.avgjobpopratio i.type), /// baseline
  absorb(i.id i.year) vce(cluster id) // FE and se estimation

eststo didchrt

esttab didchrt using "output\tables\71did.tex", ///
  label replace booktabs ///
  se(%9.4f) b(%9.4f) ///
  stats(N r2 r2_a r2_within, fmt(%9.3f) labels("Observations" "\(R^{2}\)" "Adjusted \(R^{2}\)" "Within \(R^{2}\)")) ///
  keep(1.post fset 1.post#c.fset 1.post#*.treatment *.treatment#*) ///
  compress wide

restore

// regular diff-in-diff
xtreg lcrimer post##c.fset popdens school i.year, fe cluster(id)

// event study on exposure measure
eventstudy fset, treat(treatment) t(year) g(id) fe(year)

// regular diff in diff on exposure measure
xtreg fset post i.year, fe cluster(id)


// development of exposure measure over relative time
use "datasets\sample.dta", clear
qui keep if treatment!=0
qui sum year
qui drop if treatment>`=r(max)'
gen rel = year - treatment
qui sum fset
local ymin = r(min) - 1
local ymax = r(max) + 1
qui levelsof treatment, local(chrts)
foreach c of local chrts {
  preserve
  qui keep if treatment==`c'
  local fsetline ""
  qui levelsof id, local(ids)
  foreach x of local ids {
    local fsetline "`fsetline' (line fset rel if id==`x')"
  }
  qui sum rel
  local xmin = r(min)
  local xmax = r(max)
  qui twoway `fsetline', leg(off) ///
    ylabel(#3) yscale(r(`ymin' `ymax')) ///
    xscale(r(`xmin' `xmax')) xlabel(`xmin'(5)`xmax' 0) xtitle("Event Time") ///
    xline(0, lpattern(dash)) ///
    plotregion(margin(zero))  graphregion(color(white)) bgcolor(white) ///
    name(exp`c', replace)
  qui graph display exp`c'
  graph export "output\figures\c`c'exp.pdf", replace
  restore
}
