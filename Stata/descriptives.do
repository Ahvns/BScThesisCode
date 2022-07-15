pwd
cap cd Stata

use "datasets\sample.dta", clear

// figure 1 - crime rate histogram
preserve
drop if year<2005
qui hist crimer, freq name(cr, replace) graphregion(color(white)) bgcolor(white)
qui graph display cr
graph export "output\figures\crimerhist.pdf", replace
qui hist lcrimer, freq name(lcr, replace) graphregion(color(white)) bgcolor(white)
qui graph display lcr
graph export "output\figures\lcrimerhist.pdf", replace
restore

// identify outliers
preserve
drop if year<2005
drop if lcrimer<6.5
levelsof id, local(outliers)
tab neighborhood
restore

//outlier info
foreach id of local outliers {
  preserve
  keep if id == `id'
  disp "ID is `id'"
  sum lcrimer treatment type area avgstudents avginclow avghsngsocial avgbldfuncliving
  restore
}

preserve
drop if year<2005
keep if avgbldfuncliving<0.25
tab neighborhood
levelsof id, local(ids)
restore

// drop outliers
foreach id of local ids {
  drop if id == `id'
}

// figure 2
preserve
drop if year<2005
qui hist crimer, freq name(cr, replace) graphregion(color(white)) bgcolor(white)
qui graph display cr
graph export "output\figures\crimerhistnoout.pdf", replace
qui hist lcrimer, freq name(lcr, replace) graphregion(color(white)) bgcolor(white)
qui graph display lcr
graph export "output\figures\lcrimerhistnoout.pdf", replace
restore

// table 1
preserve
lab var crimer "{Crime Rate}"
lab var pop "{Population}"
lab var popdens "{Population Density}"
lab var vest "{Settlements}"
lab var avghsngsocial "{Social Housing}"
lab var avginclow "{Low Income HH}"
lab var avgjobpopratio "{Resident-Job Balance}"
lab var avgbldfuncliving "{Housing Fraction}"
levelsof type, local(types)
foreach x of local types {
  gen type_`x' = type==`x'
  lab var type_`x' "{Type `x'}"
}
levelsof treatment, local(chrts)
local sums ""
foreach x of local chrts {
  eststo c`x' : qui estpost sum crimer pop popdens vest avginclow avghsngsocial avgjobpopratio avgbldfuncliving type_* if treatment==`x'
  local sums `sums' c`x'
}
esttab `sums' using "output\tables\41summ.tex", label mtitles(Untreated 2006 2010 2014 2018 2021) cells("mean(fmt(%9.2f))") booktabs replace
restore

// figure 3
qui levelsof treatment, local(chrts)
foreach x of local chrts {
  preserve
  drop if year<2005
  qui drop if treatment != `x'
  qui levelsof id, local(ids)
  local chrt`x'lines ""
  foreach y of local ids {
    local chrt`x'lines "`chrt`x'lines' (line lcrimer year if id==`y')"
  }
  if "`x'" != "0" {
    qui twoway `chrt`x'lines', xline(`x', lpattern(dash)) graphregion(color(white)) bgcolor(white) leg(off) ylabel(3(1)6) yscale(r(3 6.5)) name(c`x'lcr, replace)
    qui graph display c`x'lcr
    graph export "output\figures\c`x'lcr.pdf", replace
  }
  else {
    qui twoway `chrt`x'lines', graphregion(color(white)) bgcolor(white) leg(off) ylabel(3(1)6) yscale(r(3 6.5)) name(c`x'lcr, replace)
    qui graph display c`x'lcr
    graph export "output\figures\c`x'lcr.pdf", replace
  }
  restore
}
