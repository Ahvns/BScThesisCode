pwd
capture cd Stata

// importeer data van veiligheidsindex
foreach t of numlist 2014 2016 2018 2020 2022 {
  qui import excel "sources\Datastructuur wijkprofiel_2022.xlsx", sheet("VI `t'") cellrange(A2:BS89) firstrow clear
  disp "imported data for year `t'"

  qui foreach x of varlist * {
    label var `x'`"`=`x'[1]'"'
  }
  qui drop in 1
  disp "Added variable labels"
  qui destring id, replace
  qui destring VI-overlast_norm, replace
  // gebieden aparte categorie geven
  qui replace Gebied = "_Gebied" if id > 3999 & id < 5000
  qui replace Gebied = "_Stad" if id == 5000
  disp "fixed area indicator"

  qui encode Gebied, g(area) l(gebied)
  qui encode Wijk, g(neighborhood) l(wijk)
  qui drop Gebied Wijk
  // variabele voor jaar
  gen year=`t'
  disp "added year indicator"
  order area neighborhood id year
  // dataset apart opslaan per jaar
  save "datasets\VI`t'.dta", replace
}

use "datasets\VI2014.dta", clear
// combineer VI datasets
foreach t of numlist 2016 2018 2020 2022 {
  qui append using "datasets\VI`t'.dta"
  disp "added year `t' to dataset"
}

// variabelen ordenen
order area neighborhood id year
sort area neighborhood id year
disp "ordered variables"

// gehele dataset opslaan
save "datasets\VIfull.dta", replace

// loop voor wanneer data voor andere jaren binnen is
foreach t of numlist 14 16 18 20 22 {
  cap import excel "sources\Context2021 voor derden.xlsx", sheet("Totaal contextindicatoren WP`t'") cellrange(A1:AP88) firstrow clear // WP22 -> WP`t'
  if _rc != 601 {
    disp "imported context variables for year `t'"
    // variabelen label met waarde in eerste observatie
    qui foreach x of varlist * {
      label var `x'`"`=`x'[1]'"'
    }
    disp "added variable labels"
    // eerste observatie verwijderen
    qui drop in 1
    qui drop Gebied Wijk

    // variabele numeriek maken
    qui destring, replace
    gen year = 20`t' // 2022 -> 20`t'
    order id year
    save "datasets\context20`t'.dta", replace // -> 20`t'
  }
}

use "datasets\VIfull.dta", clear
qui merge 1:1 id year using "datasets\context2022.dta"
drop _merge
disp "added context variables to full dataset"
qui replace type = type[_n+4] if id == id[_n+4]
qui replace surface = surface[_n+4] if id == id[_n+4]
qui replace type = type[_n-1] if id == id[_n-1]
qui replace surface = surface[_n-1] if id == id[_n-1]

save "datasets\VIfull.dta", replace

// treatment variabele importeren
qui import excel "sources\treatment periode.xlsx", sheet("Thema s Buurten") cellrange(A1:C72) firstrow clear
disp "imported time of treatment"
drop wijk
qui destring, replace
gen year = 2014
save "datasets\treatment.dta", replace

// treatment toevoegen aan dataset
use "datasets\VIfull.dta", clear
qui merge 1:1 id year using "datasets\treatment.dta"
qui drop _merge
replace Treatment = Treatment[_n-1] if id == id[_n-1]
disp "added time of treatment to full dataset"
order area neighborhood id year Treatment
sort area id year
save "datasets\VIfull.dta", replace

// populatie, huishoudens, aantal huizen en aantal werkplaatsen importeren
qui import excel "sources\Bevolking per 1 januari - Buurten.xlsx", sheet("Bevolking per 1 januari Buurten") cellrange(A1:R86) firstrow clear
qui destring, replace
disp "imported household home and job data"
qui reshape long pop hhamount homes jobs, i(id) j(year)
drop wijk
save "datasets\pop14-20.dta", replace

// bovengenoemde toevoegen aan dataset
use "datasets\VIfull.dta", clear
qui merge 1:1 id year using "datasets\pop14-20.dta", update
drop _merge
disp "added household home and job data to full dataset"
qui replace popdens = pop / surface
qui replace hsngdens = homes / surface
qui replace jobpopratio = jobs / (jobs + pop)
disp "updated density variables"

// voeg groot ijsselmonde samen
order year id
sort year id
qui gen ratio = pop / (pop + pop[_n+1]) if id ==1009
qui replace ratio = 1 - ratio[_n-1] if id==1010
order year id area neighborhood Treatment surface pop hhamount homes
qui foreach x of var VI-bldfuncliving {
  replace `x' = `x' * ratio if ratio!=.
}
qui drop ratio

qui foreach x of var pop-bldfuncliving {
  replace `x' = `x' + `x'[_n+1] if id==1009
}
qui drop if id == 1010
order area neighborhood id year Treatment pop hhamount homes surface jobs
sort area id year
disp "merged Groot IJsselmonde into one neighbourhood"
save "datasets\VIfull.dta", replace


// Import populate en vestigingen van buiten gemeente
qui import excel "sources\popvest0120.xlsx", sheet("Thema s Buurten") cellrange(A1:BW78) firstrow clear
drop wijk
disp "imported population and settlement data"
collapse (sum) pop* vest* Vertrek* School*, by(id)
rename *, lower
disp "merged neighbourhoods to fit Wijkprofiel data"
qui reshape long pop vest vertrek school, i(id) j(year)
qui replace pop = pop[_n+1] if id==id[_n+1]
drop if year==2021
rename vertrek emi
qui replace emi=0 if emi==. & year>2004
qui replace school=0 if school==. & year>2004
qui gen fset = vest / pop * 100
qui gen femi = emi / pop * 100
disp "calculated exposure measure"
qui bysort id : egen avgfset = mean(fset)
disp "calculated average exposure per neighbourhood"
qui reshape wide pop vest fset emi femi school, i(id) j(year)
save "datasets\popvest.dta", replace

// genereer tijdsinvariable covariates
use "datasets\VIfull.dta", clear
qui keep id year neighborhood area
qui drop if year != 2014
qui drop year
order id
disp "generated neighbourhood names dataset"
save "datasets\names.dta", replace

use "datasets\VIfull.dta", clear
qui keep id Treatment surface type-bldfuncliving
qui foreach x of varlist popyoung-bldfuncliving {
  local lab`x' : variable label `x'
  bysort id : egen avg`x' = mean(`x')
  label var avg`x' "average of `lab`x''"
  drop `x'
}
disp "calculated average of variables per neighbourhood"
order id
qui foreach v of var * {
  local l`v' : variable label `v'
    if `"`l`v''"' == "" {
      local l`v' "`v'"
    }
}
qui collapse (mean) Treatment-avgbldfuncliving, by(id)
qui foreach v of var * {
  label var `v' "`l`v''"
}
disp "fixed variable labels"
qui merge 1:1 id using "datasets\names.dta"
drop _merge
disp "added neighbourhood names"
order area neighborhood id
sort area id
qui drop if id>3999
disp "removed non-neighbourhood data"
save "datasets\baselinecovars.dta", replace

// genereer variabelen die veranderen over de tijd
use "datasets\VIfull.dta", clear
qui keep id year pop hhamount homes popdens-jobpopratio
qui drop if id>3999
local varnames ""
foreach x of var pop-jobpopratio {
  local varnames "`varnames' `x'"
}
qui reshape wide `varnames', i(id) j(year)
disp "made time variant variable dataset"
save "datasets\varswide.dta", replace

// outcome variabelen
qui import excel "sources\Misdrijven jaar gegevens - Buurten.xlsx", sheet("Misdrijven jaar gegevens Buurte") cellrange(A1:R78) firstrow clear
disp "imported crime data"
drop wijk
qui collapse (sum) crime*, by(id)
disp "merged neighbourhoods to match Wijkprofiel data"
qui merge 1:1 id using "datasets\popvest.dta"
qui reshape long crime pop vest fset emi femi school, i(id) j(year)
drop vest-_merge
qui replace crime = 0 if crime==.
qui gen crimer = crime / (pop) * 1000
disp "calculated crime rate per neighbourhood"
drop pop
qui reshape wide crime crimer, i(id) j(year)
save "datasets\crime.dta", replace

// sample maken
use "datasets\baselinecovars.dta", clear
qui merge 1:1 id using "datasets\crime.dta"
drop _merge
disp "merged baseline variables with crime data"
qui merge 1:1 id using "datasets\popvest.dta"
drop _merge
disp "added population data"
qui reshape long crime crimer pop vest fset emi femi school, i(id) j(year)
//qui drop if year<2005
disp "removed observations with no crime data"
qui gen lcrimer = ln(crimer) //TODO test
drop if fset==.
disp "removed observations with no exposure measure data"
disp "calculated log crime rate"
qui gen popdens = pop / surface
disp "calculated population density"
lab var crimer  "Crime rate"
lab var lcrimer "Log crime rate"
lab var popdens "Population density"
lab var fset  "Exposure measure"
qui tab year
local yrcnt = r(r)
qui levelsof id, local(ids)
foreach x of local ids {
  qui tab year if id == `x'
  local `x'count = r(r)
  if ``x'count' < `yrcnt' {
    disp "dropped neighbourhood with id `x', no data for full period"
  }
}
qui bysort id : drop if _N < `yrcnt'
rename *, lower
order area neighborhood id year treatment lcrimer fset femi school popdens crimer crime pop vest emi
sort area id year
qui replace treatment = 0 if treatment==.
label define cohorts 0 "untreated"
label values treatment cohorts
//qui keep area-type avghsngsocial avgfset //comment out for all time invariant covars
save "datasets\sample.dta", replace
