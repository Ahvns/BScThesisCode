
capture program drop eventstudy

quietly program eventstudy

	syntax varname(numeric fv ts), ///
	TREATment(varname) Time(varname) Group(varname) ///
	[ FE(varlist fv ts) INTeract(varlist fv ts) COVariates(varlist fv ts) STOre(name) TRend(varlist fv ts) TImeinteract(varlist fv ts) RESiduals PLOT(name) IW DEV BINs KEEP FULLINTeract NOANTicipation NOCLUSTER]

	quietly {
		gen cohort			= `treatment'
		gen k						= `time' - cohort
		label var k "Event Time"
		replace cohort	= 0 	if `treatment'	== .
		preserve
		drop if cohort==0
		levelsof cohort, local(cohorts) // namen van cohorts
		restore
		sum `time'
	}
	if "`noanticipation'" == "" {
		foreach x of local cohorts {
			if `x' > r(max) {
				qui replace cohort = 0 if cohort == `x'
				disp "cohort `x' coded as untreated, treatment occurs after observed period."
			}
			if `x' < r(min) {
				qui replace cohort = . if cohort == `x'
				disp "cohort `x' ignored, treatment occurs before observed period."
			}
		}
	}
	quietly {
		replace k = . if cohort == 0
		if "`noanticipation'" != "" {
			replace k = -1 if k < -1
		}
	}
	if "`bins'" != "" {
		preserve
		contract k
		qui drop if _freq < 10
		qui sum k
		local newkmin = r(min) - 1
		local newkmax = r(max) + 1
		restore
		qui replace k = `newkmin' if k < `newkmin'
		qui replace k = `newkmax' if k > `newkmax'
	}

	quietly {
		sum k
		local shift				= -r(min)
		local tot					=	`shift'
		replace k	 				= k + `shift'
		qui levelsof k, local(kvals)
		local klabs ""
		foreach k of local kvals {
			local klabs `"`klabs' `k' "~~k = `=`k'-`shift''""'
		}
		label define reltime `klabs', replace
		label values k reltime
		local shift				= `shift' -1
		fvset base `shift' k
		replace k					= 0		if cohort == 0
		local fixedeffects	"i.`group'"
		local covars ""
		local interactions i.k

	}

	if "`dev'" != "" {
		display "relative time generated"
		tab k
		display "time of treatment is at k = `tot'"

	}

	// if multiple cohorts, add cohort fixed effects
	/*qui tab cohort
	if r(r) > 2 {
		local fixedeffects i.cohort `fixedeffects'
	}

	if "`dev'" != "" {
		display "cohort fe added"
	}*/

	foreach x of local fe {
		local fixedeffects i.`x' `fixedeffects'
	}

	if "`dev'" != "" {
		display "fe added"
		display "`fixedeffects'"
	}
	// add interactions with relative time indicators
	local fullint ""
	if "`interact'" != "" {
		foreach x of local interact {
			gen `x'floor = `x' != floor(`x')
			quietly sum `x'floor

			if r(max)==1 {
				local interactions `interactions' c.`x'#i.k
				if "`fullinteract'" != "" {
					local fullint c.`x'##`fullint'
				}
				local covars `covars' `x'
			}

			else {
				local interactions `interactions' i.`x'#i.k
				if "`fullinteract'" != "" {
					local fullint i.`x'##`fullint'
				}
				local fixedeffects i.`x' `fixedeffects'
			}

			drop `x'floor
		}

		if "`fullinteract'" != "" {
			local fullint = substr("`fullint'", 1, length("`fullint'")-2)
			local fullint `fullint'##i.k
			local interactions `fullint'
			local covars ""
		}
	}

	if "`dev'" != "" {
		display "interaction terms added"
		display "`interactions'"
	}

	// add covariates
	foreach x of local covariates {
		local covars `covars' `x'
	}

	if "`dev'" != "" {
		display "covariates added"
		display "`covars'"
	}

	// baseline variables linear time trend
	if "`trend'" != "" {
		foreach var of local trend {
			gen `var'floor = `var' != floor(`var')
			quietly sum `var'floor
			if r(max)==1 {
				local covars "`covars' c.`var'#c.`time'"
			}
			else {
				local covars "`covars' i.`var'#c.`time'"
			}
			drop `var'floor
		}
		if "`dev'" != "" {
			disp "linear time trends added"
			disp "`covars'"
		}
	}

	// time FE interaction with baseline variables
	if "`timeinteract'" != "" {
		foreach var of local timeinteract {
			gen `var'floor = `var' != floor(`var')
			quietly sum `var'floor
			if r(max)==1 {
				local fixedeffects "`fixedeffects' c.`var'#i.`time'"
			}
			else {
				local fixedeffects "`fixedeffects' i.`var'#i.`time'"
			}
			drop `var'floor
		}
		if "`dev'" != "" {
			disp "full time FE interactions added"
			disp "`fixedeffects'"
		}
	}


	// regression
	if "`nocluster'" != "" {
		local cluster ""
	}
	else {
	    local cluster "vce(cluster `group')"
	}
	qui xtset `group' `time'
	if "`residuals'" != "" {
		reghdfe `varlist' `interactions' `covars', absorb(`fixedeffects') `cluster' res(res)
		predict yhatd, xbd
		predict yhat, xb
		predict d, d
	}
	else {
		reghdfe `varlist' `interactions' `covars', absorb(`fixedeffects') `cluster'
	}


	// store estimates
	if "`store'" != "" {
		eststo `store'
	}

	// plot coefficient grpahs
	if "`plot'" != "" {
		qui tab k
		local kcount = r(r)
		local kcount = `kcount' - 2
		local plotlab ""
		local i ""
		forvalues x = 1/`kcount' {
			local i = `x' - `tot'
			local plotlab `"`plotlab' `x'.k="`i'""'
		}
		local ++kcount
		if "`bins'" != "" {
			local plotlab `"0.k="<" `plotlab' `kcount'.k=">""'
		}
		else {
			local plotlab `"`plotlab' `kcount'.k="`=`kcount' - `tot'' ""'
			if "`noanticipation'" != "" {
				local plotlab `"0.k="<" `plotlab'"'
			}
			else {
				local plotlab `"0.k="`=-`tot''" `plotlab'"'
			}
		}

		// TODO fix discrete interaction
		// base effect
		coefplot, keep(*.k) vertical omit base ///
			rename(`plotlab') ///
			yline(0, lcolor(gray)) xline(`tot'.5 , lpattern(dash)) ///
			xtitle("Event time") ytitle("`: variable label `varlist'' relative to event time") ///
			graphregion(color(white)) bgcolor(white) name("kplot`plot'", replace) ///
			recast(connected) ciopts(recast(rarea) color(%75))
		qui graph display kplot`plot'
		graph export "output\figures\kplot`plot'.pdf", replace

		//interaction effect
		if "`interact'" != "" {
			foreach j of local interact {
				gen `j'floor = `j' != floor(`j')
				quietly sum `j'floor

				if r(max)==1 {
					qui tab k
					local kcount = r(r)
					local kcount = `kcount' - 2
					local plotlab ""
					local i ""
					forvalues x = 1/`kcount' {
						local i = `x' - `tot'
						local plotlab `"`plotlab' `x'.k#c.`j'="`i'""'
					}
					local ++kcount
					if "`bins'" != "" {
						local plotlab `"0.k#c.`j'="<" `plotlab' `kcount'.k#c.`j'=">""'
					}
					else {
						local plotlab `"`plotlab' `kcount'.k#c.`j'="`=`kcount' - `tot'' ""'
						if "`noanticipation'" != "" {
							local plotlab `"0.k#c.`j'="<" `plotlab'"'
						}
						else {
							local plotlab `"0.k#c.`j'="`=-`tot''" `plotlab'"'
						}
					}
					coefplot, keep(*.k#c.`j') vertical omit base ///
					rename(`plotlab') ///
					yline(0, lcolor(gray)) xline(`tot'.5 , lpattern(dash)) ///
					xtitle("Event time") ytitle("`: variable label `varlist'' relative to event time") ///
					graphregion(color(white)) bgcolor(white) name("k`j'plot`plot'", replace) ///
					recast(connected) ciopts(recast(rarea) color(%75))
					qui graph display k`j'plot`plot'
					graph export "output\figures\k`j'plot`plot'.pdf", replace
				}
				/*else {
					// TODO fix "keep" option
					qui levelsof `j', local(`j'names)
					foreach n in local `j'names {
						qui tab k
						local kcount = r(r)
						local kcount = `kcount' - 2
						local plotlab ""
						local i ""
						forvalues x = 1/`kcount' {
							local i = `x' - `tot'
							local plotlab `"`plotlab' `x'.k#`n'.`j'="`i'""'
						}
						local ++kcount
						local plotlab `"0.k#`n'.`j'="<" `plotlab' `kcount'.k#`n'.`j'=">""'
						coefplot, keep(*.k#`n'.`j') vertical omit base rename(`plotlab') xline(`tot'.5 , lpattern(dash))
					}
				}*/

				drop `j'floor
			}
		}
	}

	// Sun & Abrahams IW estimator
	/* UNFINISHED !!!
	if "`iw'"	!= "" {
		tempname currentbetas

		// calculate cohort weights per k
		preserve
		qui drop if cohort==0
		qui levelsof cohort, local(chrtnames) // namen van cohorts
		restore

		qui sum k
		local kmax = r(max)

		foreach y of local chrtnames {
			local currentweight ""
			local cohortweight ""

			qui forvalues x = 0/`kmax' {
				preserve
				drop if k != `x'
				contract cohort, percent(p)
				replace p = p / 100
				sum p if cohort == `y'
				local currentweight = r(mean)
				local cohortweight `cohortweight' , `currentweight'
				restore
			}

			if "`dev'" != "" {
					disp "weights for cohort `y' calculated "
			}

			matrix input c`y'weights = (`cohortweight')

			if "`dev'" != "" {
					disp "weights for cohort `y' saved "
			}
		}

		// calculate betas per cohort
		foreach y of local chrtnames {
			preserve

			qui keep if cohort == 0 | cohort == `y'

			qui tab k
			local kcount = r(r)

			qui reg `varlist' ib`shift'.k `interactions' `covars' `fixedeffects', ///
			robust cluster(`group')

			matrix `currentbetas' = e(b)
			local cohortbeta = `currentbetas'[1,1]

			qui sum k if cohort==`y'
			local kmin = r(min)

			if r(min) > 1 {
				forvalues i = 2/`kmin' {
					local cohortbeta "`cohortbeta' \ 0"
				}
			}

			local ++kmin

			forvalues i = 2/`kcount' {
				local cohortbeta `cohortbeta' \ `=`currentbetas'[1,`i']'
			}

			if r(max) < `kmax' {
				local kdiff = `kmax' - r(max)
				forvalues i = 1/`kdiff' {
					local cohortbeta "`cohortbeta' \ 0"
				}
			}

			restore
			if "`dev'" != "" {
					disp "betas for cohort `y' calculated "
			}

			matrix input c`y'betas = (`cohortbeta')

			if "`dev'" != "" {
					disp "betas for cohort `y' saved "
			}
		}

		// calculate weighted betas per cohort
		local ++kmax
		foreach y of local chrtnames {
			matrix wc`y'betas = c`y'betas * c`y'weights
			if "`dev'" != "" {
					disp "weighted betas for cohort `y' calculated"
			}

			matrix c`y'wbeta = J(1,`kmax',0)

			forvalues i = 1/`kmax' {
				matrix c`y'wbeta[1,`i']= wc`y'betas[`i',`i']
				if missing(c`y'wbeta[1,`i']) {
					matrix c`y'wbeta[1,`i'] = 0
				}
			}

			//matrix list c`y'wbeta

			if "`dev'" != "" {
					disp "weighted betas for cohort `y' saved"
			}
		}

		// estimate betas
		// TODO check if weights sum to one
		matrix betas = J(1,`kmax',0)
		//matrix list betas
		foreach y of local chrtnames {
			matrix betas = betas + c`y'wbeta
			if "`dev'" != "" {
				disp "cohort `y' added"
			}
		}

		local --tot
		local colnames "0"
		forvalues i = 1/`tot' {
			local colnames "-`i' `colnames'"
		}
		local colnames "x `colnames'"
		local tot = `kmax' - `tot' - 2
		forvalues i = 1/`tot' {
			local colnames "`colnames' `i'"
			disp "`i'"
		}
		matrix colnames betas = `colnames'
		matrix rownames betas = beta
		coefplot matrix(betas), noci vertical base

	}*/

	if "`keep'" == "" {
		drop cohort k
	}

end
