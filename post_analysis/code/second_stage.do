./**********************************************************
        _                       ____
       ( )                     |____|
    ___|/________|\____________|____|_______
   |__/|/_)_|____|_______|\__(_)__(_)_______|
   |_(_|_/__|__(_)_______|\_________________|
   |___|____|__________(_)__________________|
   |________|_________________________(_)___|
                                        |
                                        | 
 * tables.do 
 * BAV project
 * Kusum Ailawadi, Harald van Heerde, and Hannes Datta
 * bug reports: h.datta@tilburguniversity.edu
 
/ **********************************************************
 **********************************************************/

cap log close
set linesize 255

version 13
clear all
set mem 500m
set matsize 10000
set more off
set seed 04251963
set sortseed 04251963
set scheme s2mono

*global mlist = "MNL_copula_5mmix_nomc "
global curr_modelname = "MNL_copula_5mmix_nomc"
*global path = "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"
*cd "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"

global path = "c:\Users\hanne\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"
cd "c:\Users\hanne\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"

log using ../output/stata_log.txt, replace


program do_preclean
	* Kick out non-BAV brands
	drop if bav_brand == 0
	
	* Generate clustering variables (cat_name + brand_name)
	generate cat_brand = cat_name+ "_" +brand_name
	egen cat_brand_num = group(cat_brand)
	
	egen cat_name_num = group(cat_name)

	g estbrand = 1-newbrnd
	
	* Label variables
	local mcvars c2 c3 c4 herf catgrowth_rel catgrowth_abs cat_invol cat_hedonic cat_utilit cat_perfrisk cat_socdemon cat_muchtolose cat_differences cat_fhedonic cat_fsocdemon cat_fperfrisk
	local otherinteract_vars seccat newbrnd estbrand fmcg_seccat retail_seccat food_drink_cigs

	label var seccat "Secondary market"
	label var retail_seccat "Retail chain second. cat."
	label var fmcg_seccat "FMCG second. cat."
	label var food_drink_cigs "Food, drink and cigs"
	
	label var newbrnd "New brand"
	label var estbrand "Established brand"
	
	label var c2 "Category Concentration (C2)"
	label var c3 "Category Concentration (C3)"
	label var c4 "Category Concentration"
	label var herf "Category Concentration (Herfindahl)"
	
	label var cat_hedonic "Category Hedonic Nature"
	label var cat_perfrisk "Category Functional Risk"
	label var cat_socdemon "Category Social Value"
	
	label var catgrowth_rel "Category growth rel."
	label var catgrowth_abs "Category growth abs."
	label var cat_muchtolose "Category Much to Lose"
	label var cat_utilit "Category Utilitarianism"
	label var cat_invol "Category Involvement"

	label var cat_fhedonic "PC for Hedonic"
	label var cat_fsocdemon "PC for Social Value"
	label var cat_fperfrisk "PC for Functional Risk"

	* Mean-center variables
	foreach var in `mcvars' {
		quietly sum `var'
		g `var'_mc = `var' - r(mean)
		local l: variable label `var'
		label var `var'_mc "`l'"
		}
	
	* Interactions
	foreach var in `mcvars' {
		g f2_pc1_stdX`var' = f2_pc1_std * `var'_mc
		g f2_pc2_stdX`var' = f2_pc2_std * `var'_mc
		g f3_pc1_stdX`var' = f3_pc1_std * `var'_mc
		g f3_pc2_stdX`var' = f3_pc2_std * `var'_mc
		g f3_pc3_stdX`var' = f3_pc3_std * `var'_mc
		local l: variable label `var'
		label var f2_pc1_stdX`var' "     x `l'"
		label var f2_pc2_stdX`var' "     x `l'"
		}
		
	* Interactions for non-meancentered variables
	foreach var in `otherinteract_vars' {
		g f2_pc1_stdX`var' = f2_pc1_std * `var'
		g f2_pc2_stdX`var' = f2_pc2_std * `var'
		g f3_pc1_stdX`var' = f3_pc1_std * `var'
		g f3_pc2_stdX`var' = f3_pc2_std * `var'
		g f3_pc3_stdX`var' = f3_pc3_std * `var'
		local l: variable label `var'
		label var f2_pc1_stdX`var' "     x `l'"
		label var f2_pc2_stdX`var' "     x `l'"
		}
		
	label var f2_pc1_std "Principal Component for Relevant Stature"
	label var f2_pc2_std "Principal Component for Energized Differentiation"
	
	g f2_pc1_stdXf2_pc2_std = f2_pc1_std*f2_pc2_std
	
	label var f2_pc1_stdXf2_pc2_std "Relevant Stature x Energized Diff."
	
	g f2_pc1_stdXf2_pc2_stdXestbrand = f2_pc1_stdXf2_pc2_std * estbrand
	label var f2_pc1_stdXf2_pc2_stdXestbrand "Relevant Stature x Energized Diff."
	
	label var bav_relevance_std "Relevance"
	label var bav_esteem_std "Esteem"
	label var bav_knowledge_std "Knowledge"
	label var bav_energizeddiff_std "Energized Differentiation"
	
end

program load_elasticity
	syntax, fn(string) 
	insheet using "`fn'", clear 
	drop if f2_pc1_std == . | elast_std == .
	gen weights = 1/elast_se_std
	
end


program elasticity
	syntax, fn(string) elast_vars(varlist) [ttitle(string)]
	
	local elast_dv elast_std
	
	if "`ttitle'"=="" local ttitle = "Elasticities"
	
	eststo clear
	
	/* rreg_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="rreg_pr_bt"
	do_preclean
	sort cat_name
	by cat_name: summ `elast_dv'

	eststo m1: quietly reg `elast_dv' `elast_vars' [pw=weights] 

	/* pi_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pi_bt"
	do_preclean
	sort cat_name
	by cat_name: summ `elast_dv'

	eststo m2: quietly reg `elast_dv' `elast_vars' [pw=weights] 

	/* fd_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="fd_bt"
	do_preclean
	sort cat_name
	by cat_name: summ `elast_dv'

	eststo m3: quietly reg `elast_dv' `elast_vars' [pw=weights] 

	/* pct_store_skus_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pct_store_skus_bt"
	do_preclean
	sort cat_name
	by cat_name: summ `elast_dv'

	eststo m4: quietly reg `elast_dv' `elast_vars' [pw=weights]

	/* ad */
	load_elasticity, fn("`fn'")
	keep if var_name=="adstock_bt"
	do_preclean
	sort cat_name
	by cat_name: summ `elast_dv'

	eststo m5: quietly reg `elast_dv' `elast_vars' [pw=weights] 
	
		
	* capture erase "$rtf_out" *append
	esttab m* using "$rtf_out", append mtitles("Regular Price Elasticity" "Promotional Price Elasticity" "Feature / Display Response" "Distribution Elasticity" "Advertising Elasticity") nodepvar label ///
	   addnote("All estimated with WLS.") title(`ttitle') modelwidth(8 8 8 8 8) varwidth(16) b(2) se(2) ///
	   stats(r2 N, labels("R-squared" "Number of elasticity observations")  fmt(2 0)) ///
	   onecell nogap star(* .10 ** .05 *** .01) replace
				
end

program equity_final
	syntax, ttitle(string) [dv(varlist)]
	eststo clear
	
	if "`dv'"=="" local dv sbbe_std
	
	eststo m: quietly reg `dv' f2_pc1_std f2_pc2_std ///
									  seccat ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)

	vif
	
	eststo m_pca: quietly reg `dv' f2_pc1_std f2_pc2_std ///
									  seccat ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_fhedonic f2_pc1_stdXcat_fhedonic f2_pc2_stdXcat_fhedonic ///
									  cat_fperfrisk f2_pc1_stdXcat_fperfrisk f2_pc2_stdXcat_fperfrisk ///
									  cat_fsocdemon f2_pc1_stdXcat_fsocdemon f2_pc2_stdXcat_fsocdemon ///
									  [pw=weights], vce(cluster cat_brand_num)

    vif
	
	capture erase "$rtf_out"
*nomtitles 
	esttab m* using "$rtf_out", nodepvar label wide nopar se ///
	title("`ttitle'") ///
	modelwidth(10) varwidth(30) b(2) se(2) ///
	stats(r2 N_clust N, labels("R-squared" "Number of brands" "Number of observations") fmt(2 0 0)) ///
	star(* 0.10 ** 0.05 *** .01) replace ///
	order(f2_pc1_std f2_pc1_stdXcat_socdemon f2_pc1_stdXcat_hedonic f2_pc1_stdXcat_perfrisk f2_pc1_stdXc4 ///
		  f2_pc1_stdXcat_fsocdemon f2_pc1_stdXcat_fhedonic f2_pc1_stdXcat_fperfrisk ///
		  f2_pc2_std f2_pc2_stdXcat_socdemon f2_pc2_stdXcat_hedonic f2_pc2_stdXcat_perfrisk  f2_pc2_stdXc4 ///
		  f2_pc2_stdXcat_fsocdemon f2_pc2_stdXcat_fhedonic f2_pc2_stdXcat_fperfrisk ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

end

program equity_sensitivity_7june
	syntax, ttitle(string) [dv(varlist)]
	eststo clear
	if "`dv'"=="" local dv sbbe_std
	
	eststo r1: quietly reg `dv' f2_pc1_std f2_pc2_std ///
									  [pw=weights], vce(cluster cat_brand_num)
	eststo r2: quietly reg `dv' f2_pc1_std f2_pc2_std f2_pc1_stdXf2_pc2_std ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo r3: quietly reg `dv' f2_pc1_std f2_pc2_std seccat ///
									  [pw=weights], vce(cluster cat_brand_num)
									  
	
	eststo r4: quietly reg `dv' f2_pc1_std f2_pc2_std f2_pc1_stdXf2_pc2_std seccat ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo r5: quietly reg `dv' bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo r6: quietly reg `dv' bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std seccat ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo r7: quietly reg `dv' seccat cat_socdemon cat_hedonic cat_perfrisk c4 ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	capture erase "$rtf_out"

	esttab r* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") onecell ///
	b(a2) compress ///
	modelwidth(5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5) varwidth(18) nogap ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)) ///
	star(* 0.10 ** 0.05 *** .01) replace ///
	order(f2_pc1_std f2_pc2_std f2_pc1_stdXf2_pc2_std seccat bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std)
	
end

program run_analysis
	syntax, path(string)
	
	local equityfn = "`path'\equity.csv"
	local elastfn = "`path'\elasticities.csv"
	
	* SBBE regression *
	global rtf_out "`path'\sbbe_regression.rtf"
	insheet using "`equityfn'", clear 
	do_preclean
	gen weights = 1/sbbe_se_std
	equity_final, ttitle("Regression of SBBE on CBBE Principal Components and Category Moderators")
	
	sort cat_name
	by cat_name: summ sbbe_std f2_*

	* Marketing Mix Elasticities
	elasticity, fn("`elastfn'") elast_vars(f2_pc1_std f2_pc2_std) ttitle("Regression of Marketing Mix Elasticities on CBBE Principal Components")
	
	sort cat_name
	by cat_name: summ elast_std f2_*

	* SBBE regression (robustness excluding new brands) *
	global rtf_out "`path'\sbbe_regression_newbrand.rtf"
	insheet using "`equityfn'", clear 
	do_preclean
	drop if upd_new == 1
	drop if upd_seccatandnew == 1
	gen weights = 1/sbbe_se_std
	equity_final, ttitle("Regression of SBBE on CBBE Principal Components and Category Moderators, excluding new brands")
	
	* Other robustness checks *
	
	global rtf_out "`path'\other_robustness.rtf"
	insheet using "`equityfn'", clear 
	do_preclean
	gen weights = 1/sbbe_se_std
	
	equity_sensitivity_7june, ttitle("Equity robustness checks (including all BAV brands")
	
	elasticity, fn("`elastfn'") elast_vars(bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std)

	elasticity, fn("`elastfn'") elast_vars(f2_pc2_std f2_pc2_stdXcat_socdemon f2_pc2_stdXcat_hedonic f2_pc2_stdXcat_perfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_socdemon f2_pc1_stdXcat_hedonic f2_pc1_stdXcat_perfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

	elasticity, fn("`elastfn'") elast_vars(f2_pc2_std f2_pc2_stdXcat_fsocdemon f2_pc2_stdXcat_fhedonic f2_pc2_stdXcat_fperfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_fsocdemon f2_pc1_stdXcat_fhedonic f2_pc1_stdXcat_fperfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

	* Other robustness checks excluding new brands
	global rtf_out "`path'\other_robustness_newbrands.rtf"
	insheet using "`equityfn'", clear 
	do_preclean
	drop if upd_new == 1
	drop if upd_seccatandnew == 1
	gen weights = 1/sbbe_se_std
	
	equity_sensitivity_7june, ttitle("Equity robustness checks (excluding new brands")
	
	elasticity, fn("`elastfn'") elast_vars(bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std)

	elasticity, fn("`elastfn'") elast_vars(f2_pc2_std f2_pc2_stdXcat_socdemon f2_pc2_stdXcat_hedonic f2_pc2_stdXcat_perfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_socdemon f2_pc1_stdXcat_hedonic f2_pc1_stdXcat_perfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)
	
	elasticity, fn("`elastfn'") elast_vars(f2_pc2_std f2_pc2_stdXcat_fsocdemon f2_pc2_stdXcat_fhedonic f2_pc2_stdXcat_fperfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_fsocdemon f2_pc1_stdXcat_fhedonic f2_pc1_stdXcat_fperfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

	* Regression on market share*
	global rtf_out "`path'\ms_regression.rtf"
	insheet using "`equityfn'", clear 
	do_preclean
	gen weights = 1 
	equity_final, ttitle("Regression of Marketshares on CBBE Principal Components and Category Moderators (no weights)") dv(annual_avgms_std)
	
	sort cat_name
	by cat_name: summ sbbe_std f2_*


	* Other robustness checks on market share *
	
	global rtf_out "`path'\other_robustness_ms.rtf"
	insheet using "`equityfn'", clear 
	do_preclean
	gen weights = 1
	
	equity_sensitivity_7june, ttitle("Regression of Marketshares on CBBE (Principal) Components") dv(annual_avgms_std)
	
end


program go
	local path = "$path\$curr_modelname\"
	local equityfn = "`path'\equity.csv"
	insheet using "`equityfn'", clear 
	drop if f_relestknow == .
end


program main
	run_analysis, path("$path\$curr_modelname\")
end

program spotlight_equity
	local path = "$path\$curr_modelname\"
	local equityfn = "`path'\equity.csv"
	
	insheet using "`equityfn'", clear 

	do_preclean
	
	* Equity
	gen weights = 1/sbbe_se_std
	
	reg sbbe_std f2_pc1_std f2_pc2_std ///
		seccat ///
		c4_mc c.f2_pc1_std#c.c4_mc c.f2_pc2_std#c.c4_mc ///
		cat_hedonic_mc c.f2_pc1_std#c.cat_hedonic_mc c.f2_pc2_std#c.cat_hedonic_mc ///
		cat_perfrisk_mc c.f2_pc1_std#c.cat_perfrisk_mc c.f2_pc2_std#c.cat_perfrisk_mc ///
		cat_socdemon_mc c.f2_pc1_std#c.cat_socdemon_mc c.f2_pc2_std#c.cat_socdemon_mc ///
		[pw=weights], vce(cluster cat_brand_num)

	local vlist c4_mc cat_hedonic_mc cat_perfrisk_mc cat_socdemon_mc
	
	foreach selvar in `vlist' {
		*local selvar c4_mc
		display 
		* compute category-specific percentiles, store them
		capture drop tag
		egen tag=tag(cat_name)
		quietly summarize `selvar' if tag == 1, detail
		*summarize `selvar', detail
		*_pctile(`selvar') if tag == 1, nq(10)
		quietly return list
		
		display "`selvar' : Relevant Stature"
				
		margins, expression(predict() -_b[_cons]) at((zero) _all `selvar'=(`r(p10)' `r(p90)') f2_pc1_std=(1))
		*marginsplot, recast(bar) noci
		
		*(asobserved) 
		display "`selvar' : Energized Differentation"
		
		quietly summarize `selvar' if tag == 1, detail
		quietly return list
		
		margins, expression(predict() -_b[_cons]) at((zero) _all `selvar'=(`r(p10)' `r(p90)') f2_pc2_std=(1))
		
		*marginsplot, recast(bar) noci
		*return list
		*twoway bar `r(at)' `r(b)'
		*matrix bdat = r(b)'
		*matrix rownames bdat = "low" "high"
		*clear
		*svmat bdat, names(bdat)

		*graph bar bdat1, over(bdat4)

		*matrix pred r(at) r(b)'
		
		*marginsplot, recast(bar)
		*, saving(`selvar'.gph, replace)
		*graph export `selvar'.png, replace

		}
	
end


program spotlight_elast
	local path = "$path\$curr_modelname\"
	local equityfn = "`path'\elasticities.csv"
	local elast_vars f2_pc1_std f2_pc2_std
	
	local vlist c4_mc cat_hedonic_mc cat_perfrisk_mc cat_socdemon_mc
	
	file open myfile using "sim_elast.txt", write replace
	set more off

	file write myfile  "variable" _tab "bav_factor" _tab "sim_value" _tab "prediction" _tab "N" _n ///
				  
	foreach val in rreg_pr_bt pi_bt fd_bt pct_store_skus_bt adstock_bt {
		
		display "`val'"
		*local val adstock_bt
		
		quietly load_elasticity, fn("`equityfn'")
		keep if var_name=="`val'"
		quietly do_preclean
		
		* perform regression (used to predict later on)
		reg elast_std `elast_vars' [pw=weights] 

		* add category-specific means and SDs of elasticities to data set (used to retrieve elasticities from standardized elasticities)
		capture drop mean_elast sd_elast
		egen mean_elast = mean(elast) if bav_brand==1, by(cat_name) 
		egen sd_elast = sd(elast) if bav_brand==1, by(cat_name) 
		
		foreach predfact in f2_pc1_std f2_pc2_std { 
		
			capture drop `predfact'_back
			g `predfact'_back = `predfact'
		
			foreach predval in 0 10 90 {
				capture drop `predfact'
				
				quietly summarize `predfact'_back, detail
				quietly return list
			
				if `predval' == 10 gen `predfact' = `r(p10)'
				if `predval' == 90 gen `predfact' = `r(p90)'
				if `predval' == 0 gen `predfact' = `predfact'_back
				
				quietly predict pred_elast_std_`predval'
				quietly predict pred_elast_std_`predval'_se, stdp 
			 
				g pred_elast_`predfact'_`predval' = pred_elast_std_`predval'*sd_elast + mean_elast
				g pred_elast_`predfact'_`predval'_se = pred_elast_std_`predval'_se * sd_elast
				capture drop tmp_weight 
				g tmp_weight = 1/pred_elast_`predfact'_`predval'_se
			 
				quietly summarize pred_elast_`predfact'_`predval' [w=tmp_weight], detail 
				
				g w_elast_`predfact'_`predval' = r(mean)
				
				file write myfile %9s "`val'" _tab ///
								  %9s "`predfact'" _tab ///
								  %9s "`predval'" _tab /// 
								  %12.0g (r(mean)) _tab ///
								  (r(N)) _n
		  
				capture drop pred_elast_std_*
				
				* reset factor value
				replace `predfact' = `predfact'_back
				}
				
				
	}
	tabstat w_elast_* , stat(n mean sd min max) col(stat) varwidth(16)
	
	}
	
	file close myfile
	set more on
	
end


main
spotlight_elast

log close
