./**********************************************************

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

global curr_modelname = "MNL_copula_5mmix_nomc"
global path = "c:\Users\hanne\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"
*global path = "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"




**** EXECUTION ****
cd "$path"
log using stata_log.txt, replace

global equityfn = "$path\$curr_modelname\equity.csv"
global elastfn = "$path\$curr_modelname\elasticities.csv"
global sim_equityfn = "$path\$curr_modelname\sim_equity.csv"
global sim_elastfn = "$path\$curr_modelname\sim_elast.csv"
	
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
	insheet using "$elastfn", clear 
	drop if f2_pc1_std == . | elast_std == .
	gen weights = 1/elast_se_std
	
end


program elasticity
	syntax, elast_vars(varlist) [ttitle(string) do_append(real 1)]
	
	local elast_dv elast_std
	
	if "`ttitle'"=="" local ttitle = "Elasticities"
	
	eststo clear
	
	local vars rreg_pr_bt pi_bt fd_bt pct_store_skus_bt adstock_bt
	
	local count_var : word count `vars'
	display "`count_var'"
	
	* Loop over DVs (vars)
		forvalues i = 1/`count_var' {
			local m : word `i' of `vars'
			load_elasticity, fn("$elastfn")
			keep if var_name == "`m'"
			do_preclean
			sort cat_name
			by cat_name: summ `elast_dv'
			
			eststo m`i': quietly reg `elast_dv' `elast_vars' [pw=weights] 

			}
					
	esttab m* using "$rtf_out", append ///
	   mtitles("Regular Price Elasticity" "Promotional Price Elasticity" "Feature / Display Response" ///
			   "Distribution Elasticity" "Advertising Elasticity") ///
	   nodepvar label ///
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
	* SBBE regression *
	global rtf_out "$path\$curr_modelname\sbbe_regression.rtf"
	insheet using "$equityfn", clear 
	do_preclean
	gen weights = 1/sbbe_se_std
	equity_final, ttitle("Regression of SBBE on CBBE Principal Components and Category Moderators")
	sort cat_name
	by cat_name: summ sbbe_std f2_*

	* Marketing Mix Elasticities
	elasticity, elast_vars(f2_pc1_std f2_pc2_std) ttitle("Regression of Marketing Mix Elasticities on CBBE Principal Components")
	
	sort cat_name
	by cat_name: summ elast_std f2_*

	* SBBE regression (robustness excluding new brands) *
	global rtf_out "$path\$curr_modelname\sbbe_regression_newbrand.rtf"
	insheet using "$equityfn", clear 
	do_preclean
	drop if upd_new == 1
	drop if upd_seccatandnew == 1
	gen weights = 1/sbbe_se_std
	equity_final, ttitle("Regression of SBBE on CBBE Principal Components and Category Moderators, excluding new brands")
	
	* Other robustness checks *
	
	global rtf_out "$path\$curr_modelname\other_robustness.rtf"
	insheet using "$equityfn", clear 
	do_preclean
	gen weights = 1/sbbe_se_std
	
	equity_sensitivity_7june, ttitle("Equity robustness checks (including all BAV brands")
	
	elasticity, elast_vars(bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std)

	elasticity, elast_vars(f2_pc2_std f2_pc2_stdXcat_socdemon f2_pc2_stdXcat_hedonic f2_pc2_stdXcat_perfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_socdemon f2_pc1_stdXcat_hedonic f2_pc1_stdXcat_perfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

	elasticity, elast_vars(f2_pc2_std f2_pc2_stdXcat_fsocdemon f2_pc2_stdXcat_fhedonic f2_pc2_stdXcat_fperfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_fsocdemon f2_pc1_stdXcat_fhedonic f2_pc1_stdXcat_fperfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

	* Other robustness checks excluding new brands
	global rtf_out "$path\$curr_modelname\other_robustness_newbrands.rtf"
	insheet using "$equityfn", clear 
	do_preclean
	drop if upd_new == 1
	drop if upd_seccatandnew == 1
	gen weights = 1/sbbe_se_std
	
	equity_sensitivity_7june, ttitle("Equity robustness checks (excluding new brands")
	
	elasticity, elast_vars(bav_relevance_std bav_esteem_std bav_knowledge_std bav_energizeddiff_std)

	elasticity, elast_vars(f2_pc2_std f2_pc2_stdXcat_socdemon f2_pc2_stdXcat_hedonic f2_pc2_stdXcat_perfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_socdemon f2_pc1_stdXcat_hedonic f2_pc1_stdXcat_perfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)
	
	elasticity, elast_vars(f2_pc2_std f2_pc2_stdXcat_fsocdemon f2_pc2_stdXcat_fhedonic f2_pc2_stdXcat_fperfrisk  f2_pc2_stdXc4 ///
		  f2_pc1_std f2_pc1_stdXcat_fsocdemon f2_pc1_stdXcat_fhedonic f2_pc1_stdXcat_fperfrisk f2_pc1_stdXc4 ///
		  seccat cat_socdemon cat_hedonic cat_perfrisk c4)

	* Regression on market share*
	global rtf_out "$path\$curr_modelname\ms_regression.rtf"
	insheet using "$equityfn", clear 
	do_preclean
	gen weights = 1 
	equity_final, ttitle("Regression of Marketshares on CBBE Principal Components and Category Moderators (no weights)") dv(annual_avgms_std)
	
	sort cat_name
	by cat_name: summ sbbe_std f2_*


	* Other robustness checks on market share *
	
	global rtf_out "$path\$curr_modelname\other_robustness_ms.rtf"
	insheet using "$equityfn", clear 
	do_preclean
	gen weights = 1
	
	equity_sensitivity_7june, ttitle("Regression of Marketshares on CBBE (Principal) Components") dv(annual_avgms_std)
	
end

program spotlight_equity
	insheet using "$equityfn", clear 

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

	matlist e(V)
	
	local vlist c4_mc cat_hedonic_mc cat_perfrisk_mc cat_socdemon_mc

	file open myfile using "$sim_equityfn", write replace
	set more off

	file write myfile  "pca" _tab "moderator" _tab "sim_value10p" _tab "sim_value90p" _tab "pred_10" _tab "pred_10se" _tab "pred_90" _tab "pred_90se" _n

	foreach selvar in `vlist' {
		*local selvar c4_mc
		* compute category-specific percentiles, store them
		capture drop tag
		egen tag=tag(cat_name)
		quietly summarize `selvar' if tag == 1, detail
		quietly return list
		local p10 = `r(p10)'
		local p90 = `r(p90)'
		
		*display "`selvar' : Relevant Stature"
		local facvars_loop f2_pc1_std f2_pc2_std 
		
		foreach pca_var in `facvars_loop' {
				
			file write myfile %9s "`pca_var'" _tab ///
							  %9s "`selvar'" _tab ///
							  %12.0g "`p10'" _tab ///
							  %12.0g "`p90'" _tab 
								  
			lincom `pca_var' + `p10'*c.`pca_var'#c.`selvar'
			return list
			
			file write myfile %12.0g (r(estimate)) _tab ///
							  %12.0g (r(se)) _tab 
			
			lincom `pca_var' + `p90'*c.`pca_var'#c.`selvar' 
			return list
			
			file write myfile %12.0g (r(estimate)) _tab ///
							  %12.0g (r(se)) _n
		  
		  
			}
		
		}
	
	file close myfile
	set more on
	
end


program weighted_mean, rclass
	syntax, var(varlist) weight(varlist)
 	quietly summarize `var' [w=`weight'], detail
	return scalar wmean = r(mean)
end

program test_elast

		local elast_vars f2_pc1_std f2_pc2_std
		local val adstock_bt
		quietly load_elasticity, fn("$equityfn")
		keep if var_name=="`val'"
		quietly do_preclean
		
		egen mean_elast = mean(elast), by(cat_name) 
		egen sd_elast = sd(elast), by(cat_name) 
		
		reg elast_std `elast_vars' [pw=weights] 

		
		weighted_mean, var(elast) weight(weights)
		return list
		
		lincom `r(wmean)' + f2_pc1_std * 10% * ...
		
		
		
		collapse (mean) elast (sum) weights, by (cat_name)
		
		
		collapse (mean) elast [weight=weights]
		
		lincom _cons + f2_pc1_std
		
		
		
		*reg elast_std [pw=weights] 
		cap drop pred
		predict pred
		predict pred_se, stdp
		g w2 = 1/pred_se
		
		summarize elast_std [w=weights] 

		
		g fpred = pred * sd_elast + mean_elast
		
		summarize elast [w=weights]
		summarize fpred [w=weights]
		summarize fpred [w=w2]
		summarize fpred

		summarize f2_pc2_std, detail
		return list	
			
		replace f2_pc2_std = `r(p10)'
		replace f2_pc1_std = 0
		
		* predict STANDARDIZED elasticities and standard errors
		quietly predict pred_elast_std_X
		quietly predict pred_elast_std_X_se, stdp 
		
		* DE-STANDARDIZE elasticities and standard errors
		g pred_elast_X1 = pred_elast_std_X*sd_elast + mean_elast
		g pred_elast_X1_se = pred_elast_std_X_se * sd_elast
		
		*replace pred_elast_X1 = elast
		*replace pred_elast_X1_se = elast_se
		
		*g pred_elast_X1 = pred_elast_std_X
		*g pred_elast_X1_se = pred_elast_std_X_se
		
		* weigh by inverse standard errors
		capture drop tmp_weight 
		g tmp_w = 1/pred_elast_X1_se
		egen tmp_wsum = total(tmp_w)
		g tmp_weight = tmp_w/tmp_wsum
		drop tmp_w tmp_wsum
		
		* compute weighted mean
		summarize pred_elast_X1 [w=tmp_weight], detail 

		set seed `=date("2015-05-03", "YMD")'
		bootstrap weighted_mean=r(wmean), reps(200) nodots nowarn: weighted_mean, var(pred_elast_X1) weight(tmp_weight)
				
	
end

program mean_elast
	insheet using "$elastfn", clear 
	gen weights = 1/elast_se
	sort var_name
	by var_name: summarize elast [weight=weights]
				
end

program spotlight_elast
	local elast_vars f2_pc1_std f2_pc2_std
	
	file open myfile using "$sim_elastfn", write replace
	set more off
	
	file write myfile  "variable" _tab "bav_factor" _tab "sim_value" _tab "pred" _tab "pred_se" _tab "N" _n ///
		  
	foreach val in rreg_pr_bt pi_bt fd_bt pct_store_skus_bt adstock_bt {
		
		*local val adstock_bt
		display "`val'"
		
		quietly load_elasticity, fn("$equityfn")
		keep if var_name=="`val'"
		quietly do_preclean
		
		* add category-specific means and SDs of elasticities to data set (used to retrieve elasticities from standardized elasticities)
		*capture drop mean_elast sd_elast
		*egen mean_elast = mean(elast), by(cat_name) 
		*egen sd_elast = sd(elast), by(cat_name) 
		
		foreach predfact in f2_pc1_std f2_pc2_std { 
			
			foreach predval in 10 90 {
				*local val adstock_bt
				*local predfact f2_pc1_std
				*local predval 10
				*local elast_vars f2_pc1_std f2_pc2_std
	
				preserve
				
				* calculate p10 and p90 for the PCA score, store as local variable
				quietly summarize `predfact', detail
				quietly return list
				if `predval' == 10 local percentile = `r(p10)'
				if `predval' == 90 local percentile = `r(p90)'
				
				* Get weighted elasticity (for BAV brands)
				summarize elast [weight=weights]
				return list
				local ewmean = `r(mean)'
				local esd = `r(sd)'
				
				* perform regression (to retrieve the interaction coefficient)
				reg elast_std `elast_vars' [pw=weights] 
				
				* calculate interaction effect
				lincom `ewmean' + `percentile'*`predfact'*`esd'
				return list
			
				file write myfile %9s "`val'" _tab ///
								  %9s "`predfact'" _tab ///
								  %9s "`predval'" _tab /// 
								  %12.0g (r(estimate)) _tab ///
								  %12.0g (r(se)) _tab ///
								  (r(N)) _n
		  
				restore
				}
				
				
	}
	
	}
	
	file close myfile
	set more on
	
end


log close


* test plotting
program plot_elast
	
	foreach bav in f2_pc1_std f2_pc2_std {
		
		foreach var in rreg_pr_bt pi_bt fd_bt pct_store_skus_bt adstock_bt {
	
			insheet using "$sim_elastfn", clear
		
			keep if bav_factor == "`bav'"
			drop if sim_value == 0
			keep if variable == "`var'"
				
			replace sim_value = 1 if sim_value == 10
			replace sim_value = 2.5 if sim_value == 90
			gen conf_hi = pred + 1.69 * pred_se
			gen conf_lo = pred - 1.69 * pred_se

			* labeling
			gen variable_label = " "
			replace variable_label = "Advertising" if variable == "adstock_bt"
			replace variable_label = "Feature/Display" if variable == "fd_bt"
			replace variable_label = "Promotional Price Index" if variable == "pi_bt"
			replace variable_label = "Regular Price" if variable == "rreg_pr_bt"
			replace variable_label = "Distribution" if variable == "pct_store_skus_bt"

			* retrieve variable label
			levelsof variable_label, local(levels)
			foreach l of local levels {
				local plot_label = "`l'"
				}

			* retrieve factor name
			levelsof bav_factor, local(levels)
			foreach l of local levels {
				if "`l'"=="f2_pc1_std" local factor_label = "Relevant Stature"
				if "`l'"=="f2_pc2_std" local factor_label = "Energized Differentiation"
				}

			twoway (bar pred sim_value) ///
				   (rcap conf_hi conf_lo sim_value), ///
				   xlabel(1 "Low `factor_label'" 2.5 "High `factor_label'") ///
				   xtitle("") ytitle("Elasticity") ///
				   title("`plot_label'") legend(off) ///
				   graphregion(fcolor(white) lstyle(none)) /// 
				   ysc(r(0)) ylabel(#6)
				   
			graph export $path\$curr_modelname\elast_`var'_`bav'.png, replace
		
		}
	}
	
end


run_analysis
spotlight_equity
spotlight_elast

plot_elast
mean_elast
