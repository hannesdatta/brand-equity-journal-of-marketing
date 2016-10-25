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
global path = "c:\Users\hanne\Dropbox\Tilburg\Projects\BAV\Shared\analysis_round2\analysis\output\"
*global path = "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_round2\analysis\output\"


**** EXECUTION ****
cap cd "$path"
log using stata_log.txt, replace

global equityfn = "$path\$curr_modelname\equity.csv"
global elastfn = "$path\$curr_modelname\elasticities.csv"
global sim_equityfn = "$path\$curr_modelname\sim_equity.csv"
global sim_elastfn = "$path\$curr_modelname\sim_elast.csv"
global rob_pca_eq = "$path\$curr_modelname\rob_pca_eq.csv"
global rob_pca_elast = "$path\$curr_modelname\rob_pca_elast.csv"
	
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
		local l: variable label `var'
		label var f2_pc1_stdX`var' "     x `l'"
		label var f2_pc2_stdX`var' "     x `l'"
		}
		
	* Interactions for non-meancentered variables
	foreach var in `otherinteract_vars' {
		g f2_pc1_stdX`var' = f2_pc1_std * `var'
		g f2_pc2_stdX`var' = f2_pc2_std * `var'
		local l: variable label `var'
		label var f2_pc1_stdX`var' "     x `l'"
		label var f2_pc2_stdX`var' "     x `l'"
		}
		
	label var f2_pc1_std "Principal Component for Relevant Stature"
	label var f2_pc2_std "Principal Component for Energized Differentiation"
	
	* Compute category-means of non-standardized factors
	sort cat_name
	foreach bav in f2_pc1 f2_pc2 {
		by cat_name: egen `bav'_cmean=mean(`bav')
		by cat_name: egen `bav'_cstd=sd(`bav')
		}
	
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

	/*
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
		 */
		 
		 
	esttab m* using "$rtf_out", nodepvar label ///
	cells("b(fmt(2) star) p(fmt(3))" se(par fmt(2))) /// 
	title("`ttitle'") ///
	modelwidth(10) varwidth(30) ///
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
*spotlight_equity
*spotlight_elast

*plot_elast
*mean_elast



/* =================================================================
                   R O B U S T N E S S   C H E C K
			  
	  SIMULATION OF IMPACT OF INDIVIDUAL BAV DIMENSIONS ON SBBE 
   =================================================================
*/

insheet using "$equityfn", clear 
do_preclean
gen weights = 1/sbbe_se_std

* Estimate model
local vars f2_pc1_std f2_pc2_std ///
		   seccat ///
		   c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
		   cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
		   cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
		   cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon
									  
reg sbbe_std `vars' [pw=weights], vce(cluster cat_brand_num)

* Save coefficients and variance-covariance matrix
matrix V = e(V)
matrix b = e(b)
matrix B = b[1,1..15]
local nsim 1000

* Sample from covariance matrix, save draws
preserve
drop `vars'
set seed 04071984
drawnorm `vars', cov(V) means(B) cstorage(full) n(`nsim') clear
mkmat `vars', matrix(draws)
restore
	
* Re-compute factor scores, whereby one BAV dimension is going to be shocked by category-level
* standard deviations, and new factor values are computed

* (1) Compute category-level SDs for the original (standardized) BAV values
cap drop unique
egen unique = tag(brand_name year)
foreach var in bav_relevance bav_esteem bav_knowledge bav_energizeddiff {
	cap drop `var'_globalstd
	cap drop `var'_catsd
	cap drop tmp
	* generate standardized CBBE score
	egen tmp = std(`var') if unique == 1
	* write the standardized CBBE score to remaining, duplicate brand-year observations (e.g., in other categories)
	bysort brand_name year: egen `var'_globalstd = total(tmp)
	* generate category-specific SDs for standardized factor scores
	bysort cat_name: egen `var'_catsd = sd(`var'_globalstd)
	}

* (2) Compute shocked factors

/*
Matrix to compute component scores from standardized input matrix (and vice versa):
                          PC1         PC2
bav_relevance      0.36218281  0.09216399
bav_esteem         0.37428621  0.02197208
bav_knowledge      0.35427477 -0.14749174
bav_energizeddiff -0.03687092  0.97497004
*/

* Verifies whether factor scores can be retrieved correctly from the standardized CBBE dimensions

*cap drop newfac1 newfac2
*gen newfac1 = 0.36218281 * bav_relevance_globalstd + 0.37428621 * bav_esteem_globalstd + 0.35427477 * bav_knowledge_globalstd -0.03687092 * bav_energizeddiff_globalstd
*gen newfac2 = 0.09216399 * bav_relevance_globalstd + 0.02197208 * bav_esteem_globalstd  -0.14749174 * bav_knowledge_globalstd + 0.97497004 * bav_energizeddiff_globalstd

* Do the average standard deviation within a category
cap drop fac*_catsd
by cat_name: egen fac1_catsd = sd(f2_pc1)
by cat_name: egen fac2_catsd = sd(f2_pc2)
by cat_name: egen fac1_catmean = mean(f2_pc1)
by cat_name: egen fac2_catmean = mean(f2_pc2)


* compute shocked factor scores
foreach var in bav_relevance bav_esteem bav_knowledge bav_energizeddiff {
	cap drop bav*_shock
	gen bav_relevance_shock = bav_relevance_globalstd
	gen bav_esteem_shock = bav_esteem_globalstd
	gen bav_knowledge_shock = bav_knowledge_globalstd
	gen bav_energizeddiff_shock = bav_energizeddiff_globalstd
	
	replace `var'_shock = `var'_shock + `var'_catsd
	
	cap drop newfac1_`var'_s
	cap drpo newfac2_`var'_s
	
	gen newfac1_`var'_s = (0.36218281 * bav_relevance_shock + 0.37428621 * bav_esteem_shock + 0.35427477 * bav_knowledge_shock -0.03687092 * bav_energizeddiff_shock - fac1_catmean) / fac1_catsd
	gen newfac2_`var'_s = (0.09216399 * bav_relevance_shock + 0.02197208 * bav_esteem_shock  -0.14749174 * bav_knowledge_shock + 0.97497004 * bav_energizeddiff_shock - fac2_catmean) / fac2_catsd

	}

* define colMeans (and then means across the column means) in Mata

mata
mata clear
void do_mata() {
		A = st_matrix("sim")
		avgs = colsum(A)/rows(A)
		M = mean(avgs')
		SD = sqrt(variance(avgs'))
		st_numscalar("M",M)
		st_numscalar("SD",SD)
	}
end

* Open file for output writing
file open myfile using "$rob_pca_eq", write replace
set more off
file write myfile  "variable" _tab "moderator" _tab "coef" _tab "se" _n

* Define variables for which to compute effects
local vars main Xc4 Xcat_hedonic Xcat_perfrisk Xcat_socdemon 

* Perform calculation
foreach var of local vars {
	display "Computing effects for: `var'"
	if "`var'"=="main" local var ""
	
	matrix coef = draws[1..`nsim', "f2_pc1_std`var'".."f2_pc2_std`var'"]
	
	foreach bavvar in bav_relevance bav_esteem bav_knowledge bav_energizeddiff {
		* Impact of a dimension is equal to the differences in factor values for "shocked" factor (whereby one dimension is shocked) and baseline factor value...
		cap drop comp_diff*
		g comp_diff_fac1 = newfac1_`bavvar'_s - f2_pc1_std
		g comp_diff_fac2 = newfac2_`bavvar'_s - f2_pc2_std

		* ...times the coefficient draws for the relevant coefficients
		mkmat comp_diff*, matrix(compdiff)
		matrix sim = compdiff * coef' 
		
		* calculate means across observations per draw, and then average
		mata: do_mata()
		
		file write myfile %9s "`bavvar'" _tab %9s "`var'" _tab %12.0g (M) _tab %12.0g (SD) _n 

		}
	}
* Close output file	
file close myfile
set more on

* End of simulation



/* =======================================================================
                   R O B U S T N E S S   C H E C K
			  
	  SIMULATION OF IMPACT OF INDIVIDUAL BAV DIMENSIONS ON ELASTICITIES 
   =======================================================================
*/

insheet using "$elastfn", clear 
drop if f2_pc1_std == . | elast_std == .
gen weights = 1/elast_se_std


* (1) Compute category-level SDs for the original (standardized) BAV values
cap drop unique
egen unique = tag(brand_name)
foreach var in bav_relevance bav_esteem bav_knowledge bav_energizeddiff {
	cap drop `var'_globalstd
	cap drop `var'_catsd
	cap drop tmp
	* generate standardized CBBE score
	egen tmp = std(`var') if unique == 1
	* write the standardized CBBE score to remaining, duplicate brand-year observations (e.g., in other categories)
	bysort brand_name: egen `var'_globalstd = total(tmp)
	* generate category-specific SDs for standardized factor scores
	bysort cat_name: egen `var'_catsd = sd(`var'_globalstd)
	}
	
* (2) Compute shocked factors

/*
Matrix to compute component scores from standardized input matrix (and vice versa):
                          PC1         PC2
bav_relevance      0.363712677  0.11845426
bav_esteem         0.374534329  0.01896211
bav_knowledge      0.345043543 -0.15816508
bav_energizeddiff -0.008083056  0.96559481

*/

*Assert whether factor scores can be correctly computed from the standardized CBBE dimensions
*cap drop newfac1 newfac2
*gen newfac1 = 0.363712677 * bav_relevance_globalstd + 0.374534329 * bav_esteem_globalstd + 0.345043543 * bav_knowledge_globalstd -0.008083056 * bav_energizeddiff_globalstd
*gen newfac2 = 0.11845426 * bav_relevance_globalstd + 0.01896211 * bav_esteem_globalstd  -0.15816508 * bav_knowledge_globalstd + 0.96559481 * bav_energizeddiff_globalstd


* Compute means/SDs for standardization of factor scores by category (on the original factors)
cap drop fac*_catsd
by cat_name: egen fac1_catsd = sd(f2_pc1)
by cat_name: egen fac2_catsd = sd(f2_pc2)
by cat_name: egen fac1_catmean = mean(f2_pc1)
by cat_name: egen fac2_catmean = mean(f2_pc2)

* Compute shocked factor scores
foreach var in bav_relevance bav_esteem bav_knowledge bav_energizeddiff {
	cap drop bav*_shock
	gen bav_relevance_shock = bav_relevance_globalstd
	gen bav_esteem_shock = bav_esteem_globalstd
	gen bav_knowledge_shock = bav_knowledge_globalstd
	gen bav_energizeddiff_shock = bav_energizeddiff_globalstd
	
	replace `var'_shock = `var'_shock + `var'_catsd
	
	cap drop newfac1_`var'_s
	cap drpo newfac2_`var'_s
	
	gen newfac1_`var'_s = (0.363712677 * bav_relevance_shock + 0.374534329 * bav_esteem_shock + 0.345043543 * bav_knowledge_shock -0.008083056 * bav_energizeddiff_shock - fac1_catmean) / fac1_catsd
	gen newfac2_`var'_s = (0.11845426 * bav_relevance_shock + 0.01896211 * bav_esteem_shock  -0.15816508 * bav_knowledge_shock + 0.96559481 * bav_energizeddiff_shock - fac2_catmean) / fac2_catsd

	}


* define colMeans (and then means across the column means) in Mata
mata
mata clear
void do_mata() {
		A = st_matrix("sim")
		avgs = colsum(A)/rows(A)
		M = mean(avgs')
		SD = sqrt(variance(avgs'))
		st_numscalar("M",M)
		st_numscalar("SD",SD)
	}
end

* Open file for output writing
	file open myfile using "$rob_pca_elast", write replace
	set more off
	file write myfile "elast_var" _tab "variable" _tab "coef" _tab "se" _n

* Loop over DVs (vars)
foreach var in rreg_pr_bt pi_bt fd_bt pct_store_skus_bt adstock_bt {
	preserve
	keep if var_name == "`var'"
	reg elast_std f2_pc1_std f2_pc2_std [pw=weights]
	restore
	
	* Save coefficients and variance-covariance matrix
	matrix V = e(V)
	matrix b = e(b)
	matrix B = b[1,1..2]
	local nsim 1000

	* Sample from covariance matrix, save draws
	preserve
	local vars f2_pc1_std f2_pc2_std
	drop `vars'
	set seed 04071984
	drawnorm `vars', cov(V) means(B) cstorage(full) n(`nsim') clear
	mkmat `vars', matrix(draws)
	restore
	
	matrix coef = draws
	
	foreach bavvar in bav_relevance bav_esteem bav_knowledge bav_energizeddiff {
		* Impact of a dimension is equal to the differences in factor values for "shocked" factor (whereby one dimension is shocked) and baseline factor value...
		cap drop comp_diff*
		g comp_diff_fac1 = newfac1_`bavvar'_s - f2_pc1_std
		g comp_diff_fac2 = newfac2_`bavvar'_s - f2_pc2_std

		* ...times the coefficient draws for the relevant coefficients
		mkmat comp_diff*, matrix(compdiff)
		matrix sim = compdiff * coef' 
		
		* calculate means across observations per draw, and then average
		mata: do_mata()
		
		file write myfile %9s "`var'" _tab %9s "`bavvar'" _tab %12.0g (M) _tab %12.0g (SD) _n 

		}

	}
* Close output file	
file close myfile
set more on	

	
			

* End of simulation



/*
/* ELASTICITIES 
*/

file open myfile using "$rob_pca_eq", write replace
set more off
file write myfile  "variable" _tab "moderator" _tab "coef" _tab "sd" _tab "t" _tab "pval" _n

local vars main Xc4 Xcat_hedonic Xcat_perfrisk Xcat_socdemon 
* INTERACTION EFFECTS
foreach var of local vars {
	if "`var'"=="main" local var ""
		
	* VAR x Relevance
	lincom 0.36218281 * f2_pc1_std`var' + 0.09216399 * f2_pc2_std`var'
	
	file write myfile %9s "relevance" _tab %9s "`var'" _tab
	return list
	file write myfile %12.0g (r(estimate)) _tab %12.0g (r(se)) _n 
	
	* VAR x Esteem
	lincom 0.37428621 * f2_pc1_std`var' + 0.02197208 * f2_pc2_std`var'
	
	file write myfile %9s "esteem" _tab %9s "`var'" _tab
	return list
	file write myfile %12.0g (r(estimate)) _tab %12.0g (r(se)) _n 
	
	* VAR x Knowledge
	lincom 0.35427477 * f2_pc1_std`var' -0.14749174 * f2_pc2_std`var'
	
	file write myfile %9s "knowledge" _tab %9s "`var'" _tab
	return list
	file write myfile %12.0g (r(estimate)) _tab %12.0g (r(se)) _n 
	
	* VAR x Energized Diff								 
	lincom -0.03687092 * f2_pc1_std`var' + 0.97497004 * f2_pc2_std`var'

	file write myfile %9s "energizeddiff" _tab %9s "`var'" _tab
	return list
	file write myfile %12.0g (r(estimate)) _tab %12.0g (r(se)) _n 
	
	}

file close myfile
set more on


	
	local elast_dv elast_std
	
	if "`ttitle'"=="" local ttitle = "Elasticities"
	
	eststo clear
	

*/
