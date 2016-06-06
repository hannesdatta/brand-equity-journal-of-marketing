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

global path = "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"


program do_preclean
	* Kick out non-BAV brands
	drop if bav_brand == 0
	
	* Generate clustering variables (cat_name + brand_name)
	generate cat_brand = cat_name+ "_" +brand_name
	egen cat_brand_num = group(cat_brand)

	g estbrand = 1-newbrnd
	
	* Label variables
	local mcvars c2 c3 c4 herf catgrowth_rel catgrowth_abs cat_invol cat_hedonic cat_utilit cat_perfrisk cat_socdemon cat_muchtolose
	local otherinteract_vars seccat newbrnd estbrand fmcg_seccat retail_seccat food_drink_cigs

	label var seccat "Brand extension"
	label var retail_seccat "Retail chain second. cat."
	label var fmcg_seccat "FMCG second. cat."
	label var food_drink_cigs "Food, drink and cigs"
	
	label var newbrnd "New brand"
	label var estbrand "Established brand"
	
	label var c2 "C2"
	label var c3 "C3"
	label var c4 "C4"
	label var herf "Herfindahl"
	label var catgrowth_rel "Category growth rel."
	label var catgrowth_abs "Category growth abs."
	label var cat_invol "Category involvement"
	label var cat_hedonic "Category Hedonic"
	label var cat_utilit "Category Utilitarianism"
	label var cat_perfrisk "Category performance risk"
	label var cat_socdemon "Category Social Demonstrance"
	label var cat_muchtolose "Category Much to Lose"
	
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
		label var f2_pc1_stdX`var' "Relevant Stature x `l'"
		label var f2_pc2_stdX`var' "Energized Diff. x `l'"
		*label var f_pc3_stdX`var' "third factor x `l'"
		}
		
	* Interactions for non-meancentered variables
	foreach var in `otherinteract_vars' {
		g f2_pc1_stdX`var' = f2_pc1_std * `var'
		g f2_pc2_stdX`var' = f2_pc2_std * `var'
		g f3_pc1_stdX`var' = f3_pc1_std * `var'
		g f3_pc2_stdX`var' = f3_pc2_std * `var'
		g f3_pc3_stdX`var' = f3_pc3_std * `var'
		local l: variable label `var'
		label var f2_pc1_stdX`var' "Relevant Stature x `l'"
		label var f2_pc2_stdX`var' "Energized Diff. x `l'"
		*label var f_pc3_stdX`var' "third factor x `l'"
		}
		
	label var f2_pc1_std "Relevant Stature"
	label var f2_pc2_std "Energized Differentiation "
	
	g f2_pc1_std_sq = f2_pc1_std ^ 2
	g f2_pc2_std_sq = f2_pc2_std ^ 2
	label var f2_pc1_std_sq "Relevant Stature squared"
	label var f2_pc2_std_sq "Energized Diff. squared"
	
	g f2_pc1_stdXf2_pc2_std = f2_pc1_std*f2_pc2_std
	
	label var f2_pc1_stdXf2_pc2_std "Relevant Stature x Energized Diff."
	
	g f2_pc1_stdXf2_pc2_stdXestbrand = f2_pc1_stdXf2_pc2_std * estbrand
	*label var f2_pc1_stdXf2_pc2_std "Established Relevant Stature x Energized Diff."
	
end

program load_elasticity
	syntax, fn(string) 
	insheet using "`fn'", clear 
	drop if f2_pc1_std == . | elast_std == .
	gen weights = 1/elast_se_std
	
end


program elasticity
	syntax, fn(string) elast_vars(varlist)
	
	eststo clear
	
	/* rreg_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="rreg_pr_bt"
	do_preclean
	eststo m1: quietly reg elast_std `elast_vars' [pw=weights] 

	/* pi_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pi_bt"
	do_preclean
	eststo m2: quietly reg elast_std `elast_vars' [pw=weights] 

	/* fd_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="fd_bt"
	do_preclean
	eststo m3: quietly reg elast_std `elast_vars' [pw=weights] 

	/* pct_store_skus_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pct_store_skus_bt"
	do_preclean
	eststo m4: quietly reg elast_std `elast_vars' [pw=weights]

	/* ad */
	load_elasticity, fn("`fn'")
	keep if var_name=="adstock_bt"
	do_preclean
	eststo m5: quietly reg elast_std `elast_vars' [pw=weights] 
	
		
	* capture erase "$rtf_out" *append
	esttab m* using "$rtf_out", append mtitles("regular price" "price index" "feature/display" "total distribution" "adstock") nodepvar label ///
	   addnote("All estimated with WLS.") title("Elasticities, 1/elast_se_std as weights") modelwidth(8 8 8 8 8) varwidth(30) ///
	   stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)  fmt(a2 a2 3 0 0)) ///
	   onecell nogap star(* 0.10 ** 0.05 *** .01) replace b(a2)
				
end

program equity_final_old
	syntax, ttitle(string)
	eststo clear
		
	eststo mx1: quietly reg sbbe_std f2_pc1_std f2_pc2_std seccat estbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo mx2: quietly reg sbbe_std f2_pc1_std f2_pc2_std seccat estbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	estadd vif
	capture erase "$rtf_out"

	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") ///
	b(a2) compress ///
	modelwidth(5 5 5 5 5 5 5 5 5 5) varwidth(22) nogap ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)) ///
	star(* 0.10 ** 0.05 *** .01) replace ///
	order(f2_pc1_std f2_pc2_std seccat estbrand ///
		  c4 f2_pc1_stdXc4 f2_pc2_stdXc4  ///
		  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
		  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
		  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon)
	
	
	*fmt(a2 a2 3 0 0)) ///
	*onecell  
	* nogap 
	*aux(vif 2) wide nopar ///
	
end


program equity_final
	syntax, ttitle(string)
	eststo clear
		
	eststo m1: quietly reg sbbe_std f2_pc1_std f2_pc2_std seccat ///
									  estbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
									  
	eststo m1b: quietly reg sbbe_std f2_pc1_std f2_pc2_std seccat ///
									  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
				

	eststo m1c: quietly reg sbbe_std f2_pc1_std f2_pc2_std ///
									  seccat f2_pc1_stdXseccat f2_pc2_stdXseccat ///
									  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo m1d: quietly reg sbbe_std f2_pc1_std f2_pc2_std f2_pc1_stdXf2_pc2_std ///
									  seccat f2_pc1_stdXseccat f2_pc2_stdXseccat ///
									  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo m2: quietly reg sbbe_std f2_pc1_std f2_pc2_std seccat ///
									  estbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
									  
	eststo m2b: quietly reg sbbe_std f2_pc1_std f2_pc2_std seccat ///
									  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
				

	eststo m2c: quietly reg sbbe_std f2_pc1_std f2_pc2_std ///
									  seccat f2_pc1_stdXseccat f2_pc2_stdXseccat ///
									  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	
	eststo m2d: quietly reg sbbe_std f2_pc1_std f2_pc2_std f2_pc1_stdXf2_pc2_std ///
									  seccat f2_pc1_stdXseccat f2_pc2_stdXseccat ///
									  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
									  c4 f2_pc1_stdXc4 f2_pc2_stdXc4 ///
									  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
									  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
									  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)

	capture erase "$rtf_out"

	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") ///
	b(a2) compress ///
	modelwidth(5 5 5 5 5 5 5 5 5 5) varwidth(18) nogap ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)) ///
	star(* 0.10 ** 0.05 *** .01) replace ///
	order(f2_pc1_std f2_pc2_std f2_pc1_stdXf2_pc2_std ///
		  seccat f2_pc1_stdXseccat f2_pc2_stdXseccat ///
		  estbrand f2_pc1_stdXestbrand f2_pc2_stdXestbrand ///
		  c4 f2_pc1_stdXc4 f2_pc2_stdXc4  ///
		  cat_hedonic f2_pc1_stdXcat_hedonic f2_pc2_stdXcat_hedonic ///
		  cat_perfrisk f2_pc1_stdXcat_perfrisk f2_pc2_stdXcat_perfrisk ///
		  cat_socdemon f2_pc1_stdXcat_socdemon f2_pc2_stdXcat_socdemon)
	
	
	*fmt(a2 a2 3 0 0)) ///
	*onecell  
	* nogap 
	*aux(vif 2) wide nopar ///
	
end

program run_analysis
	syntax, path(string)
	
	local equityfn = "`path'\equity.csv"
	local elastfn = "`path'\elasticities.csv"
	
	global rtf_out "`path'\stata_equity_23may2016.rtf"
	
	insheet using "`equityfn'", clear 

	do_preclean
	
	* Equity
	gen weights = 1/sbbe_se_std
	equity_final, ttitle("Equity regressions, 23 May 2016")
	
	* Elasticities
	elasticity, fn("`elastfn'") elast_vars(f2_pc1_std f2_pc2_std)
		
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

main
