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


local mlist = "MNL_copula_5mmix "
local path = "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"

program equity
	syntax, fn(string)
	
	insheet using "`fn'", clear 
	*drop if bav_asset == .
	drop if f_relestknow_std == .
	
	generate cat_brand = cat_name+ "_" +brand_name
	egen cat_brand_num = group(cat_brand)
	gen weights = 1/sbbe_se_std
	
	meancenter_interact
		
	eststo clear
	
	xtset cat_brand_num
	
	eststo m1: quietly reg sbbe_std $vars, vce(cluster cat_brand_num)
	
	eststo m2: quietly reg sbbe_std $vars [pw=weights], vce(cluster cat_brand_num)
	
	eststo m3: quietly xtmixed sbbe_std $vars [pw=weights] || cat_brand_num:, mle vce(cluster cat_brand_num)
	
	eststo m4: quietly xtreg sbbe_std $vars, be
	eststo m5: quietly xtreg sbbe_std $vars, fe vce(cluster cat_brand_num)
	
	capture erase "$rtf_out"
	esttab m* using "$rtf_out", mtitles("OLS" "WLS" "RE" "Between (no weights)" "FE (no weights)") nodepvar noconstant label ///
	addnote("") title("Equity") modelwidth(5 5 5 5 5 5 5 5 5)  varwidth(22) ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations) fmt(a2 a2 3 0 0)) ///
	onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace b(a2)
	
end

program meancenter_interact
	local mcvars seccat newbrnd c2 c3 c4 herf catgrowth_rel catgrowth_abs

	label var seccat "Brand in second. cat."
	label var newbrnd "New brand"
	label var c2 "C2"
	label var c3 "C3"
	label var c4 "C4"
	label var herf "Herfindahl"
	label var catgrowth_rel "Category growth rel."
	label var catgrowth_abs "Category growth abs."
	
	* Mean-center variables
	foreach var in `mcvars' {
		quietly sum `var'
		g `var'_mc = `var' - r(mean)
		local l: variable label `var'
		label var `var'_mc "`l'"
		}
	
	* Interactions
	foreach var in `mcvars' {
		g f_relestknow_stdX`var' = f_relestknow_std * `var'_mc
		g f_energdiff_stdX`var' = f_energdiff_std * `var'_mc
		local l: variable label `var'
		label var f_relestknow_stdX`var' "RelEstKnow x `l'"
		label var f_energdiff_stdX`var' "EnergDiff x `l'"
		}
	
	label var f_relestknow_std "RelEstKnow"
	label var f_energdiff_std "EnergDiff"
end

program load_elasticity
	syntax, fn(string) 
	insheet using "`fn'", clear 
	drop if f_relestknow_std == . | elast_std == .
	generate cat_brand = cat_name+ "_" +brand_name
	egen cat_brand_num = group(cat_brand)
	gen weights = 1/elast_se
	
end
	
program elasticity
	syntax, fn(string)
	
	eststo clear
	
	/* ad */
	load_elasticity, fn("`fn'")
	keep if var_name=="adstock_bt"
	meancenter_interact
	eststo m1: quietly reg elast_std $vars [pw=weights] 
	
	/* fd_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="fd_bt"
	meancenter_interact
	eststo m2: quietly reg elast_std $vars [pw=weights] 
	
	/* pct_store_skus_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pct_store_skus_bt"
	meancenter_interact
	eststo m3: quietly reg elast_std $vars [pw=weights]
	
	/* pi_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pi_bt"
	meancenter_interact
	eststo m4: quietly reg elast_std $vars [pw=weights] 
	
	/* rreg_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="rreg_pr_bt"
	meancenter_interact
	eststo m5: quietly reg elast_std $vars [pw=weights] 
	
	* capture erase "$rtf_out"
	esttab m* using "$rtf_out", append mtitles("ad" "fd" "pct_store_skus" "pi" "rreg") nodepvar label noconstant ///
	   addnote("All estimated with WLS.") title("Elasticities") modelwidth(6 6 6 6 6) varwidth(24) ///
	   stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)  fmt(a2 a2 3 0 0)) ///
	   onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace b(a2)
				
end

*equity
*elasticity

foreach m of local mlist {
 local equityfn = "`path'\`m'\equity.csv"
 local elastfn = "`path'\`m'\elasticities.csv"
 
 insheet using "`equityfn'", clear 
	
 * regular
 global rtf_out "`path'\`m'\stata_brand.rtf"
 global vars f_relestknow_std f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_energdiff_std f_energdiff_stdXseccat f_energdiff_stdXnewbrnd seccat_mc newbrnd_mc
 equity, fn("`equityfn'")
 elasticity, fn("`elastfn'")

 * regular + cat
 global rtf_out "`path'\`m'\stata_brand_c4.rtf"
 global vars f_relestknow_std f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_relestknow_stdXc4 f_energdiff_std f_energdiff_stdXseccat f_energdiff_stdXnewbrnd f_energdiff_stdXc4 seccat_mc newbrnd_mc c4_mc
 equity, fn("`equityfn'")
 elasticity, fn("`elastfn'")

 * regular + cat
 global rtf_out "`path'\`m'\stata_brand_herf.rtf"
 global vars f_relestknow_std f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_relestknow_stdXherf f_energdiff_std f_energdiff_stdXseccat f_energdiff_stdXnewbrnd f_energdiff_stdXherf seccat_mc newbrnd_mc herf_mc
 equity, fn("`equityfn'")
 elasticity, fn("`elastfn'")
 
 * regular + cat
 global rtf_out "`path'\`m'\stata_brand_catgrowth.rtf"
 global vars f_relestknow_std f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_relestknow_stdXcatgrowth_abs f_energdiff_std f_energdiff_stdXseccat f_energdiff_stdXnewbrnd f_energdiff_stdXcatgrowth_abs seccat_mc newbrnd_mc catgrowth_abs
 equity, fn("`equityfn'")
 elasticity, fn("`elastfn'")
 
 * regular + cat
 global rtf_out "`path'\`m'\stata_brand_catgrowth_andc4.rtf"
 global vars f_relestknow_std f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_relestknow_stdXc4 f_relestknow_stdXcatgrowth_abs f_energdiff_std f_energdiff_stdXseccat f_energdiff_stdXnewbrnd f_energdiff_stdXc4 f_energdiff_stdXcatgrowth_abs seccat_mc newbrnd_mc c4 catgrowth_abs
 equity, fn("`equityfn'")
 elasticity, fn("`elastfn'")

 }	

