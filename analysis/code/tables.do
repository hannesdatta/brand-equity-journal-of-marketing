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

/* Goal: estimate Model 19, 20, 21 using Random, Fixed and OLS
*/

/* Estimate elasticities using the same specification */



program equity
	insheet using "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\MNL_copula_5mmix\equity.csv", clear 
	*drop if bav_asset == .
	drop if f_relestknow_std == .
	
	generate cat_brand = cat_name+ "_" +brand_name
	egen cat_brand_num = group(cat_brand)
	gen weights = 1/sbbe_se
	
	meancenter
		
	eststo clear
	
	eststo m1: quietly reg sbbe_std f_relestknow_std f_energdiff_std [pw = weights]
	
	eststo m2: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat newbrnd [pw = weights]
	
	eststo m3: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat newbrnd f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_energdiff_stdXseccat f_energdiff_stdXnewbrnd [pw=weights]
	
	local varlist sbbe_std f_relestknow_std f_energdiff_std seccat newbrnd f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_energdiff_stdXseccat f_energdiff_stdXnewbrnd
	eststo m4: quietly xtmixed `varlist' [pw=weights] || cat_brand_num:, mle vce(cluster cat_brand_num)
	
	esttab m*, mtitles("WLS" "WLS" "WLS" "RE robust") nodepvar label addnote("") title("Equity") wide
	
end

program meancenter
	quietly sum seccat
	g seccat_mc = seccat-r(mean)
	
	quietly sum newbrnd
	g newbrnd_mc = newbrnd-r(mean)
	
	g f_relestknow_stdXseccat = f_relestknow_std * seccat_mc
	g f_relestknow_stdXnewbrnd = f_relestknow_std * newbrnd_mc
	g f_energdiff_stdXseccat = f_energdiff_std * seccat_mc
	g f_energdiff_stdXnewbrnd = f_energdiff_std * newbrnd_mc
	
	label var f_relestknow_std "RelEstKnow_STD"
	label var f_energdiff_std "EnergDiff_STD"
		
	label var f_relestknow_stdXseccat "RelEstKnow_STD x second. cat"
	label var f_relestknow_stdXnewbrnd "RelEstKnow_STD x new brand"
	label var f_energdiff_stdXseccat "EnergDiff_STD x second. cat"
	label var f_energdiff_stdXnewbrnd "EnergDiff_STD x new brand"
end

program load_elasticity
	insheet using "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\MNL_copula_5mmix\elasticities.csv", clear 
	drop if f_relestknow_std == . | elast_std == .
	generate cat_brand = cat_name+ "_" +brand_name
	egen cat_brand_num = group(cat_brand)
	gen weights = 1/elast_se
	
end
	
program elasticity
	
	local varlist elast_std f_relestknow_std f_energdiff_std seccat newbrnd f_relestknow_stdXseccat f_relestknow_stdXnewbrnd f_energdiff_stdXseccat f_energdiff_stdXnewbrnd
	
	eststo clear
	
	/* ad */
	load_elasticity
	keep if var_name=="adstock_bt"
	meancenter
	eststo m1: quietly reg `varlist' [pw=weights] 
	
	/* fd_bt */
	load_elasticity
	keep if var_name=="fd_bt"
	meancenter
	eststo m2: quietly reg `varlist' [pw=weights] 
	
	/* pct_store_skus_bt */
	load_elasticity
	keep if var_name=="pct_store_skus_bt"
	meancenter
	eststo m3: quietly reg `varlist' [pw=weights]
	
	/* pi_bt */
	load_elasticity
	keep if var_name=="pi_bt"
	meancenter
	eststo m4: quietly reg `varlist' [pw=weights] 
	
	/* rreg_bt */
	load_elasticity
	keep if var_name=="rreg_pr_bt"
	meancenter
	eststo m5: quietly reg `varlist' [pw=weights] 
	

	esttab m*, mtitles("ad" "fd" "pct_store_skus" "pi" "rreg") nodepvar label addnote("All estimated with WLS.") title("Elasticities")
	
end


elasticity
