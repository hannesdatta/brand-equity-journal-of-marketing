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


global mlist = "MNL_copula_5mmix "
global path = "d:\DATTA\Dropbox\Tilburg\Projects\BAV\Shared\analysis_hannes\analysis\output\"


program equity
	syntax, fn(string) vars(varlist)
	
	insheet using "`fn'", clear 
	do_preclean
	gen weights = 1/sbbe_se_std

	* Define models
	eststo clear
	xtset cat_brand_num
	eststo m1: quietly reg sbbe_std `vars', vce(cluster cat_brand_num)
	eststo m2: quietly reg sbbe_std `vars' [pw=weights], vce(cluster cat_brand_num)
	eststo m3: quietly xtmixed sbbe_std `vars' [pw=weights] || cat_brand_num:, mle vce(cluster cat_brand_num)
	eststo m4: quietly xtreg sbbe_std `vars', be
	eststo m5: quietly xtreg sbbe_std `vars', fe vce(cluster cat_brand_num)
	
	capture erase "$rtf_out"
	esttab m* using "$rtf_out", mtitles("OLS" "WLS" "RE" "Between (no weights)" "FE (no weights)") nodepvar label ///
	addnote("") title("Equity") modelwidth(5 5 5 5 5 5 5 5 5)  varwidth(22) ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations) fmt(a2 a2 3 0 0)) ///
	onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace b(a2)
	
end

program equity_sensitivity
	syntax, ttitle(string)
	eststo clear
	
	eststo m4: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std [pw=weights], vce(cluster cat_brand_num)
	
	eststo m4b: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 food_drink_cigs ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std ///
						   f_relestknow_stdXfood_drink_cigs f_energdiff_stdXfood_drink_cigs [pw=weights], vce(cluster cat_brand_num)

	eststo m4c: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 cat_invol ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std ///
						   f_relestknow_stdXcat_invol f_energdiff_stdXcat_invol [pw=weights], vce(cluster cat_brand_num)
						   
	eststo m4d: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 cat_hedonic ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std ///
						   f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic [pw=weights], vce(cluster cat_brand_num)
						   
	eststo m4e: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 cat_utilit ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std ///
						   f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit [pw=weights], vce(cluster cat_brand_num)
						   
	eststo m4f: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 cat_perfrisk ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std ///
						   f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk [pw=weights], vce(cluster cat_brand_num)
						   
	eststo m4g: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 cat_socdemon ///
						   f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std ///
						   f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon [pw=weights], vce(cluster cat_brand_num)

	capture erase "$rtf_out"
	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") modelwidth(5 5 5 5 5 5 5 5 5)  varwidth(22) ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations) fmt(a2 a2 3 0 0)) ///
	onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace b(a2)
end

program equity_catmeasures
	syntax, ttitle(string)
	eststo clear
		
	eststo m4x1: quietly reg sbbe_std f_relestknow_std f_energdiff_std cat_invol ///
						   f_relestknow_stdXcat_invol f_energdiff_stdXcat_invol f_relestknow_stdXf_energdiff_std [pw=weights], vce(cluster cat_brand_num)
	
	eststo m4x2: quietly reg sbbe_std f_relestknow_std f_energdiff_std cat_hedonic ///
						   f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic f_relestknow_stdXf_energdiff_std [pw=weights], vce(cluster cat_brand_num)
	
	eststo m4x3: quietly reg sbbe_std f_relestknow_std f_energdiff_std cat_utilit ///
						   f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit f_relestknow_stdXf_energdiff_std [pw=weights], vce(cluster cat_brand_num)
						   
	eststo m4x4: quietly reg sbbe_std f_relestknow_std f_energdiff_std cat_perfrisk ///
						   f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk f_relestknow_stdXf_energdiff_std [pw=weights], vce(cluster cat_brand_num)
	
	eststo m4x5: quietly reg sbbe_std f_relestknow_std f_energdiff_std cat_socdemon ///
						   f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon f_relestknow_stdXf_energdiff_std [pw=weights], vce(cluster cat_brand_num)
	
	capture erase "$rtf_out"
	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") modelwidth(5 5 5 5 5 5 5 5 5)  varwidth(22) ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations) fmt(a2 a2 3 0 0)) ///
	onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace b(a2)
end

program harald

*reg sbbe_std f_relestknow_std f_energdiff_std seccat c4 f_relestknow_stdXc4 f_energdiff_stdXc4 cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon i.cat_name [pw=weights], vce(cluster cat_brand_num)
			 
	

end

program equity_22may2016
	syntax, ttitle(string)
	eststo clear
		
	eststo m05a0: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
	
	eststo m05a1: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  [pw=weights], vce(cluster cat_brand_num)
									  
	estadd vif
									  
	eststo m05b: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05c: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif

	eststo m05c2: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
	
	eststo m05d: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05e: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05f: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
								  
	eststo m05g: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05h: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  [pw=weights], vce(cluster cat_brand_num)
									  
	estadd vif
									  
	eststo m05i: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05j: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
				
	eststo m05k: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
				
	capture erase "$rtf_out"

	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") ///
	onecell b(a2) compress ///
	modelwidth(5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5) varwidth(22) ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)) ///
	aux(vif 2) wide nopar ///
	star(+ 0.10 * 0.05 ** .01 *** .001) replace ///
	order(f_relestknow_std f_energdiff_std seccat ///
		  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
		  f_relestknow_stdXf_energdiff_std ///
		  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
		  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
		  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
		  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon)
		  
	*fmt(a2 a2 3 0 0)) ///
	*onecell  
	* nogap 
end

program equity_22may2016_v2
	syntax, ttitle(string)
	eststo clear
		
	eststo m05l: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
	
	eststo m05m: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif

	eststo m05n: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05o: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
	
	eststo m05p: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05q: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05r: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05s: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
										  
	eststo m05t: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  									  
	eststo m05u: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
									  
	eststo m05v: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif
				
	eststo m05w: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  f_relestknow_stdXf_energdiff_std ///
									  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
	estadd vif

	capture erase "$rtf_out"
*onecell b(a2) compress ///
	
	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") ///
	aux(vif 2) wide nopar ///
	modelwidth(5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5) varwidth(22) ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)) ///
	aux(vif 2) wide nopar ///
	star(+ 0.10 * 0.05 ** .01 *** .001) replace ///
	order(f_relestknow_std f_energdiff_std seccat ///
		  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
		  f_relestknow_stdXf_energdiff_std ///
		  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
		  cat_utilit f_relestknow_stdXcat_utilit f_energdiff_stdXcat_utilit ///
		  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
		  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon)
		  
	*fmt(a2 a2 3 0 0)) ///
	*onecell  b(a2) compress 
	* nogap 

end


program equity_final
	syntax, ttitle(string)
	eststo clear
		
	eststo m05c2: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
									  
	eststo m05c2_2: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  estbrand ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
  eststo m05c2_2b: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  estbrand f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
 
   	eststo m05e: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
						  
	eststo m05e_2: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  estbrand ///
									  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)
eststo m05e_2b: quietly reg sbbe_std f_relestknow_std f_energdiff_std seccat ///
									  estbrand f_relestknow_stdXf_energdiff_std ///
									  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
									  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
									  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon ///
									  [pw=weights], vce(cluster cat_brand_num)

	estadd vif
	capture erase "$rtf_out"

	esttab m* using "$rtf_out", nodepvar label ///
	addnote("") title("`ttitle'") ///
	b(a2) compress ///
	modelwidth(5 5 5 5 5 5 5 5 5 5) varwidth(22) nogap ///
	stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)) ///
	star(* 0.10 ** 0.05 *** .01) replace ///
	order(f_relestknow_std f_energdiff_std seccat ///
		  c4 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
		  cat_hedonic f_relestknow_stdXcat_hedonic f_energdiff_stdXcat_hedonic ///
		  cat_perfrisk f_relestknow_stdXcat_perfrisk f_energdiff_stdXcat_perfrisk ///
		  cat_socdemon f_relestknow_stdXcat_socdemon f_energdiff_stdXcat_socdemon)
	
	
	*fmt(a2 a2 3 0 0)) ///
	*onecell  
	* nogap 
	*aux(vif 2) wide nopar ///
	
end

program analysis2
	syntax, path(string)
	
	local equityfn = "`path'\equity.csv"
	local elastfn = "`path'\elasticities.csv"
	
	insheet using "`equityfn'", clear 

	do_preclean
	
	* Set weights
	gen weights = 1/sbbe_se_std
	
	* Equity
	* global rtf_out "`path'\stata_equity_wsurvey.rtf"
	* equity_sensitivity, ttitle("Equity with weights = 1/sbbe_se_std")
	
	* Equity
	* global rtf_out "`path'\stata_equity_newsurvey.rtf"
	* equity_catmeasures, ttitle("Equity with new category measures and weights = 1/sbbe_se_std")
	
	* Equity
	*global rtf_out "`path'\stata_equity_22may2016.rtf"
	*equity_22may2016, ttitle("Equity regressions, 22 May 2016")
	
	* Equity v2
	*global rtf_out "`path'\stata_equity_22may2016_v2.rtf"
	*equity_22may2016_v2, ttitle("Equity regressions, 22 May 2016")
	* Equity Final check with Kusum
	global rtf_out "`path'\stata_equity_23may2016.rtf"
	equity_final, ttitle("Equity regressions, 23 May 2016")
	
	* main effects only
	elasticity, fn("`elastfn'") elast_vars(f_relestknow_std f_energdiff_std)
	
		
end


program go
	local path = "$path\MNL_copula_5mmix\"
	local equityfn = "`path'\equity.csv"
	insheet using "`equityfn'", clear 
	drop if f_relestknow == .
end


program main2
	analysis2, path("$path\MNL_copula_5mmix\")
end
	
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

	label var seccat "Brand in second. cat."
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
		g f_relestknow_stdX`var' = f_relestknow_std * `var'_mc
		g f_energdiff_stdX`var' = f_energdiff_std * `var'_mc
		local l: variable label `var'
		label var f_relestknow_stdX`var' "Relevant Stature x `l'"
		label var f_energdiff_stdX`var' "Energized Diff. x `l'"
		}
		
	* Interactions for non-meancentered variables
	foreach var in `otherinteract_vars' {
		g f_relestknow_stdX`var' = f_relestknow_std * `var'
		g f_energdiff_stdX`var' = f_energdiff_std * `var'
		local l: variable label `var'
		label var f_relestknow_stdX`var' "Relevant Stature x `l'"
		label var f_energdiff_stdX`var' "Energized Diff. x `l'"
		}
		
	label var f_relestknow_std "Relevant Stature"
	label var f_energdiff_std "Energized Diff."
	
	g f_relestknow_std_sq = f_relestknow_std ^ 2
	g f_energdiff_std_sq = f_energdiff_std ^ 2
	label var f_relestknow_std_sq "Relevant Stature squared"
	label var f_energdiff_std_sq "Energized Diff. squared"
	
	g f_relestknow_stdXf_energdiff_std = f_relestknow_std*f_energdiff_std
	
	label var f_relestknow_stdXf_energdiff_std "Relevant Stature x Energized Diff."
	
	g frelenergXestbrand = f_relestknow_stdXf_energdiff_std * estbrand
	
end

program load_elasticity
	syntax, fn(string) 
	insheet using "`fn'", clear 
	drop if f_relestknow_std == . | elast_std == .
	gen weights = 1/elast_se_std
	
end


program elasticity
	syntax, fn(string) elast_vars(varlist)
	
	eststo clear
	
	/* ad */
	load_elasticity, fn("`fn'")
	keep if var_name=="adstock_bt"
	do_preclean
	eststo m1: quietly reg elast_std `elast_vars' [pw=weights] 
	
	/* fd_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="fd_bt"
	do_preclean
	eststo m2: quietly reg elast_std `elast_vars' [pw=weights] 
	
	/* pct_store_skus_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pct_store_skus_bt"
	do_preclean
	eststo m3: quietly reg elast_std `elast_vars' [pw=weights]
	
	/* pi_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="pi_bt"
	do_preclean
	eststo m4: quietly reg elast_std `elast_vars' [pw=weights] 
	
	/* rreg_bt */
	load_elasticity, fn("`fn'")
	keep if var_name=="rreg_pr_bt"
	do_preclean
	eststo m5: quietly reg elast_std `elast_vars' [pw=weights] 
	
	* capture erase "$rtf_out" *append
	esttab m* using "$rtf_out", append mtitles("adstock" "feature/display" "total distribution" "price index" "regular price") nodepvar label ///
	   addnote("All estimated with WLS.") title("Elasticities, 1/elast_se_std as weights") modelwidth(8 8 8 8 8) varwidth(30) ///
	   stats(r2 F p N_clust N, labels(R-squared F p-value brands observations)  fmt(a2 a2 3 0 0)) ///
	   onecell nogap star(* 0.10 ** 0.05 *** .01) replace b(a2)
				
end

*equity
*elasticity

program analysis
	syntax, path(string) save_fn(string) [maineffects(varlist) interactions(varlist)]
	
	local equityfn = "`path'\equity.csv"
	local elastfn = "`path'\elasticities.csv"
	insheet using "`equityfn'", clear 
	do_preclean
	
	global rtf_out "`path'\stata_`save_fn'.rtf"
	
	* equity
	equity, fn("`equityfn'") vars(f_relestknow_std f_energdiff_std `maineffects' `interactions')
	
	* main effects only
	elasticity, fn("`elastfn'") elast_vars(f_relestknow_std f_energdiff_std)
	
	* main effects only with interaction
	elasticity, fn("`elastfn'") elast_vars(f_relestknow_std f_energdiff_std f_relestknow_stdXf_energdiff_std)
	
	* main effects plus additional variables
	elasticity, fn("`elastfn'") elast_vars(f_relestknow_std f_energdiff_std `maineffects' f_relestknow_stdXf_energdiff_std )
	
	* main effects plus interactions and additional variables
	elasticity, fn("`elastfn'") elast_vars(f_relestknow_std f_energdiff_std `maineffects' `interactions' f_relestknow_stdXf_energdiff_std )
	
end

program main
	
	local m = "MNL_copula_5mmix"
	local fpath = "$path\`m'\"
	local equityfn "`fpath'\equity.csv"
	local elastfn = "`fpath'\elasticities.csv"
		
	insheet using "`equityfn'", clear 
	do_preclean

	* M12 + fmcg
	analysis, path("`fpath'") save_fn("M12A_fmcg") maineffects(fmcg_seccat c4) ///
										     interactions(f_relestknow_stdXfmcg_seccat f_energdiff_stdXfmcg_seccat ///
											 f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std)

	* M12 + seccat
	analysis, path("`fpath'") save_fn("M12B_seccat") maineffects(seccat c4) ///
										     interactions(f_relestknow_stdXseccat f_energdiff_stdXseccat ///
											 f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_stdXf_energdiff_std)

	* M12 + fmcg + growth
	analysis, path("`fpath'") save_fn("M12A2_fmcg_catgrowth") maineffects(fmcg_seccat c4 catgrowth_abs) ///
										     interactions(f_relestknow_stdXfmcg_seccat f_energdiff_stdXfmcg_seccat ///
											 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
											 f_relestknow_stdXcatgrowth_abs f_energdiff_stdXcatgrowth_abs ///
											 f_relestknow_stdXf_energdiff_std)

	* M12 + seccat + growth
	analysis, path("`fpath'") save_fn("M12B2_seccat_catgrowth") maineffects(seccat c4 catgrowth_abs) ///
										     interactions(f_relestknow_stdXseccat f_energdiff_stdXseccat ///
											 f_relestknow_stdXc4 f_energdiff_stdXc4 ///
											 f_relestknow_stdXcatgrowth_abs f_energdiff_stdXcatgrowth_abs ///
											 f_relestknow_stdXf_energdiff_std)
									 
end

*main

program main_old

	foreach m of local mlist {
	 
	 global elast_vars f_relestknow_std f_energdiff_std
	 
	 
	  * m4
	 global rtf_out "`path'\`m'\stata_M4.rtf"
	 global vars f_relestknow_std f_energdiff_std f_relestknow_std_sq f_energdiff_std_sq
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	  * m5
	 global rtf_out "`path'\`m'\stata_M5.rtf"
	 global vars f_relestknow_std f_energdiff_std seccat
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	  * regular
	 global rtf_out "`path'\`m'\stata_M6.rtf"
	 global vars f_relestknow_std f_energdiff_std seccat f_relestknow_stdXseccat f_energdiff_stdXseccat
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	 * regular
	 global rtf_out "`path'\`m'\stata_M7.rtf"
	 global vars f_relestknow_std f_energdiff_std c4 f_relestknow_stdXc4 f_energdiff_stdXc4
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	 * regular
	 global rtf_out "`path'\`m'\stata_M8.rtf"
	 global vars f_relestknow_std f_energdiff_std c4 f_relestknow_stdXc4 f_energdiff_stdXc4 f_relestknow_std_sq f_energdiff_std_sq
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	 * regular
	 global rtf_out "`path'\`m'\stata_M9.rtf"
	 global vars f_relestknow_std f_energdiff_std fmcg_seccat f_relestknow_stdXfmcg_seccat f_energdiff_stdXfmcg_seccat
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	* regular
	 global rtf_out "`path'\`m'\stata_M10.rtf"
	 global vars f_relestknow_std f_energdiff_std retail_seccat f_relestknow_stdXretail_seccat f_energdiff_stdXretail_seccat
	 equity, fn("`equityfn'")
	 elasticity, fn("`elastfn'")

	 }	

end

*main
main2
