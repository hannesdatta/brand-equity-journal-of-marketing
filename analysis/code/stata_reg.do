./**********************************************************

 * stata_reg.do 
 * BAV project
 * Hannes Datta
 * bug reports: h.datta@tilburguniversity.edu
 
/ **********************************************************
 **********************************************************/
 
cap log close
set linesize 255

version 14
clear all
set mem 500m
set matsize 10000
set more off
set seed 04071984
set sortseed 09091987
set scheme s2mono

local wlist = "w_atus_ag w_default"

global dataset_in = "..\output\elasticities.csv"
global out_dir = "..\output\"

global signstars = "       {\super +} {\i p} < 0.10, {\super *} {\i p} < 0.05, {\super **} {\i p} < .01, {\super ***} {\i p} < .001."
global signstarswo10 = "       {\super *} {\i p} < 0.05, {\super **} {\i p} < .01, {\super ***} {\i p} < .001."
	
*cd "d:\DATTA\Dropbox\Projects\analysis\code\"
*cd "D:\Dropbox\A  My Dropbox\academic research\Spotify\analysis\code"

do "common.do"

program main
	syntax, wtchoice(string)
	
	global wdef = "`wtchoice'"
	global report_out = "$out_dir" + "full_tables_" + "$wdef.rtf"
	
	*******************
	* Main analysis   *
	*******************
	global label_appendix = "" 
	global tablenum = 2

	run_analysis

	************
	* APPENDIX *
	************
	
	global tablenum = 1
	global label_appendix = "C" 
	* set to empty string if NOT appendix, and to A IF it is an appendix (or C, as currently in the paper)
	
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	
	* computes summary stats on the currently loaded sample (e.g., number of users); external in common.do
	sample_cutoffs
	sample_stats
	
	/* DESCRIPTIVE STATISTICS */
	rtf_newpage "us"
	table_quantity_stats
	rtf_newpage "us"
	table_variety_stats
	rtf_newpage "us"

	/* Matching */
	match_table_stats
	rtf_newpage "us"
	match_table_model
	rtf_newpage "us"
	
	/* Weights */
	table_weights
	rtf_newpage "us"
	
	/* Placebo test */
	placebo_test
	rtf_newpage "us"
	
	/* Summary of variables in levels */
	table_descr_levels
	rtf_newpage "us"

end



program run_analysis
	if "$wdef" == "w_default" {
		global note_weighting "are not reweighted" /* to be read as Panelists/Estimates [...are reweighted using...] */
		}
	if "$wdef" == "w_atus_ag" {
		global note_weighting "are reweighted using estimates of age and gender to match the US population of music listeners as per the American Time Use Survey by the US Bureau of Labor Statistics (2013)" /* to be read as Panelists/Estimates [...are reweighted using...] */
		}
	
	use "$dataset_in", clear 
	keep if matched==1 & common_support==1
	
	sample_cutoffs
	sample_stats
	
	global appendrtf = "no"
	
	table_quantity_log

	keep if plays_tot>0

	global appendrtf = "yes"
	
	table_v_volume_logs

	table_v_concentration1
	table_v_concentration2

	table_v_discovery

	table_v_repeatconsumption_share

	table_v_new_plays

end

************************
***** DESCRIPTIVES *****
************************
program doodle

	use "$dataset_in", clear 

	eststo clear

	keep if matched == 1 & common_support == 1 

	gen post = periods_since>=0
	
	collapse (mean) c2_artists, by(adoption user post)
	
	bys post adoption : summarize c2
	
end


* on matched samples, but not reweighted
program table_descr_levels

	use "$dataset_in", clear 

	eststo clear

	local vlist plays_tot plays_itun plays_oth 
	
	local vlist2 uniq_artist uniq_song uniq_genre superstar_shareu20 superstar_shareu100 superstar_shareu500 superstar_sharep20 superstar_sharep100 superstar_sharep500 c2_artists c2_songs c2_genres c10_artists c10_songs c10_genres newartu newsonu newgenu newartp newsonp newgenp martistu msognu mgenreu mnewartistu mnewsognu mnewgenreu 
	
	local vlist3 top1_artist top5_artist top1_song top5_song top1_genre top5_genre
	
	keep if matched == 1 & common_support == 1 
	
	sort adoptiontype_both
	
	foreach var in `vlist2' {
		replace `var' = . if plays_tot==0
		}
		
	foreach var in `vlist3' {
		replace `var' = . if !(next_all_artist_top1 > 0 & (periods_since_adoption<=(-8) | (periods_since_adoption>=0 & periods_since_adoption <= max_period - 8)))
		}
	
 
	* Save variable labels
	foreach v of var * {
		local l`v' : variable label `v'
			if `"`l`v''"' == "" {
			local l`v' "`v'"
			}
		}
	
	collapse (mean) `vlist' `vlist2' `vlist3', by (adoptiontype_both user)

	* Restore variable labels
	foreach v of var * {
		label var `v' "`l`v''"
		}

	by adoptiontype_both: eststo: quietly estpost summarize `vlist' `vlist2' `vlist3'

	esttab using "$report_out", append cells("mean(fmt(a2))") label nodepvar noobs title({\b Table $label_appendix$tablenum.} {\i Summary Statistics in Levels for Adopters and Non-adopters Before and After Adoption}) modelwidth(13 13 13 13) varwidth(25)

end

* on matched samples, but not reweighted

program table_quantity_stats
	/* generates summary statistics on playcounts per platform for adopters and non-adopters */
	use "$dataset_in", clear 
	keep if matched==1&common_support==1
	
	estpost tabstat plays_spot plays_tot plays_itun plays_other, by(adoptiontype_both) statistics(mean sd min max) nototal
	esttab . using "$report_out", append cells("plays_spot(fmt(a2) label(Spotify)) plays_tot(fmt(a2) label(All)) plays_itun(fmt(a2) label(iTunes)) plays_other(fmt(a2) label(Other))") nomtitle nonumber ///
		varwidth(24) onecell nogap title({\b Table $label_appendix$tablenum.} {\i Summary Statistics: Quantity}) modelwidth(10 10 10 10 ) /// 
		label nomtitle nonumber replace ///
		note({\i Notes}: Summary statistics $label_obs_matched; $label_unit_userweek.)
		
	global tablenum = $tablenum+1
end


program table_variety_stats
	/* generates summary statistics on a set of variety measures */
	use "$dataset_in", clear 
	keep if matched==1&common_support==1&plays_tot>0
	
	generate ydum = uniform()
	
	local vlist uniq_artist uniq_song uniq_genre ///
				superstar_shareu20 superstar_shareu100 superstar_shareu500 ///
				superstar_sharep20 superstar_sharep100 superstar_sharep500 ///
				c2_artists c2_songs c2_genres ///
				c10_artists c10_songs c10_genres ///
				newartu newsonu newgenu newartp newsonp newgenp ///
				martistu msognu mgenreu mnewartistu mnewsognu mnewgenreu
				
	local vlist2 top1_artist top5_artist top1_song top5_song top1_genre top5_genre

	foreach var in `vlist2' {
	replace `var' = . if !(next_all_artist_top1 > 0 & (periods_since_adoption<=(-8) | (periods_since_adoption>=0 & periods_since_adoption <= max_period - 8)))
	}

	/*rtf_newpage "us"*/
	
	foreach v of varlist `vlist' `vlist2' {
		label variable `v' `"- `: variable label `v''"'
	}

	regress ydum `vlist', noconstant
	estadd summ
	
	esttab . using "$report_out", append ///
		cells("mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") onecell nogap nomtitle nonumber label title({\b Table $label_appendix$tablenum.} ///
		{\i Summary Statistics: Variety}) ///
		note({\i Notes}: Summary statistics $label_obs_matched; $label_unit_userweek when there is at least one song played. The relevant statistics are computed on the basis of non-missing artist characteristics.) ///
		varwidth(46) modelwidth(7) noobs ///
		refcat(uniq_artist "\b Breadth of variety" superstar_shareu20 "\b Concentration of variety" newartu "\b Discovery of new content" martistu "\b Repeat consumption", nolabel)
	
	global tablenum = $tablenum+1
	drop ydum
end


program match_table_stats
	use "$dataset_matching_in", clear
	sample_stats
	sort adopt
	
	/* generates summary statistics on unmatched sample */
	
	generate ydum2 = uniform()
	
	local vlist avgdailyplays uniq_artists superstar_plays_100 c2_artists s_newartist sartists_morethanonce library_artistage europe southamerica canada_us age_imp gender_female_imp
	eststo clear
	by adopt: eststo: quietly regress ydum2 `vlist', noconstant
	
	estadd summ : *

	by adopt: eststo: quietly regress ydum2 `vlist' if matched==1, noconstant
	eststo drop est4
	estadd summ : est3

	/*rtf_newpage "us"  */
 
	esttab using "$report_out", append cells("mean(fmt(a3) label(Mean))" sd(par fmt(2))) label ///
		replace mtitles("All control users " "Treated users" "Matched control users") ///
		varwidth(32) modelwidth(10) b(a2) ///
		title({\b Table $label_appendix$tablenum.} ///
		{\i Comparison of Adopters with (matched and unmatched) Non-Adopters}) ///
		note({\i Notes}: Means and standard deviations $label_obs_unmatched_matching2; $label_unit_user. Some users are dropped because they do not listen in the initialization period.)
		
	eststo clear 
		 
	global tablenum = $tablenum+1
	drop ydum2
	
end

program match_table_model
	/* Replicate logit model for propensity scores */
	use "$dataset_matching_in", clear
	sample_stats
	sort adopt

	*rtf_newpage "us"
	
	label var adopt " "
	
	local regressors avgdailyplays uniq_artists superstar_plays_100 c2_artists s_newartist sartists_morethanonce library_age_1800 europe southamerica canada_us 
	*age_imp gender_female_imp
	
	eststo matchlogit: logit adopt `regressors'
	
	esttab matchlogit using "$report_out", append ///
		replace  varwidth(38) modelwidth(20) b(a2) label ///
		mtitles ("Mean effect (Std. error)") ///
		coeflabels(_cons "constant") onecell ///
		nonote ///
		se ///
		star(+ 0.10 * 0.05 ** .01 *** .001) ///
		addnote("{\i Notes}: Logit model with standard errors in parentheses. Estimates $label_obs_unmatched_matching; $label_unit_user. The dependent variable is whether a user adopted Spotify (adopt=1), or not (adopt=0). Some users are dropped before model estimation because they do not listen in the initialization period." "$signstars") ///
		title("{\b Table $label_appendix$tablenum.} {\i Propensity Score Model}") wide nodepvars nonumbers

	global tablenum = $tablenum+1
				
	fitstat
	 
end

program table_weights
	use "$dataset_in", clear 
	keep if matched==1 & common_support==1
	egen first_obs = tag(username)
	keep if first_obs == 1
	/* note, without filter, this data set pertains to the unmatched data set of 960 users */
	
	g w_female = .
	replace w_female = w_atus_ag if customer_female==1
	g w_male = .
	replace w_male = w_atus_ag if customer_female==0
	
	label var w_female "female"
	label var w_male "male"
	
	estpost tabstat w_male w_female, by(age_discrete) statistics(mean N) columns(statistics) nototal
	
	esttab . using "$report_out", append ///
		main(mean) aux(count) mtitle("age group") nostar unstack noobs  nonumber nonote nogap ///
		title({\b Table $label_appendix$tablenum.} {\i Poststratification weights}) /// 
		note({\i Notes}: Poststratification weights are reported as inverse-probability weights for the different demographic strata (with frequencies in the sample in parentheses).) ///
		label
	
	global tablenum = $tablenum+1
	clear
end


********************
***** ANALYSIS *****
********************

program make_regression_table  
	syntax varlist using / , footnote(string) ttitle(string) [additional_controls(varlist) colwidth(integer 7)]
	
	local controls1 adoption_shortterm adoption_mediumterm adoption_longterm _Iperiod*
	label var adoption_shortterm "$label_shortterm" 
	label var adoption_mediumterm "$label_mediumterm" 
	label var adoption_longterm "$label_longterm"
	
	xi i.period
		
	estimates clear

	local count_var : word count `varlist'
	local count_control : word count `additional_controls'
	
	display "$wdef"
	display "`varlist'"

	assert (`count_var'==`count_control') | (`count_control'==0)
		
	* Loop over DVs (varlist) and additional controls.
		forvalues i = 1/`count_var' {
			local m : word `i' of `varlist'
			
			local cntr : word `i' of `additional_controls'
			display "`a'  `b'"
			
			xtset username
			eststo m`m': areg `m' `controls1' `cntr' [pw = $wdef] , abs(username) vce(cluster username)

			}

	* Tabulate results  mtitles(`colheaders')   coeflabels() ///
	*global appendrtf "yes"
	
	if "$appendrtf" == "yes" {
		esttab m* using "$report_out", append drop(_Iperiod* _cons*) label noconstant ///
		stats(r2 F p N_clust N, labels(R-squared F p-value users observations)  fmt(a2 a2 3 0 0)) ///
		nonote addnote(`footnote') title(`ttitle')  ///
		se onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace varwidth(17) modelwidth(`colwidth') b(a2) 
		}
		
	if "$appendrtf" == "no" {
		esttab m* using "$report_out", drop(_Iperiod* _cons*) label noconstant ///
		stats(r2 F p N_clust N, labels(R-squared F p-value users observations)  fmt(a2 a2 3 0 0)) ///
		nonote addnote(`footnote') title(`ttitle')  ///
		se onecell nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace varwidth(17) modelwidth(`colwidth') b(a2) 
		}
		
	global tablenum = $tablenum+1
end

program placebo_test

	local controls1 placeboadoption _Iperiod*
	estimates clear
	
	* data set with total plays
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	xi i.period
	xtset username
	
	local vlist lplays_tot lplays_itun lplays_oth
	
	foreach m in `vlist' {
		eststo `m': areg `m' `controls1' [pw = $wdef], abs(username) vce(cluster username)
		}	
	
	* data set with variety metrics (conditioned on plays > 0)
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1 & plays_tot>0
	xi i.period
	xtset username
	
	local vlist luniq_artist luniq_song luniq_genre superstar_shareu20 superstar_shareu100 superstar_shareu500 superstar_sharep20 superstar_sharep100 superstar_sharep500 c2_artists c2_songs c2_genres c10_artists c10_songs c10_genres newartu newsonu newgenu newartp newsonp newgenp martistu msognu mgenreu mnewartistu mnewsognu mnewgenreu
	
	foreach m in `vlist' {
		eststo `m': areg `m' `controls1' [pw = $wdef], abs(username) vce(cluster username)
		* *[pw = $wdef]
		}	
		
	
	* data set with discovery metrics
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1 & plays_tot>0 & next_all_artist_top1 > 0 & (periods_since_adoption<=(-8) | (periods_since_adoption>=0 & periods_since_adoption <= max_period - 8))

	xi i.period
	xtset username
	
	local vlist top1_artist top5_artist top1_song top5_song top1_genre top5_genre
	
	foreach m in `vlist' {
		eststo `m': areg `m' `controls1' [pw = $wdef], abs(username) vce(cluster username)
		* *
		}	

	esttab, drop(_Iperiod* _cons*) se nostar r2 label
	
	matrix C = r(coefs)
	matrix S = r(stats)
	eststo clear

	local rnames : rownames C
	local models : coleq C
	local models : list uniq models
	local i 0

	foreach name of local rnames {
		local ++i
		local j 0
		capture matrix drop b
		capture matrix drop se
		foreach model of local models {
			local ++j
			matrix tmp = C[`i', 2*`j'-1]
			if tmp[1,1]<. {
				matrix colnames tmp = `model'
				matrix b = nullmat(b), tmp
				matrix tmp[1,1] = C[`i', 2*`j']
				matrix se = nullmat(se), tmp
				}
			}
		ereturn post b
		quietly estadd matrix se
		eststo `name'
	}

	local snames : rownames S
	local i 0
	
	foreach name of local snames {
		local ++i
		local j 0
		capture matrix drop b
		foreach model of local models {
			local ++j
			matrix tmp = S[`i', `j']
			matrix colnames tmp = `model'
			matrix b = nullmat(b), tmp
			}
		ereturn post b
		eststo `name'
	}

	esttab using "$report_out", append mtitles ("Placebo effect" "N" "R-squared") noobs compress nonumb label wide ///
	title({\b Table $label_appendix$tablenum.} {\i Placebo tests}) ///
	nonote addnote("{\i Notes}: Placebo adoption regressions with robust standard errors in parentheses. Estimates are calculated on a matched sample of $n_matched adopters and $n_matched non-adopters in a sample prior to adoption; $label_fe and $label_unit_userweek. Estimates $note_weighting. The independent variable is an indicator for users' placebo adoption. For each user, this indicator is 0 until half the pre-adoption time period elapses; then the value takes on one until the end of the pre-adoption time period." "$signstars") ///
	se nogap star(+ 0.10 * 0.05 ** .01 *** .001) replace varwidth(40) b(3)
	
	global tablenum = $tablenum+1
	
end


program table_quantity_log
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	local vlist lplays_tot lplays_itun lplays_oth
	local note `" "{\i Notes}: Regression with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek. Estimates $note_weighting. The dependent variable is the log number of songs heard by a panelist on a week (playcount). $label_independentvars." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Consumption Growth and Displacement across Platforms}"
	
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'") colwidth(15)
end

program table_v_volume_logs
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	keep if plays_tot>0
	local vlist luniq_artist luniq_song luniq_genre
	local note `" "{\i Notes}: Regression with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek when there is at least one song played. The dependent variable is the log number of distinct artists, songs, and genres heard by a panelist on a week. Estimates $note_weighting. $label_independentvars." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Breadth of variety: Unique Number of Consumed Artists, Songs, and Genres}"
	
	rtf_newpage "us"
	
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'")  colwidth(12)
end


program table_v_concentration1
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	keep if plays_tot>0
	local vlist superstar_shareu20 superstar_shareu100 superstar_shareu500 superstar_sharep20 superstar_sharep100 superstar_sharep500 
	local note `" "{\i Notes}: Regression with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek when there is at least one song played. The dependent variable is the number of unique / amount of plays to popular artists (measured in an initialization period) in a user's geographic region, devided by the number of unique artists / total number of plays. Estimates $note_weighting. $label_independentvars." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Concentration of Variety (1): Share of Superstar Consumption}"
	
	rtf_newpage "usland"
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'")  colwidth(10)

end

program table_v_concentration2
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	keep if plays_tot>0
	local vlist c2_artists c2_songs c2_genres c10_artists c10_songs c10_genres 
	local note `" "{\i Notes}: Regression with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek when there is at least one song played. The dependent variable is the share of playcounts to a user's weekly top two and top ten artists, songs, and genres (C2 and C10). Estimates $note_weighting. $label_independentvars." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Concentration of Variety (2): Share of Top Two (C2) and Top Ten (C10)}"
	
	rtf_newpage "usland"
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'")  colwidth(10)
end

program table_v_discovery
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	keep if plays_tot>0
		
	local vlist newartu newsonu newgenu newartp newsonp newgenp
	local note `" "{\i Notes}: Regression model with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek when there is at least one song played. The dependent variable is the number of distinct / amount of plays to distinct new artists, songs, and genres listened to by a user for the first time, devided by the total number of distinct artists, songs, and genres / total number of plays. Estimates $note_weighting. $label_new. $label_independentvars, and the log of unique artists, songs, and genres." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Discovery of New Content (1): New Artists, Songs, and Genres Consumed}"
		
	rtf_newpage "usland"
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'")  colwidth(10)

end

program table_v_repeatconsumption_share
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	keep if plays_tot>0

	local vlist martistu msognu mgenreu mnewartistu mnewsognu mnewgenreu
	local note `" "{\i Notes}: Regression with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek when there is at least one song played. The dependent variable is the number of unique known (versus new) artists, songs, and genres played more than once, devided by the total number of unique new and known artists, songs, and genres listened to. Estimates $note_weighting. $label_new. $label_independentvars." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Discovery of New Content (2): Repeat Consumption (Known versus New)}"
	rtf_newpage "usland"
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'")  colwidth(10)
end


program table_v_new_plays
	use "$dataset_in", clear
	keep if matched == 1 & common_support == 1
	keep if plays_tot>0
	keep if next_all_artist_top1>0 & (periods_since_adoption<=(-8) | (periods_since_adoption>=0 & periods_since_adoption <= max_period - 8))

	local vlist top1_artist top5_artist  top1_song top5_song  top1_genre top5_genre
	local note `" "{\i Notes}: Regression with robust standard errors in parentheses. Estimates $label_obs_matched; $label_fe and $label_unit_userweek with at least one new song played. Estimates $note_weighting. The dependent variable is the amount of plays to the top 1 and 5 new artists, songs, and genres in the week of discovery and subsequent seven weeks (t, t+1, ..., t+7) ranked in order of plays, divided by the amount of plays to the overall (not necessarily new) top 1 and 5 artists, songs, and genres over the same time period. Observations are excluded when the rolling 8-week window includes both pre-adoption and post-adoption periods, and when there are fewer than 8 weeks remaining at the end of each user's observation period. $label_new. $label_independentvars." "$signstars" "'
	local tabtitle "{\b Table $label_appendix$tablenum.} {\i Discovery of New Content (3): Value of "top" discoveries}"
	
	rtf_newpage "usland"
	
	make_regression_table `vlist' using "$dataset_in", footnote(`"`note'"') ttitle("`tabtitle'")  colwidth(10)
		
end


program rtf_newpage
	/* inserts a new page, with orientation "us" (for portrait) or "usland" (for landscape) 
	Requires package rtfutil; to be installed by typing ssc install rtfutil.
	*/
	
	rtfappend rtfdoc using "$report_tmp" , replace from("$report_out")
	rtfsect rtfdoc, paper("`1'")
	rtfclose rtfdoc
	copy "$report_tmp" "$report_out", replace
	erase "$report_tmp"
end

foreach wt of local wlist {
 main, wtchoice("`wt'")
}	
