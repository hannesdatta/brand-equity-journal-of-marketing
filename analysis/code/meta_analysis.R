

# Load data as CSV
models <- c('MNL_copula_4mmix', 'MNL_copula_5mmix')

for (r in models) {
	require(data.table)
	equity <- fread(paste0('..//output//', r, '//equity.csv'))
	elast <- fread(paste0('..//output//', r, '//elasticities.csv'))

	source('proc_report.R')
	options(width=1000)
	sink(paste0('..//output//', r, '//summary_', r, '.txt'))
	summarize_elast(elast)
	summarize_equity(equity)
	
	#####################
	# Regressions: SBBE #
	#####################
	
	cat('\n\n===========================================================\nMode 1: SBBE (intercepts) regressed on BAV Factors\n===========================================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD, data = equity, subset = !is.na(F_RelEstKnow), weights=1/sbbe_se)

	print(summary(m))
	
	equity[, ':=' (seccat_mc = seccat - mean(ifelse(is.na(F_RelEstKnow_STD),NA, seccat),na.rm=T),
				   newbrnd_mc = newbrnd - mean(ifelse(is.na(F_RelEstKnow_STD),NA, newbrnd),na.rm=T))]
	
	
	cat('\n\n===========================================================\nModel 2: SBBE (intercepts) regressed on BAV Factors and brand characteristics\n===========================================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD + seccat + newbrnd, data = equity, subset = !is.na(F_RelEstKnow), weights=1/sbbe_se)

	print(summary(m))

	cat('\n\n===========================================================\nModel 3: SBBE (intercepts) regressed on BAV Factors and brand characteristics with interactions\n===========================================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD + seccat_mc + newbrnd_mc +
			   F_RelEstKnow_STD * seccat_mc + F_EnergDiff_STD * seccat_mc +
			   F_RelEstKnow_STD * newbrnd_mc + F_EnergDiff_STD * newbrnd_mc, data = equity, subset = !is.na(F_RelEstKnow), weights=1/sbbe_se)
 
	print(summary(m))

	cat('\n\n===========================================================\nModel 4: Averaged SBBE intercepts, regressed on average BAV Factors and brand characteristics with interactions\n===========================================================\n')
	
	equity_avg = equity[!is.na(F_RelEstKnow_STD), lapply(.SD, mean), by=c('cat_name', 'brand_name'), .SDcols=c('sbbe_STD', 'F_RelEstKnow_STD', 'F_EnergDiff_STD', 'sbbe_se', 'seccat', 'newbrnd')]
	
	equity_avg[, ':=' (seccat_mc = seccat - mean(seccat),
				   newbrnd_mc = newbrnd - mean(F_RelEstKnow_STD))]
	
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD + seccat_mc + newbrnd_mc +
			   F_RelEstKnow_STD * seccat_mc + F_EnergDiff_STD * seccat_mc +
			   F_RelEstKnow_STD * newbrnd_mc + F_EnergDiff_STD * newbrnd_mc, data = equity_avg, weights=1/sbbe_se)
 
	print(summary(m))

	#############################
	# Regressions: Elasticities #
	#############################
	
	# select BAV brands only
	# compute mean-centered interaction effects
	
	signific = c(0.001, 0.01, .050, .1)
	names(signific) <- c('****', '***', '**', '*')
	options(signif.symbols=signific)

	elastreg<-function(form, weights, standardize=FALSE, dt, msg=NULL) {
		# do by variable
		if (!is.null(weights)) eval(parse(text=paste0('dt[, w:=', weights,']'))) else dt[, w:=1]
		vars = unique(dt$var_name)
		meta<-NULL
		for (j in seq(along=vars)) { #as.factor(cat_name) +
			meta[[j]] <- lm(as.formula(form), data= dt, subset= !is.na(dt)&var_name==vars[j], weights=w)#(1/elast_se))
			}
		#!elast_outlier & 
		require(car)
		require(memisc)
		
		m_all = paste0(paste0('\"',vars,'\"'),'=meta[[',seq(along=vars),']]')
		
		cat('\n\n\nMeta analysis')
		#print(eval(parse(text=paste0('mtable(', paste0(m_all, collapse=','), ',coef.style=\"all\")'))))
		print(eval(parse(text=paste0('mtable(', paste0(m_all, collapse=','), ')'))))
		
		#,signif.symbols=signific)
		# 
		cat(paste0('\n', msg, '\n'))
	
		cat('\nVIF values\n')
		for (j in seq(along=meta)) {
			cat('\n', vars[j], '\n')
			print(vif(meta[[j]]))
			}
	}

	cat('\n\n======================================\n')
	cat('======================================\nElasticities regressed on BAV Factors\n======================================\n')
	cat('======================================\n\n\n')
	
	cat('\n\nElasticities regressed on BAV Factors for all BAV brands\n=========================================================\n')
	uniq_br=elast[!is.na(bav_asset)][, list(.N),by=c('cat_name', 'brand_name')]
	elastreg('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se', dt=elast[!is.na(bav_asset)], msg=paste0('Models estimated with all ', nrow(uniq_br), ' BAV brands.'))
	
	#cat('\n\nElasticities regressed on BAV Factors for main-category BAV brands only\n=========================================================\n')
	#uniq_br=elast[seccat==0 & !is.na(bav_asset)][, list(.N),by=c('cat_name', 'brand_name')]
	#elastreg('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se', dt=elast[seccat==0& !is.na(bav_asset)], msg=paste0('Models estimated with ', nrow(uniq_br), ' brands in their main categories #(seccat==0).'))
	
	sink()
	
	############
	# PLOTTING #
	############
	
	source('summary_plots.R')

	
	}
