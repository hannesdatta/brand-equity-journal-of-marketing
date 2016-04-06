

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
	
	cat('\n\n======================================\nSBBE (intercepts) regressed on BAV Factors\n======================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD, data = equity, weights=1/sbbe_se)

	print(summary(m))

	cat('\n\n======================================\nSBBE (market shares, not standardized) regressed on BAV Factors\n======================================\n')
	m<-lm(sbbems~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD, data = equity, weights=1/sbbe_se)

	print(summary(m))

	#############################
	# Regressions: Elasticities #
	#############################

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

	cat('\n\n======================================\nElasticities regressed on BAV Factors\n======================================\n')
	
	cat('\n\nElasticities regressed on BAV Factors for main-category BAV brands only\n======================================\n')
	uniq_br=elast[seccat==0 & !is.na(bav_asset)][, list(.N),by=c('cat_name', 'brand_name')]
	elastreg('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se', dt=elast[seccat==0& !is.na(bav_asset)], msg=paste0('Models estimated with ', nrow(uniq_br), ' brands in their main categories (seccat==0).'))
	
	cat('\n\nElasticities regressed on BAV Factors for all BAV brands\n======================================\n')
	uniq_br=elast[!is.na(bav_asset)][, list(.N),by=c('cat_name', 'brand_name')]
	elastreg('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se', dt=elast[!is.na(bav_asset)], msg=paste0('Models estimated with all ', nrow(uniq_br), ' BAV brands.'))
	
	sink()
	
	}

# Make summary plots
#	source('summary_plots.R')


#elastreg('elast ~ 1 + F_RelEstKnow + F_EnergDiff', weights='1/elast_se')

#elastreg('elast_STD ~ 1 + F_RelEstKnow_STD * secondary_cat + F_EnergDiff_STD * secondary_cat + F_RelEstKnow_STD * fooddrinks + F_EnergDiff_STD * fooddrinks', weights='1/elast_se')
#elastreg('elast ~ 1 + F_RelEstKnow * secondary_cat + F_EnergDiff * secondary_cat + F_RelEstKnow * fooddrinks + F_EnergDiff * fooddrinks', weights='1/elast_se')

#tmp=elast[var_name==vars[1]]
#with(tmp, cor(data.frame(ms, coef, elast, F_RelEstKnow_STD, F_EnergDiff_STD),use='complete.obs'))
