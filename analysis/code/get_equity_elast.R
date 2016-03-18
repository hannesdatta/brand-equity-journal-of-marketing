#################################################
# EXTRACT DATA SET WITH EQUITY AND ELASTICITIES #
#################################################

extract_elast_equity <- function(tmp_results) {

	equity=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$equity)))
	setcolorder(equity, c('cat_name','brand_name','year','sbbe', 'sbbe_se', 'bav_relevance', 'bav_esteem','bav_knowledge', 'bav_energizeddiff', 'bav_asset'))
	
	# extract extra variables
	othervars=rbindlist(lapply(datasets, function(x) x[year>=2002, list(unitsales=sum(sales_bt,na.rm=T), revenue=sum(rev_bt,na.rm=T), price = mean(act_pr_bt,na.rm=T)),by=c('cat_name','brand_name', 'year')]))
	othervars[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]
	
	equity <- merge(equity, othervars,by=c('cat_name','brand_name', 'year'),all.x=T,all.y=F)
	
	equity[, rev_total := sum(revenue),by=c('cat_name', 'brand_name')]
	equity[, max_cat := rev_total==max(rev_total),by=c('brand_name')]
	equity[, ':=' (max_cat=NULL, rev_total=NULL)]

	meanequity = equity[, lapply(.SD, mean, na.rm=T), by=c('cat_name', 'brand_name'), .SDcols=grep('bav[_]', colnames(equity),value=T)]
	setkey(meanequity, cat_name, brand_name)
	
	# retrieve parameter estimates
	elast=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$elasticities)))
	elast[,var_name := gsub('adstock[0-9]*', 'adstock', var_name)]
	elast = elast[!grepl('cop[_]', var_name)]
	
	setkey(elast, cat_name, brand_name)
	elast=meanequity[elast]

	elast=elast[, !colnames(elast)%in%c('z', 'orig_var'),with=F] #'mean_var','coef','se',  #'mean_ms'
	return(list(elast=elast, equity=equity, meanequity=meanequity))
	
	}

tmp = extract_elast_equity(all_results[sel_models])

equity = tmp$equity
elast = tmp$elast

###################
# Factor analysis #
###################

library(psych)
bav_dims =  c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff')
	
	# Equity brand-value metrics (i.e., by year and brand)
		setkey(equity, brand_name, year)
		mydata=unique(equity)
	
		fit <- principal(mydata[, bav_dims,with=F], nfactors=2, rotate="varimax")
		fit_scores <- cbind(mydata[, c('brand_name', 'year'),with=F], fit$scores)
		setkey(fit_scores, brand_name, year)
		equity[fit_scores, ':=' (F_RelEstKnow=i.PC1, F_EnergDiff=i.PC2)]
		equity[, var_name:='none']
		
	# Elasticities
		setkey(elast, brand_name)
		mydata=unique(elast)
		fit2 <- principal(mydata[, bav_dims,with=F], nfactors=2, rotate="varimax")
		fit_scores2 <- cbind(mydata[, c('brand_name'),with=F], fit2$scores)
		setkey(fit_scores2, brand_name)
		elast[fit_scores2, ':=' (F_RelEstKnow=i.PC1, F_EnergDiff=i.PC2)]
	


# Standardize variables
	stdvar <- function(x) (x-mean(x,na.rm=T))/sd(x, na.rm=T)
	
	# Equity / elasticitiy
	tmp=lapply(list(equity, elast), function(df) {
		
		for (.var in colnames(df)) {
			if (.var %in% c('year', 'cat_name', 'brand_name', 'var_name', grep('bav[_]', colnames(df),value=TRUE))) next #grep('F[_]', colnames(df),value=TRUE)
			# check whether column is dummy: do not meancenter
			if (all(unique(unlist(df[, .var, with=F]))%in%c(1,0))) next
			# varies by category
			if (all(unlist(df[, list(N=length(unique(get(.var)))), by=c('cat_name', 'var_name')]$N)==1)) next
			
			df[, paste0(.var, '_STD') := stdvar(get(.var)), by=c('cat_name', 'var_name'),with=F]
			}
		return(df)
		})
