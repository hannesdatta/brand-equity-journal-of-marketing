#################################################
# EXTRACT DATA SET WITH EQUITY AND ELASTICITIES #
#################################################

source('proc_metadata.R') # get meta characteristics

extract_equity <- function(tmp_results) {

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
	
	# retrieve parameter estimtaes
	elast=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$elasticities)))
	elast[,var_name := gsub('adstock[0-9]*', 'adstock', var_name)]
	elast = elast[!grepl('cop[_]', var_name)]
	
	setkey(elast, cat_name, brand_name)
	elast=meanequity[elast]

	elast=elast[, !colnames(elast)%in%c('z', 'orig_var', 'mean_ms'),with=F] #'mean_var','coef','se', 
	return(list(elast=elast, equity=equity, meanequity=meanequity))
	
	}

tmp = extract_equity(all_results[selected_models$index])
	
# Extract final elasticities
# equity data set
equity = merge(tmp$equity, meta_char, by=c('cat_name', 'brand_name'),all.x=T,all.y=F)
elast = merge(tmp$elast, meta_char, by = c('cat_name', 'brand_name'), all.x=T, all.y=F)

# Add factor analysis
	# Equity
	library(psych)
	mydata=equity[, c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff'),with=F]
	fit <- principal(mydata, nfactors=3, rotate="varimax")
	fit
	
	equity <- cbind(equity, fit$scores)

	setnames(equity, 'PC1', 'F_RelEst')
	setnames(equity, 'PC3', 'F_Knowledge')
	setnames(equity, 'PC2', 'F_EnergDiff')

	# Elasticities
	mydata=elast[, c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff'),with=F]
	fit2 <- principal(mydata, nfactors=3, rotate="varimax")
	fit2
	
	elast <- cbind(elast, fit2$scores)

	setnames(elast, 'PC1', 'F_RelEst')
	setnames(elast, 'PC3', 'F_Knowledge')
	setnames(elast, 'PC2', 'F_EnergDiff')

	# Elasticities
	mydata=elast[, c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff'),with=F]
	fit2b <- principal(mydata, nfactors=2, rotate="varimax")
	fit2b
	
	elast <- cbind(elast, fit2b$scores)

	setnames(elast, 'PC1', 'F2_RelEstKnow')
	setnames(elast, 'PC2', 'F2_EnergDiff')

	
# Standardize variables
	stdvar <- function(x) (x-mean(x,na.rm=T))/sd(x, na.rm=T)
	
	# Equity
	for (.var in colnames(equity)) {
		if (.var %in% c('year', 'cat_name', 'brand_name')) next
		# check whether column is dummy: do not meancenter
		if (all(unique(unlist(equity[, .var, with=F]))%in%c(1,0))) next
		# varies by category
		if (all(unlist(equity[, list(N=length(unique(get(.var)))), by=c('cat_name')]$N)==1)) next
		
		equity[, paste0(.var, '_STD') := stdvar(get(.var)), by=c('cat_name'),with=F]
		}
	
	# Elasticities
	for (.var in colnames(elast)) {
		if (.var %in% c('year', 'cat_name', 'brand_name', 'var_name')) next
		# check whether column is dummy: do not meancenter
		if (all(unique(unlist(elast[, .var, with=F]))%in%c(1,0))) next
		# varies by category
		if (all(unlist(elast[, list(N=length(unique(get(.var)))), by=c('cat_name')]$N)==1)) next
		
		elast[, paste0(.var, '_STD') := stdvar(get(.var)), by=c('cat_name'),with=F]
		}
		
