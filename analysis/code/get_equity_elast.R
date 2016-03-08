#################################################
# EXTRACT DATA SET WITH EQUITY AND ELASTICITIES #
#################################################

source('proc_report.R')
source('proc_metadata.R') #--> meta_char

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
		
