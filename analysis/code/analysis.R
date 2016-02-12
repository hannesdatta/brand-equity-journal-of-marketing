#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

									  

### LOAD DATA SETS
require(data.table)
require(marketingtools)
require(reshape2)

load('..//..//derived//output//datasets.RData')


###########
# To do's #
###########

# Steps 
# 1: Enable estimation of homogenous coefficients for some attributes
# 2: Compute Copula's / remove insignificant ones



#########
# Setup #
#########

# Constants

	print(names(datasets))
	data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	length(datasets)
	
	
	run <- function(i, xvars_heterog = c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt', 'adstock_bt')) {
	#i=1
	print(i)
	print(names(datasets)[i])
	
	dt <- datasets[[i]]

	# kick out first 4 observations by brand
	dt[, min_week := min(week),by=c('brand_name')]
	dt <- dt[week>min_week+3]
	# keep only data for 2002 onwards
	dt <- dt[year>=2002]
	dt[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]
	
	# get out brands with equal number of observations
	#obs <- dt[,list(.N), by=c('brand_name')]
	#dt <- dt[brand_name%in%obs[N==572]$brand_name,]

	####################
	# DEFINE VARIABLES #
	####################

	# add copula controls
		dt[, cop_reg_pr_bt := make_copula(reg_pr_bt), by=c('brand_name')]
	
	# define heterogenous Xs (can be potentially endogenous; copula correction terms pooled or homogenous (--> HARALD)
		#xvars_heterog <- c('reg_pr_bt', 'ddepth_bt', 'adstock_bt', 'pct_store_skus')
		#xvars_heterog <- c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt', 'advertising_bt')
		#'reg_pr_bt','', #'linelength_bt', pct_store_skus
	
		#xvars_heterog <- c(xvars_heterog, grep('cop[_]', colnames(dt),value=T))
		# define y variable, to be used to calculate market shares
		
		yvars <- c('sales_bt') 
		# define homogenous coefficients (e.g., brand attributes)
		xvars_homog <- c(grep('attr[_]', colnames(dt),value=T))
		
		# check whether any of these attributes is zero for all brands, then remove this column
		attrsums = colSums(dt[, xvars_homog, with=F])
		if(any(attrsums==0)) {
			warning('Some of the product attributes are zero for all brands; removing variables ', paste0(names(attrsums)[which(attrsums==0)],collapse=' ,'))
			xvars_homog <- xvars_homog[-which(attrsums==0)]
			}
		
		# define index for brands and time (# indexes brands ([1]), and time ([2]))
		it <- c(brand='brand_name', time='week') 

	##################
	# TRANSFORM DATA #
	##################

	# check for variation in all variables by brand; if not available, set all to NA
	for (.n in c(xvars_heterog, yvars)) {
		dt[, get_n := length(unique(get(.n))),by=c('brand_name')]
		dt[get_n==1, .n := NA, with=F]
	
		dt[,':=' (get_n=NULL)]
		}
	cat('Unique values\n')
	dt[, lapply(.SD, function(x) length(unique(x[!is.na(x)]))), by=c('brand_name'), .SDcols=c(xvars_heterog, yvars)]
	
	###############################
	### NEW BASE BRAND APPROACH ###
	###############################
	
	source('attraction.R')
	source('itersur.R')
	source('attraction_data.R')
					
	dtraw = new("attr.raw")
	
	cat('Create data set structure\n')

	# calculate market share of yvar
	dt[, year:=as.numeric(year)]
	dt[, nbrands := length(unique(get(it[1]))), by = c(it[2])]
	dt[, ms := sales_bt/sum(sales_bt), by=c(it[2])]
	
	# Populate dt
	dtraw@X <- as.matrix(dt[,c(xvars_heterog, xvars_homog),with=F])
	dtraw@y <- as.numeric(dt$ms)
	dtraw@individ <- as.character(dt$brand_name)
	dtraw@period <- dt$week
	validObject(dtraw)
			
	show(dtraw)
	
	# convert to base-brand representation
	dtbb <- convertbb(dtraw, model='MNL', heterogenous = c(rep(1, length(xvars_heterog)), rep(0, length(xvars_homog))))
	#dtbb <- convertbb(dtraw, model='MNL', benchmark='Wisk',heterogenous = c(rep(1, length(xvars_heterog)), rep(0, length(xvars_homog))))
	
	show(dtbb)
	
	# create dummies for all brands in all years they are available
	brands <- unique(dt$brand_name)
	benchbrand = brands[which(!brands%in%unique(dtbb@individ))]
	
	for (.var in grep('^dummy[_]', colnames(dt),value=T)) dt[, .var:=NULL,with=F]
	
	for (br in brands[!brands%in%benchbrand]) {
		yrs = unique(dt[brand_name==br]$year)
		
		for (yr in yrs) {
			# do not create dummy variable for benchmark brand
			if (br==benchbrand) next# & yr == yrs[1]) next
			dt[, paste0('dummy', '_', br, '_yr_', yr) := as.numeric(brand_name %in% br & year==yr)]
			}
		}

	# match new dummy matrix to transformed data set
	dummatrix <- dt[, c('brand_name','week', grep('^dummy[_]', colnames(dt),value=T)), with=F]
	for (.var in grep('^dummy[_]', colnames(dt),value=T)) dt[, .var:=NULL,with=F]
	
	if(0) { # not identified 
	# create dummy matrix for benchmark brand, except in year 1
	for (yr in yrs[-1]) dt[, paste0('dummy', '_', benchbrand, '_yr_', yr) := as.numeric(year==yr)]
	bdummatrix <- dt[, c('brand_name','week', grep('^dummy[_]', colnames(dt),value=T)), with=F]
	setkey(bdummatrix, 'week')
	bdummatrix <- unique(bdummatrix)
	bdummatrix[, brand_name:=NULL]
	
	dummatrix <- merge(dummatrix, bdummatrix, by = c('week'))
	}
	
	X=dtbb@X[, -c(grep('[_]dum', colnames(dtbb@X)))]
	Y=dtbb@y
	index=data.table(week=dtbb@period, brand_name=dtbb@individ)

	dummatch <- merge(index, dummatrix, by=c('week','brand_name'),all.x=T, sort=F)
	dummatch[, ':=' (brand_name=NULL, week=NULL)]
	X<-as.matrix(data.frame(X, dummatch))
	
	
	#X=as.matrix(dtbb@X)
	
	# drop one indicator for benchmark brand
	# choice of base brand: put last.
	if (any(colSums(X)==0)) stop(paste0('Problems with no variation in variables: ', paste(colnames(X)[which(colSums(X)==0)], collapse = ', ')))
	
	#ols=solve(t(X)%*%X)%*%t(X)%*%Y
	#X2 = as(X, "dgeMatrix")
	#ols2 <- solve(crossprod(X2), crossprod(X2,Y))
	#ch <- chol(crossprod(X))
	#chol.sol <- backsolve(ch, forwardsolve(ch, crossprod(X, Y), upper = TRUE, trans = TRUE))

	m <- itersur(X=X,Y=as.matrix(dtbb@y), dates_brands=data.frame(date=dtbb@period,brand=dtbb@individ))

	m$coefficients$orig_var=m$coefficients$variable
	m$coefficients$variable <- NULL
	m$coefficients$z <- m$coefficients$coef/m$coefficients$se
	m$coefficients$brand_name<-unlist(lapply(strsplit(as.character(m$coefficients$orig_var), '_'), function(x) x[[1]][1]))
	m$coefficients$var_name<-unlist(lapply(strsplit(as.character(m$coefficients$orig_var), '_'), function(x) paste0(x[-1], collapse='_')))
	
	#print(m$sigma)
	#m$coefficients
	
	########################
	# EXTRACT ELASTICITIES #
	########################

	# calculate means of explanatory variables
	means <- dt[, c('brand_name', xvars_heterog),with=F][, lapply(.SD, mean), by=c('brand_name'), .SDcols=c(xvars_heterog)]
	meansmelt <- melt(means, id.vars=c('brand_name'))
	setnames(meansmelt, 'value','mean_var')
	setnames(meansmelt, 'variable','var_name')
	
	msaverage <- dt[, list(mean_ms = mean(ms)), by=c('brand_name')]
	
	elasticities <- merge(meansmelt, m$coefficients, by=c('var_name', 'brand_name'), all.x=T, all.y=F)
	elasticities <- merge(elasticities, msaverage, by=c('brand_name'))

	elasticities[, elast := coef * (1-mean_ms) * mean_var]
	elasticities[, elast_se := se * (1-mean_ms) * mean_var]
	
	# summary
	selast <- elasticities[!is.na(coef), list(median_elast = median(elast), 
											  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
											  N_brands= .N, 
											  perc_positive = length(which(z>=(1.96)))/.N, 
											  perc_null = length(which(abs(z)<1.96))/.N, 
											  perc_negative = length(which(z<=(-1.96)))/.N), by=c('var_name')]
	
	# summary table (by brands)
	#cat('\n\nElasticities by brands\n\n')
	tmpx<-data.table(elasticities)
	tmp=melt(tmpx, id.vars=c('brand_name', 'var_name','orig_var'))
	sumtable = (dcast(tmp[var_name%in%c('brand_name', xvars_heterog) & variable=='elast'], brand_name ~ var_name))


	return(list(cat_name = names(datasets)[i], model=m, elasticities = elasticities, summary_elasticities=selast, elast_by_brand=sumtable))
	
	}


### RUN MODEL FOR ALL CATEGORIES ###	
	focal_cats <- 1:3 #seq(along=datasets)
	all_results <- lapply(focal_cats, function(x) {
		cat('Running category ', x, '...\n')
		if (names(datasets[x])%in%c('laundet', 'beer')) {
			try(run(x, xvars_heterog=c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt')),silent=T) } else {
			try(run(x, xvars_heterog=c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt', 'adstock_bt')),silent=T) }
	
		})
		
	names(all_results) <- names(datasets)[seq(along=all_results)]
	
	
save(all_results, file = '../output/results.RData')
