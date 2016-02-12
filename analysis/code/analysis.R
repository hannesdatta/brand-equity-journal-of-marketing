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
	
	
#allres <- NULL

#	for (i in c(1:8)) {
	
	i=1
	print(i)
	print(names(datasets)[i])
	
	#i=1
	dt <- datasets[[i]]
	
	#dt <- dt[grepl('Milwaukee', brand_name)]
	
	if(0){
	# identify outlier weeks: where pi_bt drops by 30%
	obs <- nrow(dt)
	for (j in 1:5) {
	dt[, lag_pi_bt := c(NA,pi_bt[-(.N)]), by=c('brand_id')]
	dt[, outlier := pi_bt<.9*lag_pi_bt, by=c('brand_id')]
	dt <- dt[outlier==F]
	obs<-c(obs,nrow(dt))
	}
	plot(obs, type='l')
	
	print(xyplot(pi_bt~week|brand_name,data=dt, auto.key=list(space="bottom", columns=2, 
						   title="Brands", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free"))))
						   
	print(xyplot(reg_pr_bt~week|brand_name,data=dt, auto.key=list(space="bottom", columns=2, 
						   title="Brands", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free"))))
	dt[, mshannes := sales_bt/sum(sales_bt),by=c('week')]
	dev.new()
	print(xyplot(mshannes~week|brand_name,data=dt, auto.key=list(space="bottom", columns=2, 
						   title="Brands", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free"))))
	
	print(xyplot(sales_bt~week|brand_name,data=dt, auto.key=list(space="bottom", columns=2, 
						   title="Brands", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free"))))
						   
	}
	
	# kick out first 4 observations by brand
	dt[, min_week := min(week),by=c('brand_name')]
	dt <- dt[week>min_week+3]
	
	# kick out first year
	dt <- dt[year>=2002]
	
	
	# identify promotional quantile: 
	#dt[, promo_quant := quantile(pi_bt, .05),by=c('brand_name')]
	#dt[, list(unique(promo_quant)),by=c('brand_name')]
	
	#dt <- dt[pi_bt>promo_quant]
	
	
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
		xvars_heterog <- c('promo_bt', 'ract_pr_bt', 'adstock_bt', 'pct_store_skus_bt')
		#'reg_pr_bt','', #'linelength_bt', pct_store_skus
	
		#xvars_heterog <- c(xvars_heterog, grep('cop[_]', colnames(dt),value=T))
		# define y variable, to be used to calculate market shares
		
		yvars <- c('sales_bt') 
		# define homogenous coefficients (e.g., brand attributes)
		xvars_homog <- c(grep('attr[_]', colnames(dt),value=T))
		# define index for brands and time (# indexes brands ([1]), and time ([2]))
		it <- c(brand='brand_name', time='week') 

	##################
	# TRANSFORM DATA #
	##################

	
#	source('proc_geometric_attraction.R')


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
	show(dtbb)
	
	
	# create dummies for all brands in all years they are available
	brands <- unique(dt$brand_name)

	for (br in brands) {
		yrs = unique(dt[brand_name==br]$year)
		
		for (yr in yrs) {
			dt[, paste0('dummy', '_', br, '_yr_', yr) := as.numeric(brand_name %in% br & year==yr)]
			}
		}

	# match new dummy matrix to transformed data set
	dummatrix <- dt[, c('brand_name','week', grep('^dummy[_]', colnames(dt),value=T)), with=F]
			
	X=dtbb@X[, -c(grep('dum[_]', colnames(dtbb@X)))]
	#
	X=dtbb@X
	Y=dtbb@y
	index=data.table(date=dtbb@period, brand=dtbb@individ)
	setkey(index, date, brand)
	setkey(dummatrix, week, brand_name)
	
	dummatch <- dummatrix[index]
	dummatch[, ':=' (brand_name=NULL, week=NULL)]
	#X<-as.matrix(data.frame(X, dummatch))
	
	# drop one indicator for benchmark brand
	
	ols=solve(t(X)%*%X)%*%t(X)%*%Y
	m <- itersur(X=dtbb@X,Y=as.matrix(dtbb@y), dates_brands=data.frame(date=dtbb@period,brand=dtbb@individ))

	m$coefficients$ols=ols
	m$coefficients$varname=m$coefficients$variable
	m$coefficients$brand_name<-unlist(lapply(strsplit(as.character(m$coefficients$varname), '_'), function(x) x[[1]][1]))
	m$coefficients$variable<-unlist(lapply(strsplit(as.character(m$coefficients$varname), '_'), function(x) paste0(x[-1], collapse='_')))
	m$coefficients$z <- m$coefficients$coef/m$coefficients$se
	m$coefficients$variable <- NULL
	
	print(m$sigma)
	m$coefficients
	

		if(0){

	
		
# merge dependent variable (y's), dummy variables,  with dummy variables
	setkey(widedt, week)
	longdt = merge(dt[, c('week', 'brand_name', 'dv_ms', grep(paste0('dummy', std_sep),colnames(dt),value=T)),with=F], widedt, by = 'week', all.x=T)

# Set NAs to ZERO (-> do not estimate coefficients)
	longdt[is.na(longdt)]<-0

# Delete one brand-year specific dummy
	longdt[, grep(paste0('dummy',std_sep), colnames(longdt), value=T)[1] := NULL, with=F]

	}
	
	

	########################
	# EXTRACT ELASTICITIES #
	########################

	# calculate mean variables
	means <- dt[, c('brand_name', xvars_heterog),with=F][, lapply(.SD, mean), by=c('brand_name'), .SDcols=c(xvars_heterog)]
	msaverage <- dt[, list(mean_ms = mean(ms)), by=c('brand_name')]
	meansmelt <- melt(means, id.vars=c('brand_name'))
	setnames(meansmelt, 'value','mean_var')

	elasticities <- merge(meansmelt, m$coefficients, by=c('variable', 'brand_name'), all.x=T, all.y=F)
	elasticities <- merge(elasticities, msaverage, by=c('brand_name'))

	elasticities[, elast := coef * (1-mean_ms) * mean_var]
	elasticities[, elast_se := se * (1-mean_ms) * mean_var]
	
	# summary
	selast <- elasticities[!is.na(coef), list(median_elast = median(elast), 
											  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
											  N_brands= .N, 
											  perc_positive = length(which(z>=(1.96)))/.N, 
											  perc_null = length(which(abs(z)<1.96))/.N, 
											  perc_negative = length(which(z<=(-1.96)))/.N), by=c('variable')]
	show(selast)
	

# summary table (by brands)
	{
	tmpx<-data.table(elasticities)
	try(setnames(tmpx, 'variable','varnamex'),silent=T)
	tmp=melt(tmpx, id.vars=c('brand_name', 'varname','varnamex', 'par'))
	print(dcast(tmp[varnamex%in%c('brand_name', xvars_heterog) & variable=='elast'], brand_name ~ varnamex))
	cat('\n\n')
	print(res$summary_elast[!grepl('cop[_]', variable)])
	}

		
	
	
#allres[[i]]<-res
	#}

#save(allres, file='..//output//results.RData')

	# compute VIFs
	#tmp=split(data.frame(res$X, res$dates_brands), res$dates_brands$brand)
	#vifs <- sapply(colnames(res$X), function(x) {
	#	m<-lm(as.matrix(res$X[,x, with=F])~1+as.matrix(res$X[, !colnames(res$X)%in%x,with=F]))
	#	1/(1-summary(m)$r.squared)
	#	})
	
	# Base brand:
	
	tmp <- split(data.frame(X), dtbb@individ)
	vifs<-lapply(tmp, function(X) {
	
	vifs <- sapply(colnames(X), function(x) {
		m<-lm(as.matrix(X[,x])~1+as.matrix(X[, !colnames(X)%in%x]))
		1/(1-summary(m)$r.squared)
		})
	
		return(vifs) 
	})

	vifs<-unlist(vifs)[!is.na(unlist(vifs))]
	
	vifs<-data.table(variable=names(vifs),vif=vifs)
	setorder(vifs, variable)
	
	vifs[, equation := sapply(variable, function(x) strsplit(x, '.',fixed=T)[[1]][1])]
	vifs[, coefficient := sapply(variable, function(x) strsplit(x, '.',fixed=T)[[1]][2])]
	vifs[, variable:=NULL]
	setcolorder(vifs, c('equation','coefficient','vif'))
	
#res=allres[[5]]



	
	
source('proc_plots.R')

	
	xyplot(coef~year|brand, data = res$brand_year_dummies, groups = grp,
		 upper = res$brand_year_dummies$upper, lower = res$brand_year_dummies$lower,
		 panel = function(x, y, ...){
		 panel.superpose(x, y, panel.groups = my.panel.bands, type='l', col='gray',...)
		 panel.xyplot(x, y, type='b', cex=0.6, lty=1,...)
		 }, main = c('Estimated SBBE with 1.96 x SE confidence bounds'))


	# summary(lm(log(sales_bt) ~ -1 + as.factor(brand_name) *log(reg_pr_bt), data = dt))

# save results
	#save(results_brands, results_category, markets, models, file='..\\output\\results.RData')
