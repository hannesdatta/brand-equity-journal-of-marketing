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
	std_sep = '@' # character used to separate brand names from variable names

	print(names(datasets))
	data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	length(datasets)
	
	
#allres <- NULL

#	for (i in c(1:8)) {
	
	i=2
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
		xvars_homog <- c() #c(grep('attr[_]', colnames(dt),value=T))[1]
		# define index for brands and time (# indexes brands ([1]), and time ([2]))
		it <- c(brand='brand_name', time='week') 

	##################
	# TRANSFORM DATA #
	##################

	source('proc_geometric_attraction.R')

	# check for variation in all variables by brand; if not available, set all to NA
	for (.n in c(xvars_heterog, yvars)) {
		dt[, get_n := length(unique(get(.n))),by=c('brand_name')]
		dt[get_n==1, .n := NA, with=F]
	
		dt[,':=' (get_n=NULL)]
		}
	cat('Unique values\n')
	dt[, lapply(.SD, function(x) length(unique(x[!is.na(x)]))), by=c('brand_name'), .SDcols=c(xvars_heterog, yvars)]
	
	res <- run()
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
	
	
	# all eq	
	vifs <- sapply(colnames(res$X), function(x) {
		m<-lm(as.matrix(res$X[,x, with=F])~1+as.matrix(res$X[, !colnames(res$X)%in%x,with=F]))
		1/(1-summary(m)$r.squared)
		})
	


#res=allres[[5]]


# summary table (by brands)
	{
	tmpx<-res$elasticities
	try(setnames(tmpx, 'variable','varname'),silent=T)
	tmp=melt(tmpx, id.vars=c('brand_name', 'varname', 'orig_var'))
	print(dcast(tmp[varname%in%c('brand_name', xvars_heterog) & variable=='elast'], brand_name ~ varname))
	cat('\n\n')
	print(res$summary_elast[!grepl('cop[_]', variable)])
	}

	
	
	
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
