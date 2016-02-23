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
require(reshape2)
require(marketingtools)
require(car) # for delta method

load('..//..//derived//output//datasets.RData')

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)
	
# Load analysis code
	source('proc_analysis.R')
	
# Example for one category
	i=24
	dt <- prepare_data(i)
	
	# laundry
	#dt[brand_name%in%c('Fab','XTRA','SUN','Surf', 'EraLaundryProducts'), adstock_bt:=NA]
	
	#out=analyze_marketshares(dt, xvars_heterog=c('pct_store_skus_bt', 'pi_bt','rreg_pr_bt','adstock_bt'), simpleDummies=FALSE,attributes=TRUE,method="FGLS")
	#out=analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt'), simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= 'Purex')
	
		
	#te=dcast(dt, week ~ brand_name, value.var='adstock_bt')
	#xyplot(value~week|variable, data=data.table(melt(te, id.vars='week'))[order(week)],type='l')
	
	
	#xyplot(adstock_bt~week|brand_name,data=dt,type='l')
	#out=analyze_marketshares(dt, simpleDummies=FALSE,method="FGLS")
	
	out=analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt','adstock_bt'), simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL)#'ScottPaperGoods')
	
	outendog=analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock_bt'),
								 xvars_endog = c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock_bt'), simpleDummies=FALSE,method="FGLS-Praise-Winsten")
								 
	show(out)
	show(outendog)

####################################
### RUN MODEL FOR ALL CATEGORIES ###
####################################

	focal_cats <- seq(along=datasets)
	#focal_cats <- 21:22
	vars <- c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt')
	
	all_results <- lapply(focal_cats, function(x) {
		cat('Running category ', x, '...\n')
		dt<-prepare_data(x)
		benchmark=NULL
		
		catn = names(datasets)[x]
		if (catn=='beer') benchmark='Coors'
		if (catn=='toitisu') dt[brand_name=='MD', adstock_bt:=0]
		
		if (names(datasets[x])%in%c('laundet')) {
			vars_estim = vars } else {
			vars_estim = c(vars,'adstock_bt')
			}
		
		v1=	try(analyze_marketshares(dt, 
									 xvars_heterog=c(vars_estim),
									 simpleDummies=FALSE, 
									 attributes=TRUE, 
									 benchmark=benchmark,
									 method = "FGLS-Praise-Winsten"), silent=T)
		v2=	try(analyze_marketshares(dt, 
									 xvars_heterog=c(vars_estim), 
									 xvars_endog = c(vars), 
									 simpleDummies=FALSE, 
									 attributes=TRUE, 
									 benchmark=benchmark,
									 method = "FGLS-Praise-Winsten"),silent=T)
		
		return(list(v1,v2))
		})

	names(all_results) <- names(datasets)[seq(along=all_results)]

####################
### SAVE RESULTS ###
####################
	
	save(all_results, file = '../output/results.RData')
