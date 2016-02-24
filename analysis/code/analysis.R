#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

									  

### LOAD DATA SETS
load('..//..//derived//output//datasets.RData')

init <- function() {
		require(marketingtools)
		require(reshape2)
		require(data.table)
		require(car)
		source('proc_analysis.R')
		}
		
init()	

# Enable cluster estimation	
	require(parallel)
	cl <- makePSOCKcluster(10)

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)
	
# Example for one category
	
#	if(0){ #cluster estimation for prototyping
	
		clusterExport(cl, c('prepare_data', 'datasets', 'init', 'analyze_marketshares'))
		void<-clusterEvalQ(cl, init())
		
		estimorder = overview$index[order(overview$nobs,decreasing=T)] # largest cat first
		names(estimorder) <- rownames(overview)[order(overview$nobs,decreasing=T)]
		
		all_results <-parLapply(cl, estimorder, function(i) {
					dt <- prepare_data(i)
					
					v1=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt','adstock50_bt'), simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL),silent=T)
					v2=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt','adstock50_bt'), simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL),silent=T)
					return(list(v1,v2))
				})
		
		
	names(all_results) <- names(estimorder)

	
	#	errors = estimorder[c(1,2,4,10,14,18)]
	#}
	
	if(0) { # estimation of a single category
	
		i=21
		init()
		dt <- prepare_data(i)
		out=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt','adstock50_bt'), simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL),silent=T)
		
		outendog=analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt'),
									 xvars_endog = c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt'), simpleDummies=FALSE,method="FGLS-Praise-Winsten")
		
		show(out)
		show(outendog)
	}
	
####################################
### RUN MODEL FOR ALL CATEGORIES ###
####################################
if(0){
	focal_cats <- seq(along=datasets)
	#focal_cats <- 21:22
	vars <- c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt')
	
	all_results <- lapply(focal_cats, function(x) {
		cat('Running category ', x, '...\n')
		dt<-prepare_data(x)
		benchmark=NULL
			
		v1=	try(analyze_marketshares(dt, 
									 xvars_heterog=c(vars),
									 simpleDummies=FALSE, 
									 attributes=TRUE, 
									 benchmark=benchmark,
									 method = "FGLS-Praise-Winsten"), silent=T)
		v2=	try(analyze_marketshares(dt, 
									 xvars_heterog=c(vars), 
									 xvars_endog = c(vars), 
									 simpleDummies=FALSE, 
									 attributes=TRUE, 
									 benchmark=benchmark,
									 method = "FGLS-Praise-Winsten"),silent=T)
		
		return(list(v1,v2))
		})

		
		
	names(all_results) <- names(datasets)[seq(along=all_results)]
}
####################
### SAVE RESULTS ###
####################
	
	save(all_results, file = '../output/results.RData')
