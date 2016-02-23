#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     


#################################
## PRODUCE TABLES WITH RESULTS  #
#################################

### LOAD DATA SETS
require(data.table)
require(marketingtools)
require(reshape2)

load('..//..//derived//output//datasets.RData')
load('..//..//analysis//output//results.RData')
source('xtable2.R')
source("auxilary.R")

# Load externals	
source('proc_rename.R')

# Load packages
	require(xtable)

	
	allres <- NULL
	
	allres[[1]] <- lapply(all_results, function(x) x[[1]])	
	allres[[2]] <- lapply(all_results, function(x) x[[2]])	
	
	checked_all <- lapply(allres, function(x) which(unlist(lapply(x, class))=='list'))
	
	
	
	
	
	#results <- all_results
	
#res <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)), error = unlist(lapply(results, class)))
#checked = res$index[which(res$error=='list')]

if (0){
# Prepare panel data
	brand_panel=rbindlist(lapply(all_data, function(x) rbindlist(x$data_cleaned)))
	setorder(brand_panel, market_id, category,country,brand,date)
	
	category_panel = rbindlist(lapply(all_data, function(x) rbindlist(x$data_category)))
	setorder(category_panel, market_id, category,country,date)


# Check models for completeness / crashes
checks_brands <- NULL
for (i in seq(along=results_brands)) {
	check = unlist(lapply(results_brands[[i]], function(x) {
	if(class(x)=='try-error') return('error')
	if(class(x)=='list' & length(x$error)>0) return('small_N')
	return('ok')
	}))
	checks_brands[[i]]<-check
	}
	

	checks_category <- NULL
for (i in seq(along=results_category)) {
	check = unlist(lapply(results_category[[i]], function(x) {
	if(class(x)=='try-error') return('error')
	if(class(x)=='list' & length(x$error)>0) return('small_N')
	return('ok')
	}))
	checks_category[[i]]<-check
	}
	}
