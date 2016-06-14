### LOAD DATA SETS
require(data.table)
require(reshape2)
require(marketingtools)
require(car) # for delta method and VIFs
library(Hmisc)
options(width=600)

fn_results = '..//..//analysis//output//results.RData'
fn_data = '..//..//derived//output//datasets.RData'

for (fn in c(fn_data, fn_results)) {
	cat(fn, as.character(file.info(fn)$mtime),'\n')
	load(fn)
	}
	
# Load analysis code
	source('proc_analysis.R')

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)

###################
# MODEL SELECTION #
###################

# Prepare model selection (retrieve BIC/AIC)
	models = rbindlist(lapply(all_results, function(x) data.frame(error=!'bav_attraction' %in% class(x))))
	models[, ':=' (index=1:nrow(models), model_name = names(all_results))]
	models <- models[!error==T]
	
	models_meta = rbindlist(lapply(all_results[models$index], function(x) data.frame(error=!'bav_attraction' %in% class(x), aic=x$model$aic, bic=x$model$bic, llik = x$model$llik, est_minutes = x$model$elapse_minutes)))
	models_meta[, ':=' (index=models$index, model_name = names(all_results[models$index]))]
	models <- models_meta
	
	models[, cat_index := sapply(model_name, function(x) strsplit(x, split='_')[[1]][1])]
	models[, type := sapply(model_name, function(x) strsplit(x, split='_')[[1]][2])]
	models[, decay := as.numeric(sapply(model_name, function(x) strsplit(x, split='_')[[1]][3]))]
	models[, attr_type := sapply(model_name, function(x) strsplit(x, split='_')[[1]][4])]
	models[, varspec := sapply(model_name, function(x) strsplit(x, split='_')[[1]][5])]
	models[, meancentering := sapply(model_name, function(x) strsplit(x, split='_')[[1]][6])]
	
# Select best-fitting models (min AIC)
	models[, ':=' (selected= max(llik) == llik), by=c('cat_index', 'attr_type', 'type', 'varspec', 'meancentering')]
	
# Plot decay patterns
	path='..//audit//decay_selection//'
	dir.create(path)
	require(lattice)
	
	for (i in unique(models$cat_index)) {
		tmp=models[cat_index==i]
		cat_name=rownames(overview)[match(i, overview$index)]
		png(paste0(path, cat_name, '.png'), res=200, units='in', height=8, width=12)

		print(xyplot(llik+aic+bic~decay, groups=type,dat=tmp, 
		main=paste0(cat_name, ' (id ', i, ')'),
		scales = list(y = list(relation = "free")), type='l', auto.key=TRUE))
		dev.off()	   
		
		}

# Select models for reporting
	selected_models = models[selected==TRUE]

##################
# COMPUTE EQUITY #
##################

	# Compute equity on selected models

	require(parallel)
	cl <- makePSOCKcluster(12)

	void<-clusterEvalQ(cl, source('proc_analysis.R'))

	comp_index = selected_models$index
	equity <- clusterApplyLB(cl, all_results[comp_index], compute_equity)

	for (i in seq(from=1, to=length(equity))) {
		all_results[[comp_index[i]]]$equity <- equity[[i]]
		}

########
# SAVE #
########

save(all_results, selected_models, file = '../output/results_processed.RData')

