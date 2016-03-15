
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

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)
	
# Load analysis code
	source('proc_analysis.R')
	
	
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
	
	models[, type := sapply(model_name, function(x) strsplit(x, split='_')[[1]][2])]
	models[, cat_index := sapply(model_name, function(x) strsplit(x, split='_')[[1]][1])]
	models[, decay := as.numeric(sapply(model_name, function(x) strsplit(x, split='_')[[1]][3]))]
	
# Select best-fitting models (min AIC)
	models[, ':=' (selected= max(llik) == llik), by=c('cat_index', 'type')]
	
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
		
# Store selection
	selected_models = models[type=='copula'&selected==TRUE]

################
# PRINT OUTPUT #
################
	source('proc_report.R')

	unlink('..//output//*.txt')
	
	# Report individual model results
	for (r in unique(selected_models$type)) {
		sel=selected_models[type==r]
		sink(paste0('..//output//estimates_', r, '.txt'))
	
		for (i in sel$index) {
		res = all_results[[i]]
		if (!'try-error'%in%class(res)) {
			(show(res)) } else {
			print(i)
			print('error')
			}
		
		}
	
		sink()
		
		sink(paste0('..//output//summary_', r, '.txt'))
		summ(all_results[sel$index])
		
		
		sink()
	
	}
