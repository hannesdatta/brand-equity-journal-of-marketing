### LOAD DATA SETS
require(data.table)
require(reshape2)
require(marketingtools)
require(car) # for delta method and VIFs
library(Hmisc)
options(width=600)

fn_results = '..//..//analysis//output//results_processed.RData'
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

############################
# PRINT AND PROCESS OUTPUT #
############################

	# wipe subfolders in output directory
	.dirs = list.dirs('../output',recursive=F)
	for (.dir in .dirs) unlink(.dir, recursive=T)
	
	.files = list.files('../output', full.names=T)
	.files = grep('result.*', .files, value=T, invert = T)
	
	for (.file in .files) unlink(.file)
	
	selected_models[, type_and_attr_type := paste(attr_type,type, varspec, meancentering, sep='_')]
	
	# Report individual model results
	for (r in unique(selected_models$type_and_attr_type)) {
		sel=selected_models[type_and_attr_type==r]

		# Clean and create directory
		fpath = paste0('..//output//', r)
		unlink(fpath,recursive=TRUE)
		dir.create(fpath)
		
		##########################################
		# Print report with each model's results #
		##########################################
		
		sink(paste0('..//output//', r, '//estimates_', r, '.txt'))
	
		for (i in sel$index) {
		res = all_results[[i]]
		if (!'try-error'%in%class(res)) {
				(show(res)) } else {
				print(i)
				print('error')
				}
			}
		sink()
		
		###########################
		# Summarizing all results #
		###########################
		
		sel_models=sel$index
		
		sink(paste0('..//output//', r, '//factoranalysis_', r, '.txt'))
		source('get_equity_elast.R')
		sink()
		
		source('proc_metadata.R') # get meta characteristics
	
		# Merge equity and elasticities with brand- and category-level characteristics
			equity = merge(equity, meta_char, by=c('cat_name', 'brand_name'),all.x=T,all.y=F)
			elast = merge(elast, meta_char, by = c('cat_name', 'brand_name'), all.x=T, all.y=F)
		
		# Clean _mc from variable names
		elast[, var_name := gsub('[_]mc', '', var_name)]
		
		# Assertions
			elast[, lapply(.SD, mean, na.rm=TRUE),by=c('cat_name', 'var_name'), .SDcols=c(grep('F[0-9][_]', colnames(equity), value=T))]
			# standardized variables should be near-to-zero
			equity[, lapply(.SD, mean, na.rm=TRUE),by=c('cat_name', 'var_name'), .SDcols=c(grep('F[0-9][_]', colnames(equity), value=T))]
		
		# Output category-level means/sds for variables
		sink(paste0(fpath, '//category_means.txt'))
		
		means <- elast
		setkey(means, cat_name)
		means <- unique(means)
		
		.vars <- setdiff(c(grep('cat[_]', colnames(means),value=T), 'c2', 'c3', 'c4'), 'cat_name')
		tmp <- cbind(t(means[, lapply(.SD, mean),.SDcols=.vars]),t(means[, lapply(.SD, sd),.SDcols=.vars]))
		colnames(tmp) <- c('mean','sd')
		
		print(tmp)
		
		sink()
		
		
		
		# Write data files to disk
			# CSV
			write.table(equity, paste0(fpath, '//equity.csv'), sep='\t', row.names=F,na = "")
			write.table(elast, paste0(fpath, '//elasticities.csv'), sep='\t', row.names=F,na = "")
			#[!is.na(bav_asset)]
			
			# SPSS
			require(sjmisc)
			write_spss(equity, paste0(fpath, '//equity.sav'))
			write_spss(elast, paste0(fpath, '//elasticities.sav'))
			
			# SAS
			# require(foreign)
			# write.foreign(equity, paste0(cpath, '//equity.txt'), paste0(cpath, '//equity.sas'), package="SAS")
			# write.foreign(elast, paste0(cpath, '//elasticities.txt'), paste0(cpath, '//elasticities.sas'), package="SAS")

	}
