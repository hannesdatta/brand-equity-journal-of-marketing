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

	unlink('..//output//*.txt')
	
	selected_models[, type_and_attr_type := paste(attr_type,type, varspec, sep='_')]
	
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
		
		source('get_equity_elast.R')
		source('proc_metadata.R') # get meta characteristics
	
		# Merge equity and elasticities with brand- and category-level characteristics
			equity = merge(equity, meta_char, by=c('cat_name', 'brand_name'),all.x=T,all.y=F)
			elast = merge(elast, meta_char, by = c('cat_name', 'brand_name'), all.x=T, all.y=F)
			
		# Assertions
			elast[, lapply(.SD, mean, na.rm=TRUE),by=c('cat_name', 'var_name'), .SDcols=c('F_RelEstKnow_STD', 'F_EnergDiff_STD')]
			# should be near-to-zero
			equity[, lapply(.SD, mean, na.rm=TRUE),by=c('cat_name', 'var_name'), .SDcols=c('F_RelEstKnow_STD', 'F_EnergDiff_STD')]
		
		# Write data files to disk
			# CSV
			cpath = paste0(fpath, '')
			#cpath = paste0(fpath, '//csv')
			dir.create(cpath)
			write.table(equity, paste0(cpath, '//equity.csv'), sep='\t', row.names=F,na = "")
			write.table(elast, paste0(cpath, '//elasticities.csv'), sep='\t', row.names=F,na = "")
			#[!is.na(bav_asset)]
			
			# SPSS
			require(sjmisc)
			#cpath = paste0(fpath, '//spss')
			dir.create(cpath)
			write_spss(equity, paste0(cpath, '//equity.sav'))
			write_spss(elast, paste0(cpath, '//elasticities.sav'))
			
			# SAS
			require(foreign)
			write.foreign(equity, paste0(cpath, '//equity.txt'), paste0(cpath, '//equity.sas'), package="SAS")
			write.foreign(elast, paste0(cpath, '//elasticities.txt'), paste0(cpath, '//elasticities.sas'), package="SAS")
			
		
	}
