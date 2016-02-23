
### LOAD DATA SETS
require(data.table)
require(reshape2)
require(marketingtools)
require(car) # for delta method

load('..//..//derived//output//datasets.RData')
load('..//..//analysis//output//results.RData')

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)
	
# Load analysis code
	source('proc_analysis.R')
	
	unlink('..//output//*.txt')
	
	r=1
	sink('..//output//without_copulas.txt')
	
	for (i in 1:24) {
		res = all_results[[i]][[r]]
		if (!class(res)=='try-error') {
			(show(res)) } else {
			print(i)
			print('error')
			}
		
		}
	
	sink()
	
	
	r=2
	sink('..//output//with_copulas.txt')
	
	for (i in 1:24) {
		res = all_results[[i]][[r]]
		if (!class(res)=='try-error') {
			(show(res)) } else {
			print(i)
			print('error')
			}
		
		}
	
	sink()