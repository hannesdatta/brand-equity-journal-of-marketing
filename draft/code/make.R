#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

### YEARS
cat('Summarize results\n')
	m<-all_results[[1]]$model
	m$coefficients <- data.table(m$coefficients)
	# retrieve years
	yrs <- m$coefficients[grepl('_yr_', orig_var)]
	yrs[, brand:=sapply(var_name, function(x) strsplit(x, '_')[[1]][1])]
	yrs[, year:=as.numeric(sapply(var_name, function(x) rev(strsplit(x, '_')[[1]])[1]))]
	
	yrs[,upper:=coef+1.96*se]
	yrs[,lower:=coef-1.96*se]
	yrs[, grp:=1]
	
	tmp=dcast(yrs, brand ~ year, value.var=c('coef'))
	
	write.table(tmp, "clipboard", sep="\t")
	


	
	
source('proc_plots.R')

	
	xyplot(coef~year|brand, data = res$brand_year_dummies, groups = grp,
		 upper = res$brand_year_dummies$upper, lower = res$brand_year_dummies$lower,
		 panel = function(x, y, ...){
		 panel.superpose(x, y, panel.groups = my.panel.bands, type='l', col='gray',...)
		 panel.xyplot(x, y, type='b', cex=0.6, lty=1,...)
		 }, main = c('Estimated SBBE with 1.96 x SE confidence bounds'))


