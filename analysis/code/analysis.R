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

load('..//temp//datasets.RData')


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

# Set data set
	dt <- datasets

# get out brands with equal number of observations
	#obs <- dt[,list(.N), by=c('brand_name')]
	#dt <- dt[brand_name%in%obs[N==572]$brand_name,]

# define variables #'pct_store_skus', 'AdStock_bt'
	xvars_heterog <- c('reg_pr_bt', 'pct_store_skus') # can be potentially endogenous; copula correction terms pooled or homogenous? (--> Harald)
	yvars <- c('sales_bt') # to be used to calculate market shares
	xvars_homog <- c('attr_packet_bt') #, 'attr_liquid_bt', 'attr_granul_bt', 'attr_aspartame_bt', 'attr_sucralose_bt', 'attr_agave_bt','attr_saccharin_bt') #grep('attr_', colnames(dt), value=T) # variable names which are homogenous
	it <- c(brand='brand_name', time='week') # indexes brands ([1]), and time ([2])

	# investigate why price coef. is pos.
#	summary(lm(log(sales_bt) ~ 1 + as.factor(brand_name) + log(reg_pr_bt), data = dt))

run <- function(){
	#'pi_bt', 
	
#######################################
# DATA PREPARATION FOR SUR ESTIMATION #
#######################################

# calculate market share of yvar
	dt[, year:=as.numeric(year)]
	dt[, nbrands := length(unique(get(it[1]))), by = c(it[2])]
	dt[, ms := sales_bt/sum(sales_bt), by=c(it[2])]
	dt[, ':=' (log_ms = log(ms))]
	dt[, ':=' (geom_ms = sum(log(ms))/nbrands), by = c(it[2])]

	dt[, dv_ms := log_ms - geom_ms]
	# -> the DV is labeled 'dv_ms'
	
# create dummies for all brands (years later on)
	brands <- unique(dt$brand_name)
#	for (br in brands) {
#		dt[, paste0('dummy',std_sep, br) := as.numeric(brand_name %in% br)]
#		}

# create dummies for all brands (for T-1 years)
#if(0){
	for (br in brands) {
		yrs = unique(dt[brand_name==br]$year)
		
		for (yr in yrs) {
			dt[, paste0('dummy', std_sep, br, '_yr_', yr) := as.numeric(brand_name %in% br & year==yr)]
			}
		}
#}

# set up the structure of the data set: 
# - stacked Y's
# - repeated X's
# e.g.
#
# y11   x11 x21 ...
# y12   x12 x22
# y13   x13 x23
# y21   x11 x21
# y22   x12 x22
# y23   x13 x23
# ...
#

# y's: are already of the correct structure, as in dt
# dummy variables: are also of the correct structure, as in dt

# x's: prepare wide version of xvars_heterog columns
	dtmelt <- melt(dt[,colnames(dt) %in% c(xvars_heterog, xvars_homog, it, 'year'),with=F], id.vars=c(it, 'year'))
	widedt=dcast.data.table(dtmelt[variable%in%c(xvars_heterog, xvars_homog)], week + year ~ brand_name + variable, sep = std_sep)
	rm(dtmelt)

# merge dependent variable (y's), dummy variables,  with dummy variables
	setkey(widedt, week)
	longdt = merge(dt[, c('week', 'brand_name', 'dv_ms', grep(paste0('dummy', std_sep),colnames(dt),value=T)),with=F], widedt, by = 'week', all.x=T)

# Set NAs to ZERO (-> do not estimate coefficients)
	longdt[is.na(longdt)]<-0

# Delete one dummy
	longdt[, grep(paste0('dummy',std_sep), colnames(longdt), value=T)[1] := NULL, with=F]

# Apply variable transformations
	longdt[, nbrands:=length(unique(brand_name)), by=c('week')]

	setorder(longdt, brand_name, week)
	suppressWarnings(longdt[, grep('transf[_]', colnames(longdt),value=T) := NULL, with=F])

# Transform explanatory variables
# *own* variables need to be premultiplied by 1-1/nbrands, 
# *cross-variables* are to be multiplied by -1/m).
# For heterogenous coefficients, create a *new* column for each brand/variable combination
# For brand-specific dummy variables, create a *new* column for each brand/variable combination (these are heterogenous (by brands) as well)
# For homogenous coefficients, apply the same transformation, but sum up the variable in ONE column (per variable).

	for (.var in c(xvars_heterog,xvars_homog, grep(paste0('dummy', std_sep),colnames(longdt),value=T))) { # iterates through all xvars_heterog
		
		.cols <- grep(paste0(std_sep,.var), colnames(longdt),value=T, fixed=T)
		.brands <- sapply(strsplit(.cols, std_sep) , function(x) x[1])
		
		# if dummy variable
		if (grepl('dummy',.var)) .cols = .var
		
		# Transform all columns by * -1/nbrands.
		# Overwrite own brand column, by multiply its original value by 1-1/nbrands
		for (.br in seq(along=.cols)) {
			longdt[, paste0('transf_', .cols[.br]) := (-1/nbrands)*get(.cols[.br]), with=F]
			longdt[which(brand_name==.brands[.br]), paste0('transf_', .cols[.br]) := (1-1/nbrands)*get(.cols[.br]), with=F]
			}
		
		if (.var%in%c(xvars_homog)) { # homogenous response
			# lump all of the columns together
			tmp = rowSums(longdt[, paste0('transf_', .cols),with=F])
			longdt[, paste0('transf_', .var) := tmp]
			longdt[, paste0('transf_', .cols) := NULL]
			}

		}

####################
# MODEL ESTIMATION #
####################

	X <- longdt[, grep('transf[_]', colnames(longdt),value=T),with=F]
	
	# drop zero-value colums (e.g., ad stock for some brands)
	colnames(X)[colSums(X)==0]

	X<-X[,abs(colSums(X))>0,with=F]
	
	setnames(X, gsub('transf[_]', '',colnames(X)))
	dates_brands <- longdt[, c('brand_name', 'week'),with=F]
	setnames(dates_brands, c('brand', 'date'))
	Y <- longdt[, 'dv_ms',with=F]

	print(dim(X))
	
	m <- sur(as.matrix(X),as.matrix(Y),dates_brands)
	m$coefficients[, orig_var := variable]
	m$coefficients[, z := coef/se] # -> retrieved coefficients
	m$coefficients[, brand_name:=sapply(variable, function(x) strsplit(x, std_sep)[[1]][1])]
	m$coefficients[, variable:=sapply(variable, function(x) strsplit(x, std_sep)[[1]][2])]
	


############################
# VISUALIZE SBBE OVER TIME #
############################
	
	# retrieve years
	yrs <- m$coefficients[grepl('_yr_', orig_var)]
	yrs[, brand:=sapply(orig_var, function(x) strsplit(strsplit(x, std_sep)[[1]][2], '_')[[1]][1])]
	yrs[, year:=as.numeric(sapply(orig_var, function(x) rev(strsplit(x, '_')[[1]])[1]))]
	
	require(lattice)
	require(latticeExtra)
	# glayer 
	# code by http://www.r-bloggers.com/confidence-bands-with-lattice-and-r/
	
	my.panel.bands <- function(x, y, upper, lower, fill, col,
		 subscripts, ..., font, fontface)
		 {
		 upper <- upper[subscripts]
		 lower <- lower[subscripts]
		 panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
		 col = fill, border = FALSE,
		 ...)
		 }
	
	yrs[,upper:=coef+1.96*se]
	yrs[,lower:=coef-1.96*se]
	yrs[, grp:=1]
if(0){
	xyplot(coef~year|brand, data = yrs, groups = grp,
		 upper = yrs$upper, lower = yrs$lower,
		 panel = function(x, y, ...){
		 panel.superpose(x, y, panel.groups = my.panel.bands, type='l', col='gray',...)
		 panel.xyplot(x, y, type='b', cex=0.6, lty=1,...)
		 }, main = c('Estimated SBBE with 1.96 x SE confidence bounds'))
	}	 
	
	
	# Calculate marketing elasticities
	
########################
# EXTRACT ELASTICITIES #
########################

# calculate mean variables
means <- dt[, c('brand_name', xvars_heterog),with=F][, lapply(.SD, mean), by=c('brand_name'), .SDcols=c(xvars_heterog)]
msaverage <- dt[, list(mean_ms = mean(ms)), by=c('brand_name')]
meansmelt <- melt(means, id.vars=c('brand_name'))
setnames(meansmelt, 'value','mean_var')

elasticities <- merge(meansmelt, m$coefficients, by=c('variable', 'brand_name'), all.x=T, all.y=F)
elasticities <- merge(elasticities, msaverage, by=c('brand_name'))

elasticities[, elast := coef * (1-mean_ms) * mean_var]
elasticities[, elast_se := se * (1-mean_ms) * mean_var]

# summary
selast <- elasticities[!is.na(coef), list(median_elast = median(elast), 
										  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
										  N_brands= .N, 
										  perc_positive = length(which(z>=(1.96)))/.N, 
										  perc_null = length(which(abs(z)<1.96))/.N, 
										  perc_negative = length(which(z<=(-1.96)))/.N), by=c('variable')]
# summary table:
tmpx<-elasticities
setnames(tmpx, 'variable', 'varname')
tmp=melt(tmpx, id.vars=c('brand_name', 'varname', 'orig_var'))

print(dcast(tmp[varname%in%c('brand_name', xvars_heterog) & variable=='elast'], brand_name ~ varname))
print(selast)
return(list(coef=m$coefficients, elast=selast))
}


res <- run()

# save results
	#save(results_brands, results_category, markets, models, file='..\\output\\results.RData')
