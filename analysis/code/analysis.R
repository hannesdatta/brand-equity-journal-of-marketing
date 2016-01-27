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

# define variables
	xvars_heterog <- c('pi_bt', 'promo_bt', 'distrwidth_bt', 'LineLength_bt') # can be potentially endogenous; copula correction terms pooled or homogenous? (--> Harald)
	yvars <- c('sales_bt') # to be used to calculate market shares
	xvars_homog <- c('attr_packet_bt') #grep('attr_', colnames(dt), value=T) # variable names which are homogenous
	it <- c(brand='brand_name', time='week') # indexes brands ([1]), and time ([2])

	
	
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
	for (br in brands) {
		dt[, paste0('dummy',std_sep, br) := as.numeric(brand_name %in% br)]
		}

# create dummies for all brands (for T-1 years)
#if(0){
	for (br in brands) {
		yrs = unique(dt[brand_name==br]$year)
		
		for (yr in yrs[-(1)]) {
			dt[, paste0('dummy', std_sep, br, '_yr_', yr) := as.numeric(brand_name %in% br & year==yr)]
			}
		}
#}

# create year dummies	
	#yrs = unique(dt$year)
#		for (yr in yrs[-(1)]) {
	#		dt[, paste0('dummy_yr',yr,'_',std_sep) := as.numeric( year==yr)]
	#		}

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
	setnames(X, gsub('transf[_]', '',colnames(X)))
	dates_brands <- longdt[, c('brand_name', 'week'),with=F]
	setnames(dates_brands, c('brand', 'date'))
	Y <- longdt[, 'dv_ms',with=F]

	m <- sur(as.matrix(X),as.matrix(Y),dates_brands)

	m$coefficients[, z := coef/se] # -> retrieved coefficients


############################
# VISUALIZE SBBE OVER TIME #
############################
	
	# retrieve years
	yrs <- m$coefficients[grepl('_yr_', variable)]
	yrs[, brand:=sapply(variable, function(x) strsplit(strsplit(x, std_sep)[[1]][2], '_')[[1]][1])]
	yrs[, year:=as.numeric(sapply(variable, function(x) rev(strsplit(x, '_')[[1]])[1]))]
	
	require(lattice)
	require(latticeExtra)
	 glayer 
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
	
	xyplot(coef~year|brand, data = yrs, groups = grp,
		 upper = yrs$upper, lower = yrs$lower,
		 panel = function(x, y, ...){
		 panel.superpose(x, y, panel.groups = my.panel.bands, type='l', col='gray',...)
		 panel.xyplot(x, y, type='b', cex=0.6, lty=1,...)
		 }, main = c('Estimated SBBE with 1.96 x SE confidence bounds'))
		 
	
	

	
# save results
	#save(results_brands, results_category, markets, models, file='..\\output\\results.RData')
