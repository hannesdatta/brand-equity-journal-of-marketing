
run <- function(){

#######################################
# DATA PREPARATION FOR SUR ESTIMATION #
#######################################

cat('Create data set structure\n')

# calculate market share of yvar
	dt[, year:=as.numeric(year)]
	dt[, nbrands := length(unique(get(it[1]))), by = c(it[2])]
	dt[, ms := sales_bt/sum(sales_bt), by=c(it[2])]
	dt[, ':=' (log_ms = log(ms))]
	dt[, ':=' (geom_ms = sum(log(ms))/nbrands), by = c(it[2])]

	dt[, dv_ms := log_ms - geom_ms]
	# -> the DV is labeled 'dv_ms'
	
# create dummies for all brands in all years they are available
	brands <- unique(dt$brand_name)

	for (br in brands) {
		yrs = unique(dt[brand_name==br]$year)
		
		for (yr in yrs) {
			dt[, paste0('dummy', std_sep, br, '_yr_', yr) := as.numeric(brand_name %in% br & year==yr)]
			}
		}

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

# Delete one brand-year specific dummy
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

cat('Estimate model\n')

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

cat('Summarize results\n')
	
	# retrieve years
	yrs <- m$coefficients[grepl('_yr_', orig_var)]
	yrs[, brand:=sapply(orig_var, function(x) strsplit(strsplit(x, std_sep)[[1]][2], '_')[[1]][1])]
	yrs[, year:=as.numeric(sapply(orig_var, function(x) rev(strsplit(x, '_')[[1]])[1]))]
	
	yrs[,upper:=coef+1.96*se]
	yrs[,lower:=coef-1.96*se]
	yrs[, grp:=1]
	
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

return(list(coef=m$coefficients, elasticities = elasticities, summary_elast=selast, brand_year_dummies=yrs, X=X,dates_brands=dates_brands))
}





