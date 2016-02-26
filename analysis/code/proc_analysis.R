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
require(reshape2)
require(marketingtools)
require(car) # for delta method

prepare_data <- function(i) {
	print(i)
	print(names(datasets)[i])
	
	dt <- datasets[[i]]
	
	void<-dt[, list(obs = .N), by=c('brand_name', 'year')]

	dt[, advertising_bt := advertising_bt/1000]
	
	# compute adstock
	adstock <- function(x, lambda) {
		res=double(length(x))
		res[1]=x[1]
		for (k in seq(from=2, to=length(x))) {
			res[k] = lambda * res[k-1] + (1-lambda) * x[k]
			}
		return(res)
		}
	
	decays = formatC(seq(from=0.00, to=1, by=.05)*100, width=2, flag=0)
	for (decay in decays) {
		dt[, paste0('adstock', decay, '_bt') := adstock(advertising_bt, lambda=as.numeric(decay)/100),by=c('brand_name'),with=F]
		}
	
	# kick out first 4 observations by brand
	dt[, min_week := min(week),by=c('brand_name')]
	dt <- dt[week>min_week+3]
	
	# retain only observations from year 2002 onwards (to match with BAV data)
	dt <- dt[year>=2002]
	
	dt
	}

analyze_marketshares <- function(dt, xvars_heterog = c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt', 'adstock_bt'), xvars_endog = NULL, attributes = TRUE, simpleDummies=TRUE, method = "FGLS-Praise-Winsten", benchmark = NULL, quarter=TRUE) {

	####################
	# DEFINE VARIABLES #
	####################


	# define heterogenous Xs (can be potentially endogenous; copula correction terms pooled or homogenous (--> HARALD)
		# define y variable, to be used to calculate market shares
		yvars <- c('sales_bt') 
		# define homogenous coefficients (e.g., brand attributes)
		if (attributes==TRUE) {
			xvars_homog <- c(grep('attr[_]', colnames(dt),value=T)) 
			
			# check whether any of these attributes is zero for all brands, or is equal throughout the observation periods; then remove this column
			attrsums = colSums(dt[, xvars_homog, with=F])
			
#			attrequal = dt[, lapply(.SD, function(x) length(unique(x))), by=c('week'), .SDcols=xvars_homog]

			if(any(attrsums==0)) {
				warning('Some of the product attributes are zero for all brands; removing variables ', paste0(names(attrsums)[which(attrsums==0)],collapse=' ,'))
				xvars_homog <- xvars_homog[-which(attrsums==0)]
				}

			} else {
			xvars_homog=c()
			}
		
		# define index for brands and time (# indexes brands ([1]), and time ([2]))
		it <- c(brand='brand_name', time='week') 

	##################
	# TRANSFORM DATA #
	##################

	# check for variation in all variables by brand; if not available, set all to NA
	for (.n in c(xvars_heterog, yvars)) {
		prec=6
		dt[, get_n := length(unique(round(get(.n),prec))),by=c('brand_name')]
		dt[get_n==1, .n := NA, with=F]
	
		dt[,':=' (get_n=NULL)]
		}
	cat('Unique values\n')
	dt[, lapply(.SD, function(x) length(unique(x[!is.na(x)]))), by=c('brand_name'), .SDcols=c(xvars_heterog, yvars)]
	
	################################
	# Add Copula Correction Terms  #
	################################

	for (.var in xvars_endog) {
		dt[, paste0('cop_', .var) := make_copula(get(.var)), by=c('brand_name'),with=F]
		xvars_heterog <- c(xvars_heterog, paste0('cop_',.var))
		}
	
	# Add copula series to the data and verify normality
	copula_normality=NULL
		if (length(xvars_endog>0)) {
			tmp=dt[, c('brand_name','week', xvars_endog),with=F][, lapply(.SD, function(x) {
				if (all(is.na(x))) return(as.numeric(NA))
				as.numeric(shapiro.test(x)$p)
				}
				), by=c('brand_name'), .SDcols=xvars_endog]

			copula_normality <- melt(tmp, id.vars=c('brand_name'))
			setnames(copula_normality, 'value', 'pval')
		}

	###########################
	### BASE BRAND APPROACH ###
	###########################
		
	cat('Create data set structure\n')

	# calculate market share of yvar
	dt[, year:=as.numeric(year)]
	dt[, nbrands := length(unique(get(it[1]))), by = c(it[2])]
	dt[, ms := sales_bt/sum(sales_bt), by=c(it[2])]

	# Transform data to base-brand representation
	dtbb <- attraction_data(as.formula(paste0('ms ~ ', paste0(c(xvars_heterog, xvars_homog),collapse=' + '))), 
					data = dt, heterogenous = as.formula(paste0(' ~ ', paste0(c(xvars_heterog),collapse=' + '))),
					index = ~ brand_name + week, model = "MNL", benchmark = benchmark)
	validObject(dtbb)
	#show(dtbb)
	
	# create dummies for all brands in all years they are available
	brands <- unique(dt$brand_name)
	benchbrand = dtbb@benchmark
	
	# create dummy dataset
	dummatrix = data.table(individ=dtbb@individ, period=dtbb@period)
	# match years
	setkey(dummatrix,individ,period)
	setkey(dt, brand_name, week)
	dummatrix[dt, ':=' (year=i.year, quarter=i.quarter)]
	
	for (br in brands[!brands%in%benchbrand]) {
		# do not create dummy variable for benchmark brand
		if (br==benchbrand) next
		
		# create quarterly dummies
		if (quarter==T) {
			dummatrix[, paste0('quarter2', '_', br) := as.numeric(individ %in% br & quarter==2)]
			dummatrix[, paste0('quarter3', '_', br) := as.numeric(individ %in% br & quarter==3)]
			dummatrix[, paste0('quarter4', '_', br) := as.numeric(individ %in% br & quarter==4)]
			}
		# create year dummies
		yrs = unique(dummatrix[individ==br]$year)
				
		for (yr in yrs) {
			dummatrix[, paste0('dummy', '_', br, '_yr_', yr) := as.numeric(individ %in% br & year==yr)]
			}
		}
	
	dummatrix[, ':=' (quarter=NULL, period=NULL, individ=NULL, year=NULL)]


	X=as.matrix(data.frame(dtbb@X[, -c(grep('[_]dum', colnames(dtbb@X)))],dummatrix))
	if (simpleDummies==TRUE) X=as.matrix(dtbb@X)
	Y=dtbb@y
	index=data.table(week=dtbb@period, brand_name=dtbb@individ)
	
	# drop one indicator for benchmark brand
	# choice of base brand: put last.
	if (any(colSums(X)==0)) stop(paste0('Problems with no variation in variables: ', paste(colnames(X)[which(colSums(X)==0)], collapse = ', ')))
	a=Sys.time()
	mest <- itersur(X=X,Y=as.matrix(dtbb@y), index=data.frame(date=dtbb@period,brand=dtbb@individ),method=method)
	b=Sys.time()
	
	m<-NULL
	m$coefficients <- coef(mest)
	m$varcovar = mest@varcovar
	m$rho = mest@rho
	m$rho_hat = mest@rho_hat
	m$bic = mest@bic
	m$aic = mest@aic
	m$sigma=mest@sigma
	m$elapse = as.numeric(b-a)
	m$coefficients$orig_var=m$coefficients$variable
	m$coefficients$variable <- NULL
	m$coefficients$z <- m$coefficients$coef/m$coefficients$se
	m$coefficients$brand_name<-unlist(lapply(strsplit(as.character(m$coefficients$orig_var), '_'), function(x) x[[1]][1]))
	m$coefficients$var_name<-unlist(lapply(strsplit(as.character(m$coefficients$orig_var), '_'), function(x) paste0(x[-1], collapse='_')))
	
	#print(m$sigma)
	#m$coefficients
	
	########################
	# EXTRACT ELASTICITIES #
	########################

		# calculate means of explanatory variables
		means <- dt[, c('brand_name', xvars_heterog),with=F][, lapply(.SD, mean), by=c('brand_name'), .SDcols=c(xvars_heterog)]
		meansmelt <- melt(means, id.vars=c('brand_name'))
		setnames(meansmelt, 'value','mean_var')
		setnames(meansmelt, 'variable','var_name')
		
		msaverage <- dt[, list(mean_ms = mean(ms)), by=c('brand_name')]
		
		elasticities <- merge(meansmelt, m$coefficients, by=c('var_name', 'brand_name'), all.x=T, all.y=F)
		elasticities <- merge(elasticities, msaverage, by=c('brand_name'))

		elasticities[, elast := coef * (1-mean_ms) * mean_var]
		elasticities[, elast_se := se * (1-mean_ms) * mean_var]
		
		# summary
		selast <- elasticities[!is.na(coef), list(median_elast = median(elast), 
												  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
												  N_brands= .N, 
												  perc_positive = length(which(z>=(1.96)))/.N, 
												  perc_null = length(which(abs(z)<1.96))/.N, 
												  perc_negative = length(which(z<=(-1.96)))/.N), by=c('var_name')]
		
	################
	# EXTRACT SBBE #
	################
		
		# Extract coefficients
		ind <- which(grepl('[_]dum|[_]yr[_]', mest@coefficients$variable))
		sbbe_raw <- data.table(mest@coefficients[ind,])
		sbbe_raw[, brand_name := sapply(variable, function(x) strsplit(gsub('dummy[_]','',x),'_')[[1]][1])]
		sbbe_raw[, year := sapply(as.character(variable), function(x) substr(x, nchar(x)-3,nchar(x)))]
		sbbe_raw[year=='_dum', year:=as.character('0')]
		sbbe_raw[,year:=as.numeric(year)]
		sbbe_raw[, index:= 1:.N]
		
		sbbe_sigma = mest@varcovar[ind,ind]
		coefs=sbbe_raw$coef
		names(coefs)<-paste0('x', sbbe_raw$index)
		
		# Delta Method to compute SE for SBBE
		deltaCall <- function(index) {
			forms <- sapply(index, function(x) {
				paste0('(1/',length(index)+1, ') * (', length(index), ' * x', x, '-', paste0('x', setdiff(index,x), collapse='-'), ')')
				})
			eq1 = sapply(forms, function(f) deltaMethod(coefs, f, sbbe_sigma))
			eqbbf = paste0('(1/',length(index)+1, ') * (-', paste0('x', index, collapse='-'), ')')
			
			eqbb = deltaMethod(coefs, eqbbf,sbbe_sigma)
			
			ret = t(matrix(c(as.numeric(eq1),as.numeric(eqbb)),nrow=2))
			rownames(ret) = c(names(index), "benchmark")
			colnames(ret) = c('sbbe', 'sbbe_se')
			ret
			}

		sbbe = rbindlist(lapply(unique(sbbe_raw$year), function(yr) {
			tmp=sbbe_raw[year==yr]
			index=tmp$index
			names(index) <- tmp$brand_name
			res=data.frame(deltaCall(index))
			res$brand_name=rownames(res)
			res$year=yr
			data.table(res)
			}))
		sbbe[brand_name=="benchmark", brand_name:=dtbb@benchmark]

	
	cbbe = dt[, lapply(.SD, unique), by=c('brand_name', 'year'), .SDcols=grep('bav[_]',colnames(dt),value=T)]
	brand_equity = merge(sbbe, cbbe, by=c('brand_name', 'year'), all.x=T, all.y=T)
	
	##################
	# RETURN RESULTS #
	##################		
		
	retobject <- list(cat_name = unique(as.character(dt$cat_name)), 
					  model=m, 
					  benchmark=dtbb@benchmark, 
					  elasticities = elasticities, 
					  summary_elasticities=selast,
					  copula_normality = copula_normality,
					  equity=data.frame(brand_equity))
	class(retobject) <- append(class(retobject), "bav_attraction")
	return(retobject)
	}

show <- function(x) {
	UseMethod("show", x)
	}

show.bav_attraction <- function(x) {
	cat('BAV PROJECT Model summary:\n')
	cat('=======================================\n\n')
	options(scipen=999)
			
	cat('Category                      :', x$cat_name,'\n')
	cat('\n\n')
	cat('Estimated auto correlation in residuals:\n')
	names(x$model$rho_hat)<-names(x$model$rho)
    print(x$model$rho_hat)
	cat('Auto-correlation in residuals remaining after Praise-Winsten correction:\n')
	print(x$model$rho)
	cat('\n\n')

	cat('Elasticities by brand:\n')
	print(dcast(x$elasticities[which(!grepl('cop[_]', x$elasticities$var_name)),], brand_name ~ var_name, value.var='elast'), digits=3)

	cat('\nSummary of estimated elasticities:\n')
	print(x$summary_elasticities[which(!grepl('cop[_]', x$summary_elasticities$var_name)),], digits=3)	
	
	cat('\nBrand equity correlations:\n')
	print(cor(x$equity[,c('sbbe', grep('bav[_]', colnames(x$equity),value=T))],use='complete.obs'))
	cat('\nSBBE and CBBE measures are available for ', length(unique(x$equity[which(!is.na(x$equity$bav_asset)),]$brand_name)), ' brands.\n')
	#print(dcast(melt(x$equity,id.vars=c('brand_name', 'year')), brand_name + year ~ variable),digits=3)
	
	# run this part only if Copulas are part of the model
	if(!is.null(x$copula_normality)) {
		cat('\nAssessment of normality for Gaussian Copulas using Shapiro-Wilk tests\n')
		tmp=x$copula_normality[, list(non_normal = length(which(pval<.05)), normal = length(which(pval>=.05))), by=c('variable')]
		print(tmp)
		cat('\nNote: p-value of .05 used.\n\n')
		if (sum(tmp$normal)>0) {
			cat('\nOverview of normally-distributed variables:\n')
			print(data.frame(x$copula_normality[pval>=.05]))
			}
			
		cat('\nAssessment of significance of Gaussian Copulas terms\n')
		tmp=data.table(x$model$coefficients)[grepl('cop[_]', orig_var)]
		cat('   Number of significant copula terms:  ', nrow(tmp[abs(z)>=1.69]),'\n')
		cat('   Number of insignificant copula terms:  ', nrow(tmp[abs(z)<1.69]))
		cat('\n   Note: p-value of .1 used.\n\n')
		}
	
	}

plot.bav_attraction <- function(x) {
	require(lattice)
	xyplot(sbbe + bav_asset ~ year, groups=brand_name, data= x$equity, type ='l', auto.key=T, main='SBBE and CBBE',  scales=list(y=list(relation="free")))
	}

