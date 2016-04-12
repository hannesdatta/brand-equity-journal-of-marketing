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

prepare_data <- function(i, plus_1 = FALSE) {
	# if plus_1 == TRUE, then - if standarize == TRUE, variables will be scaled between 1 and 2. 
	
	print(i)
	print(names(datasets)[i])
	
	#dt <- datasets[[i]][selected == T]
	
	void<-dt[, list(obs = .N), by=c('brand_name', 'year')]	
	
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
		dt[, nonzero_ad := length(which(advertising_bt[year>=2002]>0)),by=c('brand_name')]
		dt[, paste0('adstock', decay, '_bt') := adstock(advertising_bt, lambda=as.numeric(decay)/100),by=c('brand_name'),with=F]
		dt[, paste0('adstock', decay, '_bt') := round(get(paste0('adstock', decay, '_bt')),0), with=F]
		dt[, paste0('adstock', decay, '_bt') := ifelse(nonzero_ad>=52, get(paste0('adstock', decay, '_bt')), NA), with=F]
		dt[, nonzero_ad := NULL]
		}

	# variation in fd_bt
		dt[, nonzero_fd := length(which(fd_bt[year>=2002]>0)),by=c('brand_name')]
		dt[, fd_bt := ifelse(nonzero_fd>=52, fd_bt, NA)]
		dt[, nonzero_fd := NULL]
	
	# retain only observations from year 2002 onwards (to match with BAV data)
	dt <- dt[year>=2002]
	
	# rescale attribute levels and fd_bt between 0 and 100 (i.e., multiply by 100)
	for (.var in c(grep('attr[_]|fd[_]bt', colnames(dt),value=T))) {
		dt[, .var := 100*(get(.var)),with=F]
		}
	
	if (plus_1==TRUE) {
		vars=c(grep('adstock', colnames(dt),value=T))
		
		for (.var in c(vars, grep('attr[_]|fd[_]bt', colnames(dt),value=T))) {
		    if (!length(unique(unlist(dt[, .var,with=F])))==1) dt[, .var := get(.var)+1,with=F]
			}
		}
	setorder(dt, brand_name, week)
		
	dt
	}
	
analyze_marketshares <- function(dtf, xvars_heterog = c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt', 'advertising_bt'), 
									  xvars_endog = NULL, 
									  attributes = TRUE, 
									  yearlyDummies=TRUE, 
									  sur_method = "FGLS-Praise-Winsten", 
									  benchmark = NULL, 
									  quarterlyDummies=TRUE, 
									  testing = FALSE, 
									  attr_spec = "MNL", 
									  rescale=FALSE) {

	####################
	# DEFINE VARIABLES #
	####################
	dtf = data.table(dtf)
	
	# define heterogenous Xs (can be potentially endogenous; copula correction terms pooled or homogenous (--> HARALD)
		# define y variable, to be used to calculate market shares
		yvars <- c('sales_bt') 
		# define homogenous coefficients (e.g., brand attributes)
		if (attributes==TRUE) {
			xvars_homog <- c(grep('attr[_]', colnames(dtf),value=T)) 
			
			# check whether any of these attributes is zero for all brands, or is equal throughout the observation periods; then remove this column
			attrsums = colSums(dtf[, xvars_homog, with=F])
			
#			attrequal = dtf[, lapply(.SD, function(x) length(unique(x))), by=c('week'), .SDcols=xvars_homog]

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
		dtf[, get_n := length(unique(get(.n))),by=c('brand_name')]
		dtf[get_n==1, .n := NA, with=F]
		
		if (grepl('advertising|adstock', .n)) {
			dtf[, obs := length(which(get(.n)>0)),by=c('brand_name')]
			dtf[obs<8, .n := NA, with=F]
			dtf[, obs:=NULL]
			}
		dtf[,':=' (get_n=NULL)]
		}

	cat('Unique values\n')
	dtf[, lapply(.SD, function(x) length(unique(x[!is.na(x)]))), by=c('brand_name'), .SDcols=c(xvars_heterog, yvars)]
	
	################################
	# Add Copula Correction Terms  #
	################################
	
	if (attr_spec=='MCI') {
		tkt <- function(x) log(x)
		btkt <- function(x) exp(x)
		}
		
	if (attr_spec=='MNL') {
		tkt <- function(x) x
		btkt <- function(x) x
		}
		
	for (.var in xvars_endog) {
		dtf[, paste0('cop_', .var) := btkt(make_copula(get(.var))), by=c('brand_name'),with=F]
		xvars_heterog <- c(xvars_heterog, paste0('cop_',.var))
		}
	
	# Add copula series to the data and verify normality
	copula_normality=NULL
		if (length(xvars_endog>0)) {
			tmp=dtf[, c('brand_name','week', xvars_endog),with=F][, lapply(.SD, function(x) {
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
	dtf[, year:=as.numeric(year)]
	dtf[, nbrands := length(unique(get(it[1]))), by = c(it[2])]
	dtf[, ms := sales_bt/sum(sales_bt), by=c(it[2])]

	# Transform data to base-brand representation
	dtbb <- attraction_data(as.formula(paste0('ms ~ ', paste0(c(xvars_heterog, xvars_homog),collapse=' + '))), 
					data = dtf, heterogenous = as.formula(paste0(' ~ ', paste0(c(xvars_heterog),collapse=' + '))),
					index = ~ brand_name + week, model = attr_spec, benchmark = benchmark)
	validObject(dtbb)
	
	# create dummies for all brands in all years they are available
	brands <- unique(dtf$brand_name)
	benchbrand = dtbb@benchmark

	# create dummy dataset
	dummatrix = data.table(individ=dtbb@individ, period=dtbb@period)
	# match years
	setkey(dummatrix,individ,period)
	setkey(dtf, brand_name, week)
	dummatrix[dtf, ':=' (year=i.year, quarter=i.quarter)]
	
	for (br in brands[!brands%in%benchbrand]) {
		# do not create dummy variable for benchmark brand
		if (br==benchbrand) next
		
		# create quarterly dummies
		if (quarterlyDummies==T) {
			for (qu in 2:4) {
				# mean-centering for effect coding
				dummatrix[individ==br, paste0('quarter', qu,'_', br) := ifelse(quarter==qu, 1, 0)-sum(quarter==qu)/.N]
				dummatrix[!individ==br, paste0('quarter', qu,'_', br) := 0]
				}
			}
			
		# create year dummies
		yrs = unique(dummatrix[individ==br]$year)
				
		for (yr in yrs) {
			dummatrix[, paste0('dummy', '_', br, '_yr_', yr) := as.numeric(individ %in% br & year==yr)]
			}
		}
	
	dummatrix[, ':=' (quarter=NULL, period=NULL, individ=NULL, year=NULL)]

	# rescaling
	X=as.matrix(data.frame(dtbb@X[, -c(grep('[_]dum', colnames(dtbb@X)))]))
	
	if(rescale==TRUE) { # divide variables by their absolute max
		cat('running rescaling\n')
		rescale_values = apply(X, 2, function(x) max(abs(x)))
		div_matrix <- matrix(rep(rescale_values, nrow(X)), byrow=TRUE, ncol=length(rescale_values))
		X=X/div_matrix
		}
	
	if (yearlyDummies==TRUE) {
		X=as.matrix(data.frame(X,dummatrix))
		} else {
		X=as.matrix(X)
		}

	Y=dtbb@y
	index=data.table(week=dtbb@period, brand_name=dtbb@individ)
	
	# drop one indicator for benchmark brand
	# choice of base brand: put last.
	X_without_quarter = X[,!grepl('quarter', colnames(X))]
	if (any(colSums(X_without_quarter)==0)) stop(paste0('Problems with no variation in variables: ', paste(colnames(X_without_quarter)[which(colSums(X_without_quarter)==0)], collapse = ', ')))
	
	a=Sys.time()
	cat('Starting model estimation...\n')
	mest <- try(itersur(X=X,Y=as.matrix(dtbb@y), index=data.frame(date=dtbb@period,brand=dtbb@individ),method=sur_method,maxiter=ifelse(testing==T, 1, 1000)),silent=TRUE)
	b=Sys.time()
	cat('Finished model estimation.\n')

	if (class(mest)=='try-error' | testing==TRUE) stop('Iterative SUR procedure does not run')
	
	retr_coefs <- coef(mest)$coef
	mvarcovar=mest@varcovar

	if (rescale==TRUE) {
		cat('transforming back coefficients\n')
		retr_coefs[seq(length.out=length(rescale_values))] = retr_coefs[seq(length.out=length(rescale_values))] / rescale_values
		
		for (ch in seq(length.out=length(rescale_values))) {
			mvarcovar[ch,] <- mvarcovar[ch,] / rescale_values[ch]
			mvarcovar[,ch] <- mvarcovar[,ch] / rescale_values[ch]
			}
	}
	
	coef_sum <- data.frame(variable=coef(mest)$variable, coef=retr_coefs, se = sqrt(diag(mvarcovar)))
	coef_sum$z = coef_sum$coef / coef_sum$se
	
	
	m<-NULL
	m$coefficients <- coef_sum
	m$varcovar = mvarcovar
	m$rho = mest@rho
	m$rho_hat = mest@rho_hat
	m$bic = mest@bic
	m$aic = mest@aic
	m$llik = mest@llik
	m$sigma=mest@sigma
	m$elapse_minutes = as.numeric(difftime(b,a,unit="mins"))
	m$coefficients$orig_var=m$coefficients$variable
	m$coefficients$variable <- NULL
	m$coefficients$brand_name<-unlist(lapply(strsplit(as.character(m$coefficients$orig_var), '_'), function(x) x[[1]][1]))
	m$coefficients$var_name<-unlist(lapply(strsplit(as.character(m$coefficients$orig_var), '_'), function(x) paste0(x[-1], collapse='_')))

	########################
	# EXTRACT ELASTICITIES #
	########################

		# calculate means of explanatory variables
		means <- dtf[, c('brand_name', xvars_heterog),with=F][, lapply(.SD, mean), by=c('brand_name'), .SDcols=c(xvars_heterog)]
		meansmelt <- melt(means, id.vars=c('brand_name'))
		setnames(meansmelt, 'value','mean_var')
		setnames(meansmelt, 'variable','var_name')
		
		msaverage <- dtf[, list(mean_ms = mean(ms)), by=c('brand_name')]
		
		elasticities <- merge(meansmelt, m$coefficients, by=c('var_name', 'brand_name'), all.x=T, all.y=F)
		elasticities <- merge(elasticities, msaverage, by=c('brand_name'))
		
		if (attr_spec=="MNL") {
			elasticities[, elast := coef * (1-mean_ms) * mean_var]
			elasticities[, elast_se := se * (1-mean_ms) * mean_var]
			}
			
		if (attr_spec=="MCI") {
			elasticities[, elast := coef * (1-mean_ms)]
			elasticities[, elast_se := se * (1-mean_ms)]
			}
			
		# summary
		selast <- elasticities[!is.na(coef), list(median_elast = median(elast), 
												  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
												  N_brands= .N, 
												  perc_positive = length(which(z>=(1.96)))/.N, 
												  perc_null = length(which(abs(z)<1.96))/.N, 
												  perc_negative = length(which(z<=(-1.96)))/.N), by=c('var_name')]
		
	brand_equity=NULL
	
	
	##################
	# RETURN RESULTS #
	##################		
		
	retobject <- list(cat_name = unique(as.character(dtf$cat_name)), 
					  model=m, 
					  attr_spec = attr_spec,
					  benchmark=dtbb@benchmark, 
					  elasticities = elasticities, 
					  summary_elasticities=selast,
					  copula_normality = copula_normality,
					  cbbe=dtf[, lapply(.SD, unique), by=c('brand_name', 'year'), .SDcols=grep('bav[_]',colnames(dtf),value=T)],
					  sbbe=NULL,
					  equity=data.frame(brand_equity))
	class(retobject) <- append(class(retobject), "bav_attraction")
	return(retobject)
	}

show <- function(x) {
	UseMethod("show", x)
	}

compute_equity <- function(x) {
	UseMethod("compute_equity", x)
	}
	
compute_equity.bav_attraction <- function(x) {
	################
	# EXTRACT SBBE #
	################
	coef_sum = x$model$coefficients
	ind <- which(grepl('[_]dum|[_]yr[_]', coef_sum$orig_var))
		
	if (length(ind)>0 & x$attr_spec == 'MNL') { # Note: SBBE can only be extracted for MNL models
		# Extract coefficients
		sbbe_raw <- data.table(coef_sum[ind,])
		sbbe_raw[, brand_name := sapply(orig_var, function(x) strsplit(gsub('dummy[_]','',x),'_')[[1]][1])]
		sbbe_raw[, year := sapply(as.character(orig_var), function(x) substr(x, nchar(x)-3,nchar(x)))]
		sbbe_raw[year=='_dum', year:=as.character('0')]
		sbbe_raw[,year:=as.numeric(year)]
		sbbe_raw[, index:= 1:.N]
		
		sbbe_sigma = x$model$varcovar[ind,ind]
		coefs=sbbe_raw$coef
		names(coefs)<-paste0('x', sbbe_raw$index)
		
		# SBBE Computation: NORMALIZED INTERCEPTS
		# SEs computed using the Delta Method (requiring package 'car' to be loaded)
		computeSBBE <- function(index) {
			# for non-benchmark brands
			forms <- sapply(index, function(x) { # build formulas
				paste0('(1/',length(index)+1, ') * (', length(index), ' * x', x, '-', paste0('x', setdiff(index,x), collapse='-'), ')')
				})
			eq1 = sapply(forms, function(f) deltaMethod(coefs, f, sbbe_sigma)) # compute
			
			# for benchmark brand
			eqbbf = paste0('(1/',length(index)+1, ') * (-', paste0('x', index, collapse='-'), ')') # formula
			eqbb = deltaMethod(coefs, eqbbf, sbbe_sigma) # compute
			
			# assign names
			ret = t(matrix(c(as.numeric(eq1),as.numeric(eqbb)),nrow=2))
			rownames(ret) = c(names(index), "benchmark")
			colnames(ret) = c('sbbe', 'sbbe_se')
			ret
			}

		# SBBE Computation: exp(alpha0i)/sum(exp(alpha0j)) for all brands j and focal brand i (which is its market share representation)
		computeSBBEms <- function(index) {
			# for non-benchmark brands
			forms <- sapply(index, function(x) { # build formulas
				paste0('exp(x', x, ')/(1+', paste0(paste0('exp(', paste0('x', index), ')'), collapse='+'),')')
				})
			eq1 = sapply(forms, function(f) deltaMethod(coefs, f, sbbe_sigma)) # compute
			
			# for benchmark brand
			eqbbf = paste0('exp(1)/(1+', paste0(paste0('exp(', paste0('x', index), ')'), collapse='+'),')') # formula
			eqbb = deltaMethod(coefs, eqbbf, sbbe_sigma) # compute
			
			# assign names
			ret = t(matrix(c(as.numeric(eq1),as.numeric(eqbb)),nrow=2))
			rownames(ret) = c(names(index), "benchmark")
			colnames(ret) = c('sbbems', 'sbbems_se')
			ret
			}

		sbbe = rbindlist(lapply(unique(sbbe_raw$year), function(yr) {
			tmp=sbbe_raw[year==yr]
			index=tmp$index
			names(index) <- tmp$brand_name
			res=data.frame(computeSBBE(index),computeSBBEms(index))
			res$brand_name=rownames(res)
			res$year=yr
			data.table(res)
			}))
		sbbe[brand_name=="benchmark", brand_name:=x$benchmark]
	
	brand_equity = merge(sbbe, x$cbbe, by=c('brand_name', 'year'), all.x=T, all.y=T)
	
	}
	return(data.frame(brand_equity))
	}
		

show.bav_attraction <- function(x) {
	cat('BAV PROJECT Model summary:\n')
	cat('=======================================\n\n')
	options(scipen=999)
			
	cat('Category                      :', x$cat_name,'\n')
	cat('\n\n')
	cat('Model type (MNL vs. MCI): ', x$attr_spec,'\n\n')
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
	
	if (nrow(x$equity)>0) {
	cat('\nBrand equity correlations:\n')
	print(cor(x$equity[,c('sbbe', grep('bav[_]', colnames(x$equity),value=T))],use='complete.obs'))
	cat('\nSBBE and CBBE measures are available for ', length(unique(x$equity[which(!is.na(x$equity$bav_asset)),]$brand_name)), ' brands.\n')
	#print(dcast(melt(x$equity,id.vars=c('brand_name', 'year')), brand_name + year ~ variable),digits=3)
	} else {
	cat('\nBrand equity:\n\nnot estimated.\n\n')
	}
	
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

