#################################################
# EXTRACT DATA SET WITH EQUITY AND ELASTICITIES #
#################################################

extract_elast_equity <- function(tmp_results) {

	equity=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$equity)))
	setcolorder(equity, c('cat_name','brand_name','year','sbbe', 'sbbe_se', 'sbbems', 'sbbems_se', 'bav_relevance', 'bav_esteem','bav_knowledge', 'bav_energizeddiff', 'bav_asset'))
	
	# extract extra variables
	othervars=rbindlist(lapply(datasets, function(x) x[year>=2002, list(unitsales=sum(sales_bt,na.rm=T), revenue=sum(rev_bt,na.rm=T), price = mean(act_pr_bt,na.rm=T)),by=c('cat_name','brand_name', 'year')]))
	othervars[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]
	
	equity <- merge(equity, othervars,by=c('cat_name','brand_name', 'year'),all.x=T,all.y=F)
	
	equity[, rev_total := sum(revenue),by=c('cat_name', 'brand_name')]
	equity[, max_cat := rev_total==max(rev_total),by=c('brand_name')]
	equity[, ':=' (max_cat=NULL, rev_total=NULL)]

	meanequity = equity[, lapply(.SD, mean, na.rm=T), by=c('cat_name', 'brand_name'), .SDcols=grep('bav[_]', colnames(equity),value=T)]
	setkey(meanequity, cat_name, brand_name)
	
	# retrieve parameter estimates
	elast=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$elasticities)))
	elast[,var_name := gsub('adstock[0-9]*', 'adstock', var_name)]
	elast = elast[!grepl('cop[_]', var_name)]
	
	setkey(elast, cat_name, brand_name)
	elast=meanequity[elast]

	elast=elast[, !colnames(elast)%in%c('z', 'orig_var'),with=F] #'mean_var','coef','se',  #'mean_ms'
	return(list(elast=elast, equity=equity, meanequity=meanequity))
	
	}

tmp = extract_elast_equity(all_results[sel_models])

equity = tmp$equity

# yearly market shares
annual_ms = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2002]
	cat_name=unique(dt$cat_name)
	res=dt[, list(annual_avgms=mean(ms_bt,na.rm=T)),by=c('brand_name', 'year')]
	res[, cat_name:=cat_name]
	return(res)
	}))

# brand names
brand_names = rbindlist(lapply(datasets, function(x) {
	x[, c('cat_name', 'brand_name', 'brand_name_orig'), with=F]
	}))
	
setkey(annual_ms, cat_name, brand_name, year)
setkey(brand_names, cat_name, brand_name)
setkey(equity, cat_name, brand_name)

equity = merge(equity, annual_ms, all.x=T, all.y=F, by=c('cat_name', 'brand_name', 'year'))
equity[brand_names, brand_name_orig := i.brand_name_orig]
equity[, bav_brand := ifelse(is.na(bav_asset), 0, 1)]

setcolorder(equity, c('cat_name', 'brand_name', 'brand_name_orig', setdiff(colnames(equity),c('cat_name', 'brand_name', 'brand_name_orig'))))

elast = tmp$elast
elast[, bav_brand := ifelse(is.na(bav_asset), 0, 1)]
setkey(elast, cat_name, brand_name)
elast[brand_names, brand_name_orig := i.brand_name_orig]
setcolorder(elast, c('cat_name', 'brand_name', 'brand_name_orig', setdiff(colnames(elast),c('cat_name', 'brand_name', 'brand_name_orig'))))

###################
# Factor analysis #
###################

library(psych)
bav_dims =  c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff')
		
	# Equity brand-value metrics (i.e., by year and brand)
	for (ds in c('equity', 'elast')) {
		for (nfactors in 2:3) {
			
			cat('\n\n===============================================================================\n')
			cat(paste0('Factor analysis on the ', toupper(ds), ' data with ', nfactors, ' factor scores to be extracted\n'))
			cat('===============================================================================\n\n\n')
			
			if (ds=='equity') keys = c('brand_name', 'year')
			if (ds=='elast') keys = c('brand_name')
			
			eval(parse(text=paste0('setkey(', ds, ', ', paste(keys, collapse=','),')')))
			mydata=eval(parse(text=paste0('unique(', ds, ')')))[!is.na(bav_asset)]
			
			fit <- principal(mydata[, bav_dims,with=F], nfactors=nfactors, rotate="varimax")
			
			summary(fit)
			print(fit)
			
			cat('\n\nInitial Eigenvalues:\n')
			eig <- data.table(eigen(cor(mydata[, bav_dims, with=F]))$values)
			setnames(eig, 'eigenvalue')
			print(eig)
			
			fit_scores <- cbind(mydata[, keys,with=F], fit$scores)
			setkeyv(fit_scores, keys)
			
			for (nf in 1:nfactors) eval(parse(text=paste0(ds, '[fit_scores, F', nfactors,'_PC', nf, ':=i.PC', nf,']')))
				
			if (ds == 'equity') equity[, var_name:='none']
			
			}
		}

# Standardize variables
	stdvar <- function(x) (x-mean(x,na.rm=T))/sd(x, na.rm=T)
	std_without_mean <- function(x) x/sd(x, na.rm=T)
	
	# Equity / elasticitiy
	tmp=lapply(list(equity, elast), function(df) {
		
		for (.var in colnames(df)) {
			if (.var %in% c('year', 'cat_name', 'brand_name', 'var_name')) next #grep('F[_]', colnames(df),value=TRUE) grep('bav[_]', colnames(df),value=TRUE)
			# check whether column is dummy: do not meancenter
			if (all(unique(unlist(df[, .var, with=F]))%in%c(1,0))) next
			# varies by category
			if (all(unlist(df[, list(N=length(unique(get(.var)))), by=c('cat_name', 'var_name')]$N)==1)) next
			
			# only standardize for bav brands (!)
			df[!is.na(bav_asset), paste0(.var, '_STD') := stdvar(get(.var)), by=c('cat_name', 'var_name'),with=F]
			
			if (grepl('sbbe_se|sbbems_se|elast_se', .var)) {
				df[!is.na(bav_asset), paste0(.var, '_SDcat') := sd(get(.var),na.rm=T), by=c('cat_name', 'var_name'),with=F]
				df[!is.na(bav_asset), paste0(.var, '_STD') := std_without_mean(get(.var)), by=c('cat_name', 'var_name'),with=F]
				}
			}
		return(df)
		})

equity=tmp[[1]]
elast=tmp[[2]]

# Load category measures
cat_measures <- fread('../../derived/output/survey.txt')
equity <- merge(equity, cat_measures, by=c('cat_name'), all.x=T)
elast <- merge(elast, cat_measures, by=c('cat_name'), all.x=T)

