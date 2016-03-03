	
extract_equity <- function(tmp_results) {

	equity=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$equity)))
	setcolorder(equity, c('cat_name','brand_name','year','sbbe', 'sbbe_se', 'bav_relevance', 'bav_esteem','bav_knowledge', 'bav_energizeddiff', 'bav_asset'))
	
	# extract extra variables
	othervars=rbindlist(lapply(datasets, function(x) x[year>=2002, list(unitsales=sum(sales_bt,na.rm=T), revenue=sum(rev_bt,na.rm=T), price = mean(act_pr_bt,na.rm=T)),by=c('cat_name','brand_name', 'year')]))
	othervars[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]
	
	equity <- merge(equity, othervars,by=c('cat_name','brand_name', 'year'),all.x=T,all.y=F)
	
	equity[, rev_total := sum(revenue),by=c('cat_name', 'brand_name')]
	equity[, max_cat := rev_total==max(rev_total),by=c('brand_name')]
	equity[, ':=' (max_cat=NULL, rev_total=NULL)]

	meanequity = equity[, lapply(.SD, mean, na.rm=T), by=c('cat_name', 'brand_name'), .SDcols=grep('bav[_]', colnames(equity),value=T)]
	setkey(meanequity, cat_name, brand_name)
	
	# retrieve parameter estimtaes
	elast=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$elasticities)))
	elast[,var_name := gsub('adstock[0-9]*', 'adstock', var_name)]
	elast = elast[!grepl('cop[_]', var_name)]
	
	setkey(elast, cat_name, brand_name)
	elast=meanequity[elast]

	elast=elast[, !colnames(elast)%in%c('mean_var','coef','se', 'z', 'orig_var', 'mean_ms'),with=F]
	return(list(elast=elast, equity=equity, meanequity=meanequity))
	
	}

	
	
summ <- function(tmp_results)	{
	######################################
	# POOLED CORRELATIONS: CBBE vs. SBBE #
	######################################

	equity=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$equity)))
	setcolorder(equity, c('cat_name','brand_name','year','sbbe', 'sbbe_se', 'bav_relevance', 'bav_esteem','bav_knowledge', 'bav_energizeddiff', 'bav_asset'))
	# standarize sbbe by category
	
	# extract extra variables
		othervars=rbindlist(lapply(datasets, function(x) x[year>=2002, list(unitsales=sum(sales_bt,na.rm=T), revenue=sum(rev_bt,na.rm=T), price = mean(act_pr_bt,na.rm=T)),by=c('cat_name','brand_name', 'year')]))
		othervars[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]
	
	equity <- merge(equity, othervars,by=c('cat_name','brand_name', 'year'),all.x=T,all.y=F)
	
	equity[, rev_total := sum(revenue),by=c('cat_name', 'brand_name')]
	equity[, max_cat := rev_total==max(rev_total),by=c('brand_name')]
	#equity <- equity[!brand_name=="PRIVATELABEL"]
	#equity <- equity[max_cat==T]
	equity[, ':=' (max_cat=NULL, rev_total=NULL)]
	
	#################################################
	# Cross-section time-series pooled correlations #
	#################################################
	
	
	std_equity = cbind(brand_name=equity$brand_name, equity[, lapply(.SD, function(x) (x-mean(x, na.rm=T))/sd(x, na.rm=T)),by=c('cat_name'), .SDcols=c('sbbe', 'unitsales','revenue','price',grep('bav[_]', colnames(equity),value=T))])
	std_equity=std_equity[complete.cases(std_equity)]
	
	#tmp = corstars(as.matrix(std_equity[,-(1:2),with=F]), method=c("pearson"), removeTriangle=c("lower"),
     #                result=c("none"))
	tmp=cor(as.matrix(std_equity[,-(1:2),with=F]))
	#tmp[upper.tri(tmp,diag=T)]<-NA				 
	#,use='pairwise.complete')
	#tmp = tmp[1:4, -(1:4)]
	
	cat('\n\nPooled Cross Section - Time Series Correlations Across Brands and Time\n\n')
	print(tmp)
	cat(paste0('\nNote: Measured for ', length(unique(std_equity$brand_name)), ' unique BAV brands\n'))
	
	
	#################################################
	# Cross-section time-series pooled correlations #
	#################################################
	
	std_equity = equity[, lapply(.SD, function(x) (x-mean(x, na.rm=T))/sd(x, na.rm=T)),by=c('cat_name', 'brand_name'), .SDcols=c('sbbe', 'unitsales','revenue','price',grep('bav[_]', colnames(equity),value=T))]
	std_equity=std_equity[complete.cases(std_equity)]
	
	tmp=cor(as.matrix(std_equity[,-(1:2),with=F]))
	#tmp[upper.tri(tmp,diag=T)]<-NA				 
	#,use='pairwise.complete')
	#tmp = tmp[1:4, -(1:4)]
	
	#tmp = corstars(as.matrix(std_equity[,-(1:2),with=F]), method=c("pearson"), removeTriangle=c("lower"),
     #                result=c("none"))

					 
	#,use='pairwise.complete')
	
	cat('\n\nTime Series Correlations Within Brands Over Time\n')
	print(tmp)
	cat(paste0('\nNote: Measured for ', length(unique(std_equity$brand_name)), ' unique BAV brands\n'))
	
	
	#########################################################
	# POOLED CORRELATIONS: CBBE vs. Marketing Effectiveness #
	#########################################################
	#vars = unlist(lapply(tmp_results, function(x) x$elasticities$var_name))
	
	#vars=c('adstock50_bt', 'pct_store_skus_bt', 'pi_bt', 'rreg_pr_bt')
	# retrieve BAV scores with CBBE
	meanequity = equity[, lapply(.SD, mean, na.rm=T), by=c('cat_name', 'brand_name'), .SDcols=grep('bav[_]', colnames(equity),value=T)]
	setkey(meanequity, cat_name, brand_name)
	
	# retrieve parameter estimtaes
	elast=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$elasticities)))
	elast[,var_name := gsub('adstock[0-9]*', 'adstock', var_name)]
	elast = elast[!grepl('cop[_]', var_name)]
	
	setkey(elast, cat_name, brand_name)
	elast=meanequity[elast]
	
	# standarize elasticities by category
	#cat('\n\nPooled correlations: Marketing elasticities and SBBE\n')
	#tmp=cor(elast[, !colnames(elast)%in%c('brand_name', 'cat_name'),with=F], use='pairwise.complete')
	#print(tmp[grep('bav[_]', rownames(tmp)), -grep('bav[_]', rownames(tmp))])	
	#
	#
	# do by variable
	vars = unique(elast$var_name)
	meta<-NULL
	for (j in seq(along=vars)) { #as.factor(cat_name) +
		meta[[j]] <- lm(elast ~ 1 + bav_relevance + bav_energizeddiff +  bav_esteem + bav_knowledge, data= elast,weights=1/elast_se, subset=!is.na(elast)&var_name==vars[j])
		}
	
	require(car)
	require(memisc)
	
	m_all = paste0(paste0('\"',vars,'\"'),'=meta[[',seq(along=vars),']]')
	
	cat('\n\n\nMeta analysis')
	print(eval(parse(text=paste0('mtable(', paste0(m_all, collapse=','), ')'))))
	
	cat('\nVIF values\n')
	for (j in seq(along=meta)) {
		cat('\n', vars[j], '\n')
		print(vif(meta[[j]]))
		}

	###############################
	# SUMMARY OF ALL ELASTICITIES #
	###############################
	
	elast=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, x$elasticities)))
	elast[, id := .GRP, by=c('cat_name', 'brand_name')]
	elast[,var_name := gsub('adstock[0-9]*', 'adstock', var_name)]
	
	# merge BAV brand IDs
	setkey(elast, cat_name, brand_name)
	setkey(meanequity, cat_name, brand_name)
	elast[meanequity,bav_available := !is.na(i.bav_asset)]
	

	sigvalue = .1
	sigtest = qnorm(1-sigvalue/2)

	signstars <- function(zscore) { # converts a z-score into a signifance asteriks
	  if (length(zscore)==0) return("   ")
	  if (is.nan(zscore) | !is.numeric(zscore) | is.na(zscore)) return("   ")
	  ret <- "ns."
	  #if (abs(zscore)>qnorm(1-(0.1))) ret <- c(paste("  ", rawToChar(as.raw(134)), sep=''))
	  
	  if (abs(zscore)>qnorm(1-(0.1/2))) ret <- c("  .")
	  if (abs(zscore)>qnorm(1-(0.05/2))) ret <- c("  *")
	  if (abs(zscore)>qnorm(1-(0.01/2))) ret <- c(" **")
	  if (abs(zscore)>qnorm(1-(0.001/2))) ret <- c("***")
	  return(ret)
	  }
	
	
	
	tmp = elast[!grepl('cop[_]', var_name), list(welast = sum(elast/elast_se,na.rm=T)/sum(1/elast_se,na.rm=T),
				   rosenthalznw = sum(elast/elast_se,na.rm=T)/sqrt(length(unique(id[!is.na(elast)]))),
				   signstars = signstars(sum(elast/elast_se,na.rm=T)/sqrt(length(unique(id[!is.na(elast)])))),
				   low90 = quantile(elast, .1,na.rm=T), up90 = quantile(elast, .9, na.rm=T),
				   ncoef = length(unique(id[!is.na(elast)])),
				   possig = length(unique(id[elast/elast_se>=sigtest]))/length(unique(id[!is.na(elast)])),
				   nullsig = length(unique(id[abs(elast/elast_se)<sigtest]))/length(unique(id[!is.na(elast)])),
				   negsig = length(unique(id[elast/elast_se<=-sigtest]))/length(unique(id[!is.na(elast)]))
				   ), by=c('var_name')]

	cat('\n\nSUMMARY OF ELASTICITIES FOR ALL BRANDS\n')
	print(tmp)
	
	tmp = elast[!grepl('cop[_]', var_name) & bav_available==T, list(welast = sum(elast/elast_se,na.rm=T)/sum(1/elast_se,na.rm=T),
				   rosenthalznw = sum(elast/elast_se,na.rm=T)/sqrt(length(unique(id[!is.na(elast)]))),
				   signstars = signstars(sum(elast/elast_se,na.rm=T)/sqrt(length(unique(id[!is.na(elast)])))),
				   low90 = quantile(elast, .1,na.rm=T), up90 = quantile(elast, .9, na.rm=T),
				   ncoef = length(unique(id[!is.na(elast)])),
				   possig = length(unique(id[elast/elast_se>=sigtest]))/length(unique(id[!is.na(elast)])),
				   nullsig = length(unique(id[abs(elast/elast_se)<sigtest]))/length(unique(id[!is.na(elast)])),
				   negsig = length(unique(id[elast/elast_se<=-sigtest]))/length(unique(id[!is.na(elast)]))
				   ), by=c('var_name')]

	cat('\n\nSUMMARY OF ELASTICITIES FOR BAV BRANDS\n')
	print(tmp)
	

	cat('\n\nSELECTED DECAY PARAMETERS FOR ADVERTISING\n')
	decay=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, adv_decay=.01*as.numeric(x$adv_decay))))
	print(summary(decay$adv_decay))
	cat('\nDecay parameters for all categories:\n')
	print(decay)
	
	

	}	
