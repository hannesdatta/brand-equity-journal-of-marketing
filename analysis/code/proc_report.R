	
summarize_equity <- function(equity) {
	
	#################################################
	# Cross-section time-series pooled correlations #
	#################################################
	vars=c('sbbe_STD', 'annual_avgms_STD', 'unitsales_STD','revenue_STD','price_STD',grep('bav[_].*[_]STD', colnames(equity),value=T))
	tmp = equity[, c('cat_name', 'brand_name', vars),with=F]
	tmp=tmp[complete.cases(tmp)]
	
	tmpcor=cor(as.matrix(tmp[,-c(1:2),with=F]))
	
	
	cat('\n\nPooled Cross Section - Time Series Correlations Across Brands and Time\n\n')
	print(tmpcor)
	cat(paste0('\nNote: Measured for ', length(unique(tmp$brand_name)), ' unique BAV brands\n'))
	
	#################################################
	# Cross-section time-series pooled correlations #
	#################################################
	
	std_equity = equity[, lapply(.SD, function(x) (x-mean(x, na.rm=T))/sd(x, na.rm=T)),by=c('cat_name', 'brand_name'), .SDcols=vars]
	std_equity=std_equity[complete.cases(std_equity)]
	
	tmp=cor(as.matrix(std_equity[,-(1:2),with=F]))
		
	cat('\n\nTime Series Correlations Within Brands Over Time\n')
	print(tmp)
	cat(paste0('\nNote: Measured for ', length(unique(std_equity$brand_name)), ' unique BAV brands\n'))
	
	}
	
summ <- function(tmp_results)	{
	
	cat('\n\nSELECTED DECAY PARAMETERS FOR ADVERTISING\n')
	decay=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, adv_decay=.01*as.numeric(x$adv_decay))))
	print(summary(decay$adv_decay))
	cat('\nDecay parameters for all categories:\n')
	print(decay)
	
	}	

summarize_elast <- function(elast) {

	###############################
	# SUMMARY OF ALL ELASTICITIES #
	###############################
	
	elast[, id := .GRP, by=c('cat_name', 'brand_name')]
	elast[, bav_available := !is.na(bav_asset)]
	
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

	}