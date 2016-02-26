
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
	sink('..//output//estimates_without_copulas.txt')
	
	for (i in 1:24) {
		res = all_results[[i]][[r]]
		if (!'try-error'%in%class(res)) {
			(show(res)) } else {
			print(i)
			print('error')
			}
		
		}
	
	sink()
	
	#	if(0){
	r=2
	sink('..//output//estimates_with_copulas.txt')
	
	for (i in 1:24) {
		res = all_results[[i]][[r]]
		if (!'try-error'%in%class(res)) {
			(show(res)) } else {
			print(i)
			print('error')
			}
		
		}
	
	sink()
	#}
	
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex")){

    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-rcorr(x, type=method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex") 
    }

} 
library(Hmisc)
	
	
	# select on TOP brands only
	
	
#	r=1
 summ <- function(r)	{
	######################################
	# POOLED CORRELATIONS: CBBE vs. SBBE #
	######################################

	equity=rbindlist(lapply(all_results, function(x) data.frame(cat_name=x[[r]]$cat_name, x[[r]]$equity)))
	setcolorder(equity, c('cat_name','brand_name','year','sbbe', 'sbbe_se', 'bav_relevance', 'bav_esteem','bav_knowledge', 'bav_energizeddiff', 'bav_asset'))
	# standarize sbbe by category
	
	# extract extra variables
		othervars=rbindlist(lapply(datasets, function(x) x[selected==T, list(unitsales=sum(sales_bt,na.rm=T), revenue=sum(rev_bt,na.rm=T), price = mean(act_pr_bt,na.rm=T)),by=c('cat_name','brand_name', 'year')]))
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
	vars=c('adstock50_bt', 'pct_store_skus_bt', 'pi_bt', 'rreg_pr_bt')
	# retrieve BAV scores with CBBE
	meanequity = equity[, lapply(.SD, mean, na.rm=T), by=c('cat_name', 'brand_name'), .SDcols=grep('bav[_]', colnames(equity),value=T)]
	setkey(meanequity, cat_name, brand_name)
	
	# retrieve parameter estimtaes
	elast=rbindlist(lapply(all_results, function(x) data.frame(cat_name=x[[r]]$cat_name, x[[r]]$elasticities)))[var_name%in%vars]
	
	#elast=data.table(dcast(tmp[], cat_name + brand_name ~ var_name, value.var=c('elast')))
	
	#
	#elast=cbind(brand_name=elast$brand_name, elast[, lapply(.SD, function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T)), .SDcols=vars, by=c('cat_name')])
	
	setkey(elast, cat_name, brand_name)
	elast=meanequity[elast]
	
	# standarize elasticities by category
	#cat('\n\nPooled correlations: Marketing elasticities and SBBE\n')
	#tmp=cor(elast[, !colnames(elast)%in%c('brand_name', 'cat_name'),with=F], use='pairwise.complete')
	#print(tmp[grep('bav[_]', rownames(tmp)), -grep('bav[_]', rownames(tmp))])	
	#
	#
	adstock <- lm(elast ~  as.factor(cat_name) + bav_relevance + bav_energizeddiff +  bav_esteem + bav_knowledge, data= elast,weights=1/elast_se, subset=!is.na(elast)&var_name=='adstock50_bt')
	pctskus <- lm(elast ~  as.factor(cat_name) + bav_relevance + bav_energizeddiff +  bav_esteem + bav_knowledge, data= elast,weights=1/elast_se, subset=!is.na(elast)&var_name=='pct_store_skus_bt')
	pibt <- lm(elast ~  as.factor(cat_name) + bav_relevance + bav_energizeddiff +  bav_esteem + bav_knowledge, data= elast,weights=1/elast_se, subset=!is.na(elast)&var_name=='pi_bt')
	rregpr <- lm(elast ~ as.factor(cat_name) + bav_relevance + bav_energizeddiff +  bav_esteem + bav_knowledge, data= elast,weights=1/elast_se, subset=!is.na(elast)&var_name=='rreg_pr_bt')
	
	require(car)
	
	require(memisc)
	
	#mall = paste0(paste0('\"',.vars,'\"'),'=meta[[',seq(along=.vars),']]')
	cat('\n\n\nMeta analysis')
	print(mtable(adstock,pctskus,pibt,rregpr, coef.style='horizontal'))
	
	#cat('\n adstock\n')
	#print(vif(adstock))
	#cat('\n pctskus\n')
	#print(vif(pctskus))
	#cat('\n pibt\n')
	#print(vif(pibt))
	#cat('\n rregpr\n')
	#print(vif(rregpr))
	
	###############################
	# SUMMARY OF ALL ELASTICITIES #
	###############################
	
	elast=rbindlist(lapply(all_results, function(x) data.frame(cat_name=x[[r]]$cat_name, x[[r]]$elasticities)))
	elast[, id := .GRP, by=c('cat_name', 'brand_name')]
	
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
		
	}
	
	sink('..//output//summary_with_copulas.txt')
	summ(2)
	sink()
	
	sink('..//output//summary_without_copulas.txt')
	summ(1)
	sink()
		

