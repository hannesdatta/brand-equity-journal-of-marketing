
### LOAD DATA SETS
require(data.table)
require(reshape2)
require(marketingtools)
require(car) # for delta method and VIFs
library(Hmisc)
options(width=600)

load('..//..//derived//output//datasets.RData')
load('..//..//analysis//output//results.RData')

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)
	
# Load analysis code
	source('proc_analysis.R')
	
	models = rbindlist(lapply(all_results, function(x) data.frame(error=!'bav_attraction' %in% class(x), aic=x$model$aic, bic=x$model$bic, est_minutes = x$model$elapse_minutes)))
	models[, ':=' (index=1:nrow(models), model_name = names(all_results))]
	
	models[, type := sapply(model_name, function(x) strsplit(x, split='_')[[1]][2])]
	models[, cat_index := sapply(model_name, function(x) strsplit(x, split='_')[[1]][1])]
	models[, decay := as.numeric(sapply(model_name, function(x) strsplit(x, split='_')[[1]][3]))]

	# drop decays == 100
	models <- models[decay<100]
	
	models[, best_aic := min(aic), by=c('cat_index', 'type')]
	models[, selected := best_aic == aic]
	
	path='..//audit//decay_selection//'
	dir.create(path)
	require(lattice)
	for (i in unique(models$cat_index)) {
		tmp=models[cat_index==i]
		cat_name=rownames(overview)[match(i, overview$index)]
		png(paste0(path, cat_name, '.png'), res=200, units='in', height=8, width=12)

		print(xyplot(aic+bic~decay, groups=type,dat=tmp, 
		main=paste0(cat_name, ' (id ', i, ')'),
		scales = list(y = list(relation = "free")), type='l', auto.key=TRUE))
		dev.off()	   
		
		}
		
	# select best fit by type
	selected_models = models[best_aic==aic]


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

################
# PRINT OUTPUT #
################

	unlink('..//output//*.txt')
	
	# Report individual model results
	for (r in unique(selected_models$type)) {
		sel=selected_models[type==r]
		sink(paste0('..//output//estimates_', r, '.txt'))
	
		for (i in sel$index) {
		res = all_results[[i]]
		if (!'try-error'%in%class(res)) {
			(show(res)) } else {
			print(i)
			print('error')
			}
		
		}
	
		sink()
		
		sink(paste0('..//output//summary_', r, '.txt'))
		summ(all_results[sel$index])
		
		
		sink()
	
	}
	
#################################
## PREPARE META CHARACTERISTICS #
#################################

selected_models$cat_index

# category characteristics
cat_char = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2001]
	cat_name=unique(dt$cat_name)
	# concentration
	tmp=dt[, list(sales=sum(sales_bt,na.rm=T)),by=c('brand_name')]
	tmp[, ms := sales/sum(sales)]
	setorderv(tmp, 'sales',order=-1L)
	
	c4= sum(tmp$sales[1:4])/sum(tmp$sales)
	H = sum(tmp$ms^2)
	
	# growth rate
	tmp=dt[, list(sales=sum(sales_bt,na.rm=T),weeks=length(unique(week))),by=c('year')]
	
	setorder(tmp, year)
	growth=(sum(rev(tmp$sales)[1:2])-sum((tmp$sales)[1:2]))/sum((tmp$sales)[1:2])
	
	fooddrinks = ifelse(cat_name %in% c('beer', 'carbbev', 'coffee', 'coldcer', 'pz_di', 'ketchup', 'margbutr', 'mayo', 'milk', 'mustard', 'spagsauc', 'peanbutr', 'saltsnck', 'soup', 'sugarsub', 'yogurt'),1,0)
	
	hygiene = ifelse(cat_name %in% c('deod', 'diapers', 'laundet', 'rz_bl', 'shamp', 'toitisu'), 1,0)
	hhclean = ifelse(cat_name %in% c('hhclean'), 1,0)
	#perishable = ifelse(cat_name %in% c('
	
	fooddrinks = ifelse(cat_name %in% c('beer', 'carbbev', 'coffee', 'coldcer', 'pz_di', 'ketchup', 'margbutr', 'mayo', 'milk', 'mustard', 'spagsauc', 'peanbutr', 'saltsnck', 'soup', 'sugarsub', 'yogurt'),1,0)
	

	data.frame(cat_name=cat_name, c4=c4, H=H, sales_growth = growth)
	}))


# brand characteristics
brand_char = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2001]
	# concentration
	tmp=dt[, list(sales=sum(sales_bt,na.rm=T), meanprice = mean(rreg_pr_bt,na.rm=T), sdpriceindex = sd(pi_bt,na.rm=T),
											meanad = mean(advertising_bt,na.rm=T)),by=c('cat_name', 'brand_name')]
	tmp[, ms := sales/sum(sales)]
	
	std_by1 = function(x) {
		-1+2*(x/max(x,na.rm=T))
		
		}
		
	tmp=tmp[, list(brand_name=brand_name, ms=ms, pricepos = std_by1(meanprice), dealdepth = sdpriceindex, relad = std_by1(meanad)), by=c('cat_name')]
	
	return(tmp)
	}))

setkey(cat_char, cat_name)
setkey(brand_char, cat_name)

meta_char = brand_char[cat_char]

#cat_equity = 