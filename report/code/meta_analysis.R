# Set path for focal model
	path = '../../analysis/output/MNL_copula_5mmix_nomc'
	require(data.table)

# Load data 
	load('..//..//derived//output//datasets.RData')

# Load results
	equity <- fread(paste0(path, '//equity.csv'))
	elast <- fread(paste0(path, '//elasticities.csv'))

	
###############################
# Table 3: Sample description #
###############################

equity[, years_per_brand := .N, by=c('cat_name', 'brand_name')]
tmp=equity[, list(no_brands = length(unique(brand_name)),
			  no_bav_brands = length(unique(brand_name[!is.na(bav_asset)])),
			  no_years = mean(years_per_brand[match(unique(brand_name), brand_name)], na.rm=T),
			  dollar_sales = mean(revenue)/1E6, 
			  dollar_sales_sd = sd(revenue/1E6),
			  bav_asset = mean(bav_asset[!is.na(bav_asset)]), 
			  bav_asset_sd = sd(bav_asset[!is.na(bav_asset)])
			  ), by=c('cat_name')]

# Total number of brands
with(equity,length(unique(paste(cat_name, brand_name, sep= '_'))))
# Total number of BAV brands
with(equity[!is.na(bav_asset)],length(unique(paste(cat_name, brand_name, sep= '_'))))
# Total number of brands
length(unique(paste(equity$cat_name, equity$brand_name, sep= '_')))
# Total number of brand-year observations
nrow(equity[, list(.N), by=c('cat_name', 'brand_name', 'year')])

# Export for XLS table
write.table(tmp, '../output/sample_description.csv', row.names=F)
	

# Mean of C4

setkey(elast,cat_name)
tmp=unique(elast)
tmp[, c('cat_name', 'c4'),with=F]

	
###########################################
# Table 4: Sales Response Model Estimates #
###########################################


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
	  
	  if (abs(zscore)>qnorm(1-(0.1/2))) ret <- c("  *")
	  if (abs(zscore)>qnorm(1-(0.05/2))) ret <- c(" **")
	  if (abs(zscore)>qnorm(1-(0.01/2))) ret <- c("***")
	  return(ret)
	  }
	
	tmp = elast[!grepl('cop[_]', var_name), list(welast = sum(elast/elast_se,na.rm=T)/sum(1/elast_se,na.rm=T),
				   elast_sd = sd(elast, na.rm=T),
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
				   elast_sd = sd(elast, na.rm=T),
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


summarize_elast(elast)

##############################
# CORRELATION TABLE FUNCTION #
##############################

# adapted from: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package

library(Hmisc)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
  # the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex"), ndec = 2){

    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-rcorr(x, type=method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "   ")))
    
    ## trunctuate the correlation matrix to three decimals
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), ndec))[,-1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    Rnew <- as.matrix(Rnew)
    Rnew <- as.data.frame(Rnew)
    
    ## remove last column and return the correlation matrix
    #Rnew <- cbind(Rnew[1:length(Rnew)-1])
	
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex") 
    }

} 

#################


###########
# TABLE 7 #
###########

# Correlations for equity (standardized within category)

rowvars = c('sbbe_STD', 'unitsales_STD', 'revenue_STD', 'price_STD')
colvars = grep('bav[_].*[_]STD', colnames(equity),value=T)

tmp = equity[, c('cat_name', 'brand_name',rowvars, colvars),with=F]
tmp = tmp[complete.cases(tmp)]
tmpcor=cor(as.matrix(tmp[,-c(1:2),with=F]))

options(width=600)
sink('../output/correlations_sbbe.txt')
print(corstars(as.matrix(tmp[,-c(1:2),with=F], method="pearson", removeTriangle='none', ndec=2)))
sink()


write.table(tmpcor, '../output/correlations_sbbe.csv', row.names=T)

###########
# TABLE 9 #
###########

# Correlations for brand equity and marketing mix response (standardized within category)

# extract mmix response
	require(reshape2)
	tmp=dcast(elast, cat_name + brand_name ~ var_name, value.var = 'elast_STD')

# get bav factors
	setkey(elast, cat_name, brand_name)
	tmp2 = unique(elast)

# merge both
	tmp <- data.table(merge(tmp, tmp2, by=c('cat_name', 'brand_name'), all.x=T))


rowvars = c('rreg_pr_bt', 'pi_bt', 'fd_bt', 'pct_store_skus_bt', 'adstock_bt')
colvars = grep('bav[_].*[_]STD', colnames(tmp),value=T)

tmpextract = tmp[, c('cat_name', 'brand_name',rowvars, colvars),with=F]
tmpcor=cor(as.matrix(tmpextract[,-c(1:2),with=F]), use = 'pairwise.complete')


write.table(tmpcor, '../output/correlations_mmix.csv', row.names=T)


options(width=600)
sink('../output/correlations_mmix.txt')
print(corstars(as.matrix(tmpextract[,-c(1:2),with=F]), method="pearson", removeTriangle='none', ndec=2))
sink()




summarize_equity <- function(equity) {
	
	#################################################
	# Cross-section time-series pooled correlations #
	#################################################
	vars=c('sbbe_STD', 'sbbems', 'sbbems_STD', 'annual_avgms_STD', 'unitsales_STD','revenue_STD','price_STD',grep('bav[_].*[_]STD', colnames(equity),value=T))
	tmp = equity[, c('cat_name', 'brand_name', vars),with=F]
	tmp=tmp[complete.cases(tmp)]
	
	tmpcor=cor(as.matrix(tmp[,-c(1:2),with=F]))
	
	
	cat('\n\nPooled Cross Section - Time Series Correlations Across Brands and Time\n\n')
	print(tmpcor)
	cat(paste0('\nNote: Measured for ', length(unique(paste0(tmp$brand_name,tmp$cat_name))), ' unique BAV brands\n'))
	
	}
	
	
	
	
summ <- function(tmp_results)	{
	
	cat('\n\nSELECTED DECAY PARAMETERS FOR ADVERTISING\n')
	decay=rbindlist(lapply(tmp_results, function(x) data.frame(cat_name=x$cat_name, adv_decay=.01*as.numeric(x$adv_decay))))
	print(summary(decay$adv_decay))
	cat('\nDecay parameters for all categories:\n')
	print(decay)
	
	}	
	

for (r in models) {
	
	source('proc_report.R')
	options(width=1000)
	sink(paste0('..//output//', r, '//summary_', r, '.txt'))
	summarize_elast(elast)
	summarize_equity(equity)
	
	#####################
	# Regressions: SBBE #
	#####################
	
	cat('\n\n===========================================================\nMode 1: SBBE (intercepts) regressed on BAV Factors\n===========================================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD, data = equity, subset = !is.na(F_RelEstKnow), weights=1/sbbe_se)

	print(summary(m))
	
	equity[, ':=' (seccat_mc = seccat - mean(ifelse(is.na(F_RelEstKnow_STD),NA, seccat),na.rm=T),
				   newbrnd_mc = newbrnd - mean(ifelse(is.na(F_RelEstKnow_STD),NA, newbrnd),na.rm=T))]
	
	
	cat('\n\n===========================================================\nModel 2: SBBE (intercepts) regressed on BAV Factors and brand characteristics\n===========================================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD + seccat + newbrnd, data = equity, subset = !is.na(F_RelEstKnow), weights=1/sbbe_se)

	print(summary(m))

	cat('\n\n===========================================================\nModel 3: SBBE (intercepts) regressed on BAV Factors and brand characteristics with interactions\n===========================================================\n')
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD + seccat_mc + newbrnd_mc +
			   F_RelEstKnow_STD * seccat_mc + F_EnergDiff_STD * seccat_mc +
			   F_RelEstKnow_STD * newbrnd_mc + F_EnergDiff_STD * newbrnd_mc, data = equity, subset = !is.na(F_RelEstKnow), weights=1/sbbe_se)
 
	print(summary(m))

	cat('\n\n===========================================================\nModel 4: Averaged SBBE intercepts, regressed on average BAV Factors and brand characteristics with interactions\n===========================================================\n')
	
	equity_avg = equity[!is.na(F_RelEstKnow_STD), lapply(.SD, mean), by=c('cat_name', 'brand_name'), .SDcols=c('sbbe_STD', 'F_RelEstKnow_STD', 'F_EnergDiff_STD', 'sbbe_se', 'seccat', 'newbrnd')]
	
	equity_avg[, ':=' (seccat_mc = seccat - mean(seccat),
				   newbrnd_mc = newbrnd - mean(F_RelEstKnow_STD))]
	
	m<-lm(sbbe_STD~1+
			   F_RelEstKnow_STD+F_EnergDiff_STD + seccat_mc + newbrnd_mc +
			   F_RelEstKnow_STD * seccat_mc + F_EnergDiff_STD * seccat_mc +
			   F_RelEstKnow_STD * newbrnd_mc + F_EnergDiff_STD * newbrnd_mc, data = equity_avg)#, weights=1/sbbe_se)
 
	print(summary(m))

	#############################
	# Regressions: Elasticities #
	#############################
	
	# select BAV brands only
	# compute mean-centered interaction effects
	
	signific = c(0.001, 0.01, .050, .1)
	names(signific) <- c('****', '***', '**', '*')
	options(signif.symbols=signific)

	elastreg<-function(form, weights, standardize=FALSE, dt, msg=NULL) {
		# do by variable
		if (!is.null(weights)) eval(parse(text=paste0('dt[, w:=', weights,']'))) else dt[, w:=1]
		vars = unique(dt$var_name)
		meta<-NULL
		for (j in seq(along=vars)) { #as.factor(cat_name) +
			meta[[j]] <- lm(as.formula(form), data= dt, subset= !is.na(dt)&var_name==vars[j], weights=w)#(1/elast_se))
			}
		#!elast_outlier & 
		require(car)
		require(memisc)
		
		m_all = paste0(paste0('\"',vars,'\"'),'=meta[[',seq(along=vars),']]')
		
		cat('\n\n\nMeta analysis')
		#print(eval(parse(text=paste0('mtable(', paste0(m_all, collapse=','), ',coef.style=\"all\")'))))
		print(eval(parse(text=paste0('mtable(', paste0(m_all, collapse=','), ')'))))
		
		#,signif.symbols=signific)
		# 
		cat(paste0('\n', msg, '\n'))
	
		cat('\nVIF values\n')
		for (j in seq(along=meta)) {
			cat('\n', vars[j], '\n')
			print(vif(meta[[j]]))
			}
	}

	cat('\n\n======================================\n')
	cat('======================================\nElasticities regressed on BAV Factors\n======================================\n')
	cat('======================================\n\n\n')
	
	cat('\n\nElasticities regressed on BAV Factors for all BAV brands\n=========================================================\n')
	uniq_br=elast[!is.na(bav_asset)][, list(.N),by=c('cat_name', 'brand_name')]
	elastreg('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se', dt=elast[!is.na(bav_asset)], msg=paste0('Models estimated with all ', nrow(uniq_br), ' BAV brands.'))
	
	#cat('\n\nElasticities regressed on BAV Factors for main-category BAV brands only\n=========================================================\n')
	#uniq_br=elast[seccat==0 & !is.na(bav_asset)][, list(.N),by=c('cat_name', 'brand_name')]
	#elastreg('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se', dt=elast[seccat==0& !is.na(bav_asset)], msg=paste0('Models estimated with ', nrow(uniq_br), ' brands in their main categories #(seccat==0).'))
	
	sink()
	
	############
	# PLOTTING #
	############
	
	source('summary_plots.R')

	
	}
