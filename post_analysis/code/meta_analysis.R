# Load data.table
require(data.table)
options(width=1000)
	
# Load data 
	load('..//..//derived//output//datasets.RData')

# Set path for focal models
	path = '../../analysis/output/MNL_copula_5mmix_nomc'
	path2 = '../../analysis/output/MNL_non-copula_5mmix_nomc'

# Load results
	equity <- fread(paste0(path, '//equity.csv'))
	elast <- fread(paste0(path, '//elasticities.csv'))

	equity_nocop <- fread(paste0(path2, '//equity.csv'))
	elast_nocop <- fread(paste0(path2, '//elasticities.csv'))

	
###############################
# Stats for the paper         #
###############################

# How many brand entry/exists are there in the data?
equity[, list(.N), by=c('cat_name', 'brand_name')][, list(.N)]

tmp = equity[, list(.N), by=c('cat_name', 'brand_name')]
length(which(tmp<10))
length(which(tmp<10))/nrow(tmp)

#enter
tmp = equity[, list(enter=length(which(!any(year%in%2002))), exit = length(which(!any(year%in%2011)))), by=c('cat_name', 'brand_name')]
sum(tmp$enter)
sum(tmp$exit)

tmp = equity[, list(.N), by=c('cat_name', 'brand_name')]

length(which(tmp<10))
length(which(tmp<10))/nrow(tmp)




###############################
# Table 3: Sample description #
###############################

	equity[, years_per_brand := .N, by=c('cat_name', 'brand_name')]
	tmp=equity[, list(no_brands = length(unique(brand_name)),
			  no_bav_brands = length(unique(brand_name[!is.na(bav_asset)])),
			  no_years = mean(years_per_brand[match(unique(brand_name), brand_name)], na.rm=T),
			  #dollar_sales = mean(revenue)/1E6, 
			  #dollar_sales_sd = sd(revenue/1E6),
			  #bav_asset = mean(bav_asset[!is.na(bav_asset)]), 
			  #bav_asset_sd = sd(bav_asset[!is.na(bav_asset)]),
			  soc_value = mean(cat_socdemon,na.rm=T),
			  hedonic = mean(cat_hedonic, na.rm=T),
			  perfrisk = mean(cat_perfrisk,na.rm=T),
			  c4=mean(c4,na.rm=T)
			  ), by=c('cat_name')]
	
	# Merge actual catgory names
	cat_names <- fread('../../raw/category_names/category_names.txt')
	
	tmp <- merge(tmp, cat_names, by=c('cat_name'),all.x=T)
	setcolorder(tmp, c('cat_name_full', setdiff(colnames(tmp),'cat_name_full')))
	
	{
	cat('\nTotal number of brand:\n')
	cat(with(equity,length(unique(paste(cat_name, brand_name, sep= '_')))),fill=T)
	cat('Total number of BAV brands:\n')
	cat(with(equity[!is.na(bav_asset)],length(unique(paste(cat_name, brand_name, sep= '_')))),fill=T)
	cat('Total number of brands:\n')
	cat(length(unique(paste(equity$cat_name, equity$brand_name, sep= '_'))),fill=T)
	cat('Total number of brand-year observations:\n')
	cat(nrow(equity[, list(.N), by=c('cat_name', 'brand_name', 'year')]),fill=T)
	}
	
	# Export for XLS table
	tmp[, cat_name := NULL]
	write.table(tmp, '../output/sample_description.csv', row.names=F)

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

sink('../output/elasticities.txt')
cat('=========================\nModel with copula\n=========================\n')
summarize_elast(elast)
cat('\n\n\n=========================\nModel without copulas\n=========================\n')
summarize_elast(elast_nocop)
sink()

# Elasticities by category
sigvalue = .1
sigtest = qnorm(1-sigvalue/2)

tmp = elast[!grepl('cop[_]', var_name) & !is.na(elast), list(ncoef = length(unique(id[!is.na(elast)])),
			   welast = sum(elast/elast_se,na.rm=T)/sum(1/elast_se,na.rm=T),
			   median = median(elast, na.rm=T),
			   elast_sd = sd(elast, na.rm=T),
			   possig = length(unique(id[elast/elast_se>=sigtest]))/length(unique(id)),
			   null = length(unique(id[abs(elast/elast_se)<sigtest]))/length(unique(id)),
			   negsig = length(unique(id[elast/elast_se<=-sigtest]))/length(unique(id))
			   ), by=c('var_name', 'cat_name')]

tmp2=melt(tmp, id.vars=c('var_name', 'cat_name'))
tmp2[var_name=='rreg_pr_bt', var_name2 := '01_rreg_pr_bt']
tmp2[var_name=='pi_bt', var_name2 := '02_pi_bt']
tmp2[var_name=='fd_bt', var_name2 := '03_fd_bt']  
tmp2[var_name=='pct_store_skus_bt', var_name2 := '04_pct_store_skus_bt']  
tmp2[var_name=='adstock_bt', var_name2 := '05_adstock_bt']  

tmp1 = dcast(tmp2, cat_name~var_name2+variable)
tmp2 = elast[, list(adv_decay = mean(adv_decay)/100), by = c('cat_name')]

# get decay parameter
write.table(merge(tmp1, tmp2, by=c('cat_name')), row.names=F, file = '../output/elast_summary.csv')



# Correlation between the elasticities
focal_vars <- c('cat_name', 'brand_name', 'var_name', 'elast')
elast[, type:='copula']
elast_nocop[, type:='nocopula']

elast_merge <- dcast(rbind(elast, elast_nocop), cat_name+brand_name+var_name~type, value.var=c('elast'))

sink('../output/elasticities.txt', append=T)
cat('\n\n\n=========================\nAssociation between elasticities in the copula and non-copula model\n=========================\n')
print(elast_merge[, list(cor = cor(copula, nocopula, use='pairwise.complete'),
                   mean_difference = mean(copula-nocopula, na.rm=T)),
				   #rmse = sqrt(mean((copula-nocopula)^2,na.rm=T))), 
				   by=c('var_name')])
sink()


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

rowvars = c('sbbe_STD', 'unitsales_STD', 'revenue_STD', 'price_STD', 'annual_avgms_STD')
colvars = grep('bav[_].*[_]STD', colnames(equity),value=T)

tmp = equity[, c('cat_name', 'brand_name',rowvars, colvars),with=F]
tmp = tmp[complete.cases(tmp)]
tmpcor=cor(as.matrix(tmp[,-c(1:2),with=F]))

options(width=600)
sink('../output/correlations_sbbe.txt')
print(corstars(as.matrix(tmp[,-c(1:2),with=F], method="pearson", removeTriangle='none', ndec=2)))
cat(paste0('\nNumber of observations: ', nrow(as.matrix(tmp[,-c(1:2),with=F])), '\n'))
sink()

write.table(tmpcor, '../output/correlations_sbbe.csv', row.names=T)

# correlations excluding new brands
tmp = equity[!upd_new==1&!upd_seccatandnew==1, c('cat_name', 'brand_name',rowvars, colvars),with=F]
tmp = tmp[complete.cases(tmp)]
tmpcor=cor(as.matrix(tmp[,-c(1:2),with=F]))

options(width=600)
sink('../output/correlations_sbbe_nonewbrands.txt')
print(corstars(as.matrix(tmp[,-c(1:2),with=F], method="pearson", removeTriangle='none', ndec=2)))
cat(paste0('\nNumber of observations: ', nrow(as.matrix(tmp[,-c(1:2),with=F])), '\n'))
sink()



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
cat(paste0('\nNumber of observations: ', nrow(as.matrix(tmp[,-c(1:2),with=F])), '\n'))
sink()

