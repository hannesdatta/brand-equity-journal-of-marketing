
	#summ(all_results[sel$index])
		
###########################
# SUMMARIZING THE RESULTS #
###########################

	#othervars=rbindlist(lapply(datasets, function(x) x[year>=2002, list(marketshare = mean(ms_bt, na.rm=T), unitsales=sum(sales_bt,na.rm=T), revenue=sum(rev_bt,na.rm=T), price = mean(act_pr_bt,na.rm=T)),by=c('cat_name','brand_name', 'year')]))
	#	othervars[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]

	
# Make summary plots
#	source('summary_plots.R')

	
	
#############################
# Regressions: Elasticities #
#############################

signific = c(0.001, 0.01, .050, .1)
names(signific) <- c('****', '***', '**', '*')
#saveopt = getOption("signif.symbols")
options(signif.symbols=signific)

elast[, newbrand := ifelse(is.na(new_brand),0,new_brand)]


form = 'elast ~ 1 + F_RelEstKnow + F_EnergDiff'

form = 'elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD'

#elast=elast[secondary_cat==0]

go('coef_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/se')


go('elast_STD ~ 1 + F_RelEstKnow_STD + F_EnergDiff_STD', weights='1/elast_se')
go('elast ~ 1 + F_RelEstKnow + F_EnergDiff', weights='1/elast_se')

go('elast_STD ~ 1 + F_RelEstKnow_STD * secondary_cat + F_EnergDiff_STD * secondary_cat + F_RelEstKnow_STD * fooddrinks + F_EnergDiff_STD * fooddrinks', weights='1/elast_se')
go('elast ~ 1 + F_RelEstKnow * secondary_cat + F_EnergDiff * secondary_cat + F_RelEstKnow * fooddrinks + F_EnergDiff * fooddrinks', weights='1/elast_se')

#tmp=elast[var_name==vars[1]]
#with(tmp, cor(data.frame(ms, coef, elast, F_RelEstKnow_STD, F_EnergDiff_STD),use='complete.obs'))

go<-function(form, weights, standardize=FALSE){
	# do by variable
	if (!is.null(weights)) eval(parse(text=paste0('elast[, w:=', weights,']'))) else elast[, w:=1]
	vars = unique(elast$var_name)
	meta<-NULL
	for (j in seq(along=vars)) { #as.factor(cat_name) +
		meta[[j]] <- lm(as.formula(form), data= elast, subset= !is.na(elast)&var_name==vars[j], weights=w)#(1/elast_se))
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
	cat('\nVIF values\n')
	for (j in seq(along=meta)) {
		cat('\n', vars[j], '\n')
		print(vif(meta[[j]]))
		}
}

#####################
# Regressions: SBBE #
#####################


# interactions
interact = c('ms', 'pricepos', 'dealdepth', 'relad', 'secondary_cat', 'c4', 'H', 'sales_growth')
bav_dims = c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff')

bav_dims = c('F_RelEstKnow', 'F_EnergDiff')

	   
m<-lm(sbbe_STD~1+
		   F_RelEstKnow_STD+F_EnergDiff_STD, data = equity, weights=1/sbbe_se)

summary(m)

if (0){

form = paste0('sbbe ~ 1 +', paste(paste0(rep(bav_dims, each=length(interact)), '*', rep(interact,length(bav_dims))), collapse='+'))

summary(m<-lm(as.formula(form), data= equity, weights=1/sbbe_se))

coefs <- data.table(varname = names(m$coefficients),data.frame(summary(m)$coefficients))
setnames(coefs, c('varname', 'beta', 'se', 't', 'pval'))
coefs[, bav := as.character(sapply(varname, function(x) {res=strsplit(x, ':')[[1]]; grep('bav', res, value=T)} ))]
coefs[, interact := as.character(sapply(varname, function(x) {res=strsplit(x, ':')[[1]]; grep('bav', res, value=T, invert=TRUE)} ))]
coefs[, varname:=NULL]
require(reshape2)
dcast(data.frame(coefs), bav~interact, value.var='t')

}

