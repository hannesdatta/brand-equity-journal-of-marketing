###########################
# SUMMARIZING THE RESULTS #
###########################


# Get results and prepare all data sets
	source('get_results.R')
# Extract equity / prepare data sets to be used in meta analysis
	source('get_equity_elast.R')
# Make summary plots
	#source('summary_plots.R')

# Write data files to disk
if (0) {
	# Write data sets to CSV

	write.table(equity, '..//output//equity.csv', sep='\t', row.names=F)
	write.table(elast, '..//output//elasticities.csv', sep='\t', row.names=F)

	# export to SAS
	require(foreign)
	dir.create('..//output//sas')
	unlink('..//output//sas//*')
	write.foreign(equity, '..//output//sas//equity.txt', '..//output//sas//equity.sas', package="SAS")
	write.foreign(elast, '..//output//sas//elasticities.txt', '..//output//sas//elasticities.sas', package="SAS")
	}
	




#############################
# Regressions: Elasticities #
#############################

form = 'elast_STD ~ 1 + F_RelEst + F_Knowledge + F_EnergDiff'
form = 'elast_STD ~ 1 + F_RelEst + F_Knowledge + F_EnergDiff + ms'
form = 'elast_STD ~ 1 + F_RelEst_STD + F_Knowledge_STD + F_EnergDiff_STD'
form = 'elast ~ 1 + F_RelEst + F_Knowledge + F_EnergDiff'
form = 'coef ~ 1 + F_RelEst + F_Knowledge + F_EnergDiff'

#dcast(elast, )
#signific = c(0.001, 0.01, .050, .1)
#names(signific) <- c('****', '***', '**', '*')
#saveopt = getOption("signif.symbols")
#options(signif.symbols=signific)

go<-function(){
	# do by variable
	vars = unique(elast$var_name)
	meta<-NULL
	for (j in seq(along=vars)) { #as.factor(cat_name) +
		meta[[j]] <- lm(as.formula(form), data= elast, subset= !is.na(elast)&var_name==vars[j], weights=(1/se))
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

bav_dims = c('F_RelEst' 'F_Knowledge' 'F_EnergDiff')

	   
m<-lm(sbbe~as.factor(cat_name)+secondary_cat+
		   brand_light+F_RelEst*secondary_cat+F_Knowledge*secondary_cat+F_EnergDiff*secondary_cat, data = equity, weights=1/sbbe_se)

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

