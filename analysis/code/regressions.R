# Scratch

# Get results and prepare all data sets
	source('get_results.R')
# Make summary plots
	source('summary_plots.R')

# Write data sets to CSV
write.table(equity, '..//output//equity.csv', sep='\t', row.names=F)
write.table(elast, '..//output//elasticities.csv', sep='\t', row.names=F)



#####################
# Regressions: SBBE #
#####################


#for (.var in colnames(

# interactions
interact = c('ms', 'pricepos', 'dealdepth', 'relad', 'secondary_cat', 'c4', 'H', 'sales_growth')
bav_dims = c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff')
bav_dims = c('RelEst', 'Knowledge','Energ')

 #cor(equity[, c('sbbe', 'ms', grep('bav[_]', colnames(equity),value=T)),with=F],use='pairwise.complete')
 
#m<-lm(sbbe~1+bav_relevance+bav_esteem+bav_knowledge+bav_energizeddiff, data = equity, weights=1/sbbe_se)
m<-lm(sbbe~1+RelEst+Knowledge+Energ, data = equity, weights=1/sbbe_se)

equity[, ':=' (lag_RelEst = c(NA, RelEst[-(.N)]),
			   lag_Knowledge = c(NA, Knowledge[-(.N)]),
			   lag_Energ = c(NA, Energ[-(.N)])), by=c('cat_name', 'brand_name')]
			   
m<-lm(sbbe~1+RelEst+Knowledge+Energ, data = equity, weights=1/sbbe_se)
summary(m)
#+lag_RelEst+lag_Knowledge+lag_Energ


#cor(equity[, c('sbbe', 'ms', grep('bav[_]', colnames(equity),value=T)),with=F],use='pairwise.complete')

#mydata= equity[!is.na(bav_asset), c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff'),with=F]

#fit <- princomp(mydata, cor=TRUE)
# summary(fit) # print variance accounted for 
 #loadings(fit) # pc loadings 
 
 ###
 

form = paste0('sbbe ~ 1 +', paste(paste0(rep(bav_dims, each=length(interact)), '*', rep(interact,length(bav_dims))), collapse='+'))

summary(m<-lm(as.formula(form), data= equity, weights=1/sbbe_se))

coefs <- data.table(varname = names(m$coefficients),data.frame(summary(m)$coefficients))
setnames(coefs, c('varname', 'beta', 'se', 't', 'pval'))
coefs[, bav := as.character(sapply(varname, function(x) {res=strsplit(x, ':')[[1]]; grep('bav', res, value=T)} ))]
coefs[, interact := as.character(sapply(varname, function(x) {res=strsplit(x, ':')[[1]]; grep('bav', res, value=T, invert=TRUE)} ))]
coefs[, varname:=NULL]
require(reshape2)
dcast(data.frame(coefs), bav~interact, value.var='t')

#############################
# Regressions: Elasticities #
#############################


# interactions
interact = c('ms', 'pricepos', 'dealdepth', 'relad', 'secondary_cat', 'c4', 'H', 'sales_growth')
bav_dims = c('bav_relevance', 'bav_esteem','bav_knowledge','bav_energizeddiff')

avg_factors = equity[, lapply(.SD, mean,na.rm=T), by=c('cat_name', 'brand_name'), .SDcols=c('RelEst','Knowledge', 'Energ')]

setkey(avg_factors, cat_name, brand_name)
setkey(elast, cat_name, brand_name)
elast[avg_factors, ':=' (RelEst=i.RelEst, Knowledge=i.Knowledge, Energ=i.Energ)]

#form = 'elast ~ 1 + RelEst + Knowledge + Energ + ms'
form = 'elast ~ 1 + bav_relevance + bav_knowledge + bav_energizeddiff + ms'

#elast[, std_elast = ]



#form = paste0('elast ~ 1 +', paste(paste0(rep(bav_dims, each=length(interact)), '*', rep(interact,length(bav_dims))), collapse='+'))
summary(lm(as.formula(form), data= elast, subset= var_name=='rreg_pr_bt', weights=1/elast_se))

#dcast(elast, )
	

	# do by variable
	vars = unique(elast$var_name)
	meta<-NULL
	for (j in seq(along=vars)) { #as.factor(cat_name) +
		meta[[j]] <- lm(as.formula(form), data= elast, subset= !is.na(elast)&var_name==vars[j], weights=1/elast_se)
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

		
		