
out=run(21,xvars_heterog=c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt', 'adstock_bt'), attributes=TRUE, simpleDummies=FALSE)


out2=run(21,xvars_heterog=c('promo_bt', 'ract_pr_bt', 'pct_store_skus_bt','adstock_bt'), attributes=TRUE, simpleDummies=TRUE)
out3=run(21,xvars_heterog=c('promo_mc', 'ract_pr_mc', 'pct_store_skus_mc','adstock_mc'), attributes=TRUE, simpleDummies=TRUE)

source('auxilary.R')
require(data.table)
# Write a function that summarizes

summary.attraction <- function(out) {
	cat('Attraction model\n')
	cat('--------------------------------------\n\n')
	cat('FGLS Iterations: ', out$model$iterations,'\n')
	cat('BIC: ', out$model$bic, '\n')
	
	cat('\n\nElasticities:\n')
	options(scipen=999)
	print(data.frame(out$elasticities[, c('brand_name', 'var_name', 'elast', 'elast_se', 'z'),with=F]),digits=3)
	cat('\n\nParameter Estimates:\n')
	print(data.frame(out$model$coefficients[, c('orig_var', 'coef', 'se', 'z')]),width=3,digits=3)

	}

summary.vif <- function(out) {
	tmp=split(data.frame(as.matrix(out$model$X)), out$model$dates_brands[,2])
	res=lapply(tmp, vif_fkt)
	printres <- data.frame(vif=unlist(lapply(res, function(x) x$vifs)))
	printres$index = 1:nrow(printres)
	print(printres, digits=3)
	return(do.call('c', lapply(res, function(x) x$all)))
	}

summary.attraction(out)
summary.attraction(out2)

vif<-summary.vif(out2)

vif<-summary.vif(out)

