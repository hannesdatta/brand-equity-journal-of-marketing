#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

									  

### LOAD DATA SETS
load('..//..//derived//output//datasets.RData')

# Data set overview
	overview <- data.frame(index = 1:length(datasets), nobs=unlist(lapply(datasets, nrow)))
	print(overview)
	
	
init <- function() {
		require(marketingtools)
		require(reshape2)
		require(data.table)
		require(car)
		source('proc_analysis.R')
		}
		
init()	

# Enable cluster estimation	
	require(parallel)
	cl <- makePSOCKcluster(16)



# Example for one category
	
#	if(0){ #cluster estimation for prototyping
	
		clusterExport(cl, c('prepare_data', 'datasets', 'init', 'analyze_marketshares'))
		void<-clusterEvalQ(cl, init())
		
		mestim = overview$index
		#mestim=22:23
		model_types <- c('non-copula', 'copula')
		decays = formatC(seq(from=0, to=1, by=c(.1))*100, width=2, flag=0)
		models=data.table(expand.grid(index=mestim, type=model_types, decay=decays))
		models[, sample_size := overview$nobs[match(index, overview$index)]]
		setorderv(models, c('type','sample_size','decay'), order=-1L)
		
		models[, descr := paste(index,type,decay,sep='_')]
		
		models <- models$descr
		unlink('..//temp//*')
		a=Sys.time()
		all_results <-clusterApplyLB(cl, models, function(m) {
					i=as.numeric(strsplit(m,'_')[[1]][1])
					spec=strsplit(m,'_')[[1]][2]
					decay=strsplit(m,'_')[[1]][3]
					sink(paste0('..//temp//progress_', m,'.txt'))
					cat('running\n')
					print(Sys.time())
					sink()
					dt <- prepare_data(i)
					
					if(spec=='non-copula') res=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', decay, '_bt')), simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL),silent=T)
					if(spec=='copula') res=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', decay, '_bt')),
																	xvars_endog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', decay, '_bt')),
																	simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL),silent=T)
					res$category_no=i
					res$spec = spec
					res$adv_decay = decay
					
					sink(paste0('../temp//progress_', m,'.txt'),append=T)
					cat(class(res))
					
					cat('ended\n')
					print(Sys.time())
					sink()
					
					return(res)
				})
		b=Sys.time()
	
	names(all_results) <- models
	
	if(0) { # estimation of a single category
	
		i=23
		
		init()
		dt <- prepare_data(i)
		xvars_heterog=c('pi_bt', 'rreg_pr_bt', 
						'pct_store_skus_bt','adstock50_bt')
		dt[brand_name=='MD', c('adstock50_bt', 'adstock60_bt'),with=F]
	
		out=try(analyze_marketshares(dt, xvars_heterog, simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=TRUE),silent=T)
		
		require(lattice)
		xyplot(adstock60_bt ~ week|brand_name, data=dt,auto.key=T,type='l')
		xyplot(adstock50_bt ~ week|brand_name, data=dt,auto.key=T,type='l')
		xyplot(advertising_bt ~ week|brand_name, data=dt,auto.key=T,type='l')
	#	
		out$model$elapse
		
		outendog=analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt'),
									 xvars_endog = c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt'), simpleDummies=FALSE,method="FGLS-Praise-Winsten")
		
		show(out)
		show(outendog)
	}
	
#	xvars_heterog=c('pi_bt', 'rreg_pr_bt')#,'adstock50_bt')
		
#	out=try(analyze_marketshares(dt, xvars_heterog, simpleDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=FALSE),silent=T)
	
#	
	if(0){
		i=7
		
		init()
		dt <- prepare_data(i)
	
	decays = formatC(seq(from=0, to=1, by=c(.1))*100, width=2, flag=0)
	out<-lapply(decays, function(decay) {
			analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', paste0('adstock', decay, '_bt')),
									 xvars_endog = c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', paste0('adstock', decay, '_bt')), simpleDummies=FALSE,method="FGLS-Praise-Winsten")
								 
			})

	par(mfrow=c(1,2))
	plot(x=decays,y=unlist(lapply(out, function(x) x$model$bic)),type='l', main='BIC')
	abline(v=decays[which(unlist(lapply(out, function(x) x$model$bic))==min(unlist(lapply(out, function(x) x$model$bic))))])
	decays[which(unlist(lapply(out, function(x) x$model$bic))==min(unlist(lapply(out, function(x) x$model$bic))))]
	which(unlist(lapply(out, function(x) x$model$bic))==min(unlist(lapply(out, function(x) x$model$bic))))

	plot(x=decays,y=unlist(lapply(out, function(x) x$model$aic)),type='l',main='AIC')
	abline(v=decays[which(unlist(lapply(out, function(x) x$model$aic))==min(unlist(lapply(out, function(x) x$model$aic))))])
	decays[which(unlist(lapply(out, function(x) x$model$aic))==min(unlist(lapply(out, function(x) x$model$aic))))]
	which(unlist(lapply(out, function(x) x$model$aic))==min(unlist(lapply(out, function(x) x$model$aic))))
	}

####################################
### RUN MODEL FOR ALL CATEGORIES ###
####################################
if(0){
	focal_cats <- seq(along=datasets)
	#focal_cats <- 21:22
	vars <- c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt')
	
	all_results <- lapply(focal_cats, function(x) {
		cat('Running category ', x, '...\n')
		dt<-prepare_data(x)
		benchmark=NULL
			
		v1=	try(analyze_marketshares(dt, 
									 xvars_heterog=c(vars),
									 simpleDummies=FALSE, 
									 attributes=TRUE, 
									 benchmark=benchmark,
									 method = "FGLS-Praise-Winsten"), silent=T)
		v2=	try(analyze_marketshares(dt, 
									 xvars_heterog=c(vars), 
									 xvars_endog = c(vars), 
									 simpleDummies=FALSE, 
									 attributes=TRUE, 
									 benchmark=benchmark,
									 method = "FGLS-Praise-Winsten"),silent=T)
		
		return(list(v1,v2))
		})

		
		
	names(all_results) <- names(datasets)[seq(along=all_results)]
}

####################
### SAVE RESULTS ###
####################
	
	save(all_results, file = '../output/results.RData')
	save(a,b, file = '../output/results_time.RData')
