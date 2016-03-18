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
	cl <- makePSOCKcluster(12)

	if(0){
	errors = unlist(lapply(all_results, function(x) !'bav_attraction' %in% class(x)))
	errors = data.table(index=1:length(errors), model_name = names(errors), err=errors)
	}
	

####################################
### RUN MODEL FOR ALL CATEGORIES ###
####################################
		
		clusterExport(cl, c('prepare_data', 'datasets', 'init', 'analyze_marketshares'))
		void<-clusterEvalQ(cl, init())
		
		mestim = overview$index
		#mestim = 22:23
		model_types <- c('non-copula', 'copula')
		decays = formatC(seq(from=0, to=.9, by=c(.1))*100, width=2, flag=0)
		models=data.table(expand.grid(index=mestim, type=model_types, decay=decays))
		models[, sample_size := overview$nobs[match(index, overview$index)]]
		setorderv(models, c('type','sample_size','decay'), order=-1L)
		
		models[, descr := paste(index,type,decay,sep='_')]
		
		models <- models$descr
		
		unlink('..//temp//*')

		all_results <-clusterApplyLB(cl, models, function(m) {
					i=as.numeric(strsplit(m,'_')[[1]][1])
					spec=strsplit(m,'_')[[1]][2]
					decay=strsplit(m,'_')[[1]][3]
					sink(paste0('..//temp//progress_', m,'.txt'))
					cat('running\n')
					print(Sys.time())
					sink()
					
					dt <- prepare_data(i)
					
					if(spec=='non-copula') res=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', decay, '_bt')), yearlyDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL,testing=FALSE),silent=T)
					if(spec=='copula') res=try(analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', decay, '_bt')),
																	xvars_endog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', decay, '_bt')),
																	yearlyDummies=TRUE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, testing=FALSE),silent=T)
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
	
	names(all_results) <- models
	
	if(0){
	set.seed(123)
	te=cbind(rnorm(1000), rnorm(1000), rnorm(1000))
	cov(te)
	
	te2=te
	
	te2[,1] <- te2[,1]/1000
	
	cov(te2)
	
	tex<-cov(te2)
	tex[1,]<-tex[1,]*1000
	tex[,1]<-tex[,1]*1000
	tex
	}
	
	if(0) { # estimation of a single category
	
		i=22
		
		init()
		dt <- prepare_data(i)
		
		xvars_heterog=c('pi_bt', 'rreg_pr_bt', 
						'pct_store_skus_bt', 'adstock70_bt') #,'adstock40_bt'
		
		# add one to variables which can take on zero values
		dt <- prepare_data(i)
		setorder(dt, brand_name, week)
		dtold <- prepare_data_old(i)
		setorder(dtold, brand_name, week)
		te=cbind(dtold$pi_bt, dt$pi_bt)
		cor(te)
		dtsave<-dt
		
		te=cbind(dtsave$pi_bt, dt$pi_bt, dtold$pi_bt)
		cor(te)
	
		out2=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=xvars_heterog, yearlyDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=TRUE,testing=FALSE,model="MNL",rescale=FALSE),silent=T)
	
		te=cbind(dtsave$pi_bt, dt$pi_bt, dtold$pi_bt)
		cor(te)
	
		dt <- prepare_data_old(i, standardize=FALSE)
		dtnew <- prepare_data(i)
		setorder(dtnew, brand_name, week)
		te=cbind(dt$pi_bt, dtnew$pi_bt)
		
		dt[, pi_bt := (pi_bt-min(pi_bt,na.rm=T))/(max(pi_bt,na.rm=T)-min(pi_bt,na.rm=T))]
	
		dt <- prepare_data_old(i, standardize=FALSE)
		setorder(dt, brand_name, week)
		
		for (.var in grep('attr[_]', colnames(dt),value=T)) {
			if (!length(unique(unlist(dt[, .var,with=F])))==1) dt[, .var := (get(.var)-min(get(.var),na.rm=T))/(max(get(.var),na.rm=T)-min(get(.var),na.rm=T)),with=F]
			}
	
	
		init()
		dt <- prepare_data(i)
		xvars_heterog=c('pi_bt', 'rreg_pr_bt', 
						'pct_store_skus_bt', 'adstock70_bt') #,'adstock40_bt'
		out=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=NULL, yearlyDummies=FALSE,attributes=FALSE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=FALSE,testing=FALSE,model="MNL",rescale=FALSE),silent=T)
	
		out2=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=NULL, yearlyDummies=FALSE,attributes=FALSE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=FALSE,testing=FALSE,model="MNL",rescale=TRUE),silent=T)
		out3=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=NULL, yearlyDummies=FALSE,attributes=FALSE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=FALSE,testing=FALSE,model="MNL",rescale=TRUE),silent=T)
		
		out=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=xvars_heterog, yearlyDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=TRUE,testing=FALSE,model="MNL",rescale=FALSE),silent=T)
		out2=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=xvars_heterog, yearlyDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=TRUE,testing=FALSE,model="MNL",rescale=TRUE),silent=T)
		show(out)
		show(out2)
		
		head(out$model$coefficients)
		head(out2$model$coefficients)

		
	
		unlist(lapply(datasets, function(x) max(x$advertising_bt)))
		
		out2=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=xvars_heterog, yearlyDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=TRUE,testing=FALSE),silent=T)
	
		dt <- prepare_data(i, plus_1=TRUE)
		out=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, xvars_endog=xvars_heterog, yearlyDummies=FALSE,attributes=TRUE,method="FGLS-Praise-Winsten", benchmark= NULL, quarter=TRUE,testing=FALSE,model="MNL",rescale=TRUE),silent=T)
		
		require(lattice)
		xyplot(adstock60_bt ~ week|brand_name, data=dt,auto.key=T,type='l')
		xyplot(adstock50_bt ~ week|brand_name, data=dt,auto.key=T,type='l')
		xyplot(advertising_bt ~ week|brand_name, data=dt,auto.key=T,type='l')
		
		out$model$elapse
		
		outendog=analyze_marketshares(dt, xvars_heterog=c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt'),
									 xvars_endog = c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt', 'adstock50_bt'), yearlyDummies=FALSE,method="FGLS-Praise-Winsten")
		
		show(out)
		show(outendog)
	}
	

####################
### SAVE RESULTS ###
####################
	
	save(all_results, file = '../output/results.RData')
	