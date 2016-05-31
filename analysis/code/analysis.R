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
	cl <- makePSOCKcluster(9)

####################################
### RUN MODEL FOR ALL CATEGORIES ###
####################################
	
	# Setup cluster
		clusterExport(cl, c('prepare_data', 'datasets', 'init', 'analyze_marketshares'))
		void<-clusterEvalQ(cl, init())
		
	# MODEL SPECIFICATION
		mestim = overview$index
		#mestim = 21:25
		endogeneity_spec <- c('copula') # c('non-copula', 'copula')
		
		model_type = c('MNL')
		adv_decay = formatC(seq(from=0, to=.9, by=c(.1))*100, width=2, flag=0)
		#adv_decay="90"
		
		varspec <- c('5mmix') # '4mmix',
	
		meancentering = c('mc', 'nomc')
		
	# EXECUTION
		models=data.table(expand.grid(index=mestim, endogeneity_spec=endogeneity_spec, decay=adv_decay, model_type = model_type, varspec=varspec, meancentering=meancentering))
		models[, sample_size := overview$nobs[match(index, overview$index)]]
		setorderv(models, c('endogeneity_spec','sample_size','decay'), order=-1L)
		
		models[, descr := paste(index,endogeneity_spec,decay,model_type,varspec,meancentering,sep='_')]
		
		model_descr <- models$descr
		
	# RUN MODELS
		all_results <-clusterApplyLB(cl, model_descr, function(m) {
			i=as.numeric(strsplit(m,'_')[[1]][1])
			endogeneity_spec=strsplit(m,'_')[[1]][2]
			adv_decay=strsplit(m,'_')[[1]][3]
			model_type=strsplit(m,'_')[[1]][4]
			varspec=strsplit(m,'_')[[1]][5]
			meancentering=strsplit(m,'_')[[1]][6]

			dt <- prepare_data(i, plus_1 = ifelse(model_type=="MNL", FALSE, TRUE), meancentering = ifelse(meancentering=="mc", TRUE, FALSE))
			
			xvars_heterog <- c('pi_bt', 'rreg_pr_bt', 'pct_store_skus_bt',paste0('adstock', adv_decay, '_bt'))
			
			if (varspec=='5mmix') xvars_heterog <- c(xvars_heterog, 'fd_bt')

			if (meancentering=='mc') xvars_heterog = paste0(xvars_heterog, '_mc')
						
			if(endogeneity_spec=='copula') xvars_endog = xvars_heterog else xvars_endog = NULL
			
			res=try({analyze_marketshares(dt, xvars_heterog=xvars_heterog,
											 xvars_endog=xvars_endog,
											 yearlyDummies=TRUE,
											 quarterlyDummies=TRUE,
											 attributes=TRUE,
											 sur_method="FGLS-Praise-Winsten",
											 attr_spec=model_type,
											 benchmark= NULL,
											 rescale=TRUE,
											 testing=FALSE)}, silent=T)
			if ('try-error'%in%class(res)) return('error')
			
			if (!'try-error'%in%class(res)) {
				res$category_id=i
				res$adv_decay = adv_decay
				res$endogeneity_spec = endogeneity_spec
				res$model_type = model_type
				res$varspec=varspec
				res$meancentering = ifelse(meancentering=='mc', TRUE, FALSE)
				}
			return(res)
		})

	names(all_results) <- model_descr

	if(0) { # estimation of a single category
	
		i=22
		init()
		
		# add one to variables which can take on zero values
		dt <- prepare_data(i, plus_1 = FALSE, meancenter = TRUE)
		
		xvars_heterog=c('pi_bt', 'rreg_pr_bt', 
						'pct_store_skus_bt', 'adstock50_bt', 'fd_bt')
		
		xvars_heterog=c('pi_bt_mc', 'rreg_pr_bt_mc', 
						'pct_store_skus_bt_mc', 'adstock50_bt_mc', 'fd_bt_mc')
		
		out=try(analyze_marketshares(dt, xvars_heterog=xvars_heterog, 
										 xvars_endog=xvars_heterog, 
										 yearlyDummies=TRUE,
										 quarterlyDummies=TRUE,
										 attributes=TRUE,
										 sur_method="FGLS-Praise-Winsten",
										 rescale=TRUE,
										 attr_spec="MNL",
										 benchmark=NULL, 
										 testing=FALSE
										 ),silent=T)
		show(out)
		out$equity <- compute_equity(out)
		show(out)

	dt[, list(.N),by=c('brand_name', 'fd_bt')]
	
	}

####################
### SAVE RESULTS ###
####################
	
	save(all_results, file = '../output/results.RData')
	