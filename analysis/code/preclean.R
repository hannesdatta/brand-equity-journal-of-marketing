#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

									  

### LOAD DATA SETS
require(data.table)
require(sas7bdat)


# Gathers all data sets, and performs a preclean
catdata <- NULL
fns = c(('..\\..\\..\\sugarsub\\revision\\sugarsub_nat_reg_wdummy_varneed.sas7bdat'))

for (fn in seq(along=fns)) {
	catdata[[fn]] <- data.table(read.sas7bdat(fns[fn]))
	
	}

	
# Already implemented in upstream-data preparation:  only brands are selected that account for a cumulative market share of 90%.
# To be implemented: take longest-observable stretch of non-missing observations (for the dependent, and for the independent variables).

# Apply category names --> store in list!
	#for (i in 1:length(skus_by_date_list)) {
	#	skus_by_date_list[[i]][, category:=names(skus_by_date_list)[i]]
	#	}
	
##############################
# PROTOTYPE FOR ONE CATEGORY #
##############################

dat <- catdata[[1]]

yvars <- c('sales_bt')
xvars <- c('pi_bt', 'promo_bt', 'distrwidth_bt', 'LineLength_bt')
attrvars_orig <- colnames(dat)[(which(colnames(dat)=='promo_bt')+1):(which(colnames(dat)=='pct_store_skus')-1)]

for (attrname in attrvars_orig) {
	setnames(dat, attrname, paste0('attr_', attrname))
	
	}

attrvars <- grep('attr_', colnames(dat),value=T)

if (!length(attrvars)==length(attrvars_orig)) stop('Attribute columns not uniquely identified')

# SELECT LONGEST CONSECUTIVE STRETCH OF AVAILABLE DATA per brand
require(zoo)

# set zero sales to NA
dat[get(yvars)==0, yvars := NA, with=F]

tmp <- split(dat[, colnames(dat)%in%c(xvars,yvars, attrvars, 'cat_name', 'week', 'year', 'brand_name'),with=F], paste0(dat$cat_name, '_', dat$brand_name))

dat <- rbindlist(lapply(tmp, function(x) {
	.zoo=zoo(x)
	.out = na.contiguous(.zoo)
	suppressWarnings(res <- x[, selected:=!1:.N %in% as.numeric(attr(.out, 'na.action'))])
	res
	# Add condition of at least 4 years of data (!)
	}))

# --> number of obs.
dat[selected==T, list(nobs = .N), by = c('cat_name', 'brand_name')]

# maybe do this by COMPLETE years?!?

datasets <- dat[selected==T]

save(datasets, file=c('..//temp//datasets.RData'))


