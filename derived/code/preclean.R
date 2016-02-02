#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     


# load packages
require(data.table)
	
# load data
	load('..//temp//rawdata.RData')
	load('..//temp//attributes.RData')


# Brand selection has already been implemented in upstream-data preparation:
# brands are selected that account for a cumulative market share of 90%.

################################
# PREPARE ATTRIBUTE-LEVEL DATA #
################################

	# Combine attribute information with panel data
	relevant_attr = which(names(attr)%in%names(catdata))

	if (!length(relevant_attr)==length(catdata)) stop('Not all attribute levels available, or a problem in the panel data')

	attr <- attr[relevant_attr]

	# add category names to data sets
	attr <- data.frame(rbindlist(lapply(attr, function(x) x[,colnames(x)%in%c('attribute', 'attribute_variables', 'cat_name')])))

	# complete attribute columns
	for (i in 2:nrow(attr)) {
		if(is.na(attr$attribute[i])) attr$attribute[i]<-attr$attribute[i-1]
		}

	attr <- data.table(attr)
	attr <- attr[!is.na(attribute_variables)]
	# mark attribute levels that are to be deleted (other, or last one per attribute value)
	
	attr[, keep := !(1:.N==.N), by=c('cat_name', 'attribute')]
	
	
###########################
# BUILD DERIVED DATA SETS #
###########################

# Variables to be included: All variables, except 1-L attribute-level combinations
dat <- NULL

	for (i in seq(along=catdata)) {
	
		# verify whether all attribute levels are listed as variable names
		vars <- colnames(catdata[[i]])
		
		attrs <- attr[cat_name==unique(catdata[[i]]$cat_name)]
		attrvars = paste0(attrs$attribute_variables, '_bt')
		
		if (!nrow(attrs)==length(attrvars)) stop('Not all attribute-level combinations found in column headers for ', unique(catdata[[i]]$cat_name))
		
		attrs_rem = paste0(attrs[keep==F]$attribute_variables, '_bt')
		
		dt = catdata[[i]]
		
		vars_keep = vars[1:grep('cat_name', vars)]
		vars_keep = vars_keep[!vars_keep%in%attrs_rem]
		
		dt <- dt[,vars_keep,with=F]
		
		# mark attribute columns by prefix attr_
		for (.n in attrvars) {
			if (.n%in%colnames(dt)) setnames(dt, .n, paste0('attr_', .n))
			}
		setnames(dt, tolower(colnames(dt)))
		dat[[i]] <- dt
		}

names(dat) <- names(catdata)


###################################################################
# SELECT LONGEST CONSECUTIVE STRETCH OF AVAILABLE DATA per brand  #
###################################################################
require(zoo)

sink('..//audit//availablebrands.txt')

for (i in seq(along=dat)) {
	# variables to be used to determine availability
	vars = colnames(dat[[i]])

	# set zero sales to NA
	dat[[i]][sales_bt==0, sales_bt := NA]

	tmp <- split(dat[[i]], paste0(dat[[i]]$cat_name, '_', dat[[i]]$brand_name))

	dt <- rbindlist(lapply(tmp, function(x) {
		.zoo=zoo(x)
		.out = na.contiguous(.zoo)
		suppressWarnings(res <- x[, selected:=!1:.N %in% as.numeric(attr(.out, 'na.action'))])
		res
		# Add condition of at least 4 years of data (!)
		}))

	# --> number of obs.
	cat(paste0('\n\nCATEGORY: ', unique(dat[[i]]$cat_name), '\n'))
	print(dt[selected==T, list(nobs = .N), by = c('cat_name', 'brand_name')])

	dat[[i]] <- dt[selected==T]
	}
	
sink()

# save
	datasets <- dat
	save(datasets, file=c('..//output//datasets.RData'))
