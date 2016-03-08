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


# Gathers all data sets, and saves them as RData
	catdata <- NULL

	basepath <- '..\\..\\..\\'
	# Define available categories
	category_dirs = c('beer', 'carbbev', 'cigets', 'coffee', 'coldcer', 'deod', 'diapers', 'pz_di', 'hhclean', 'ketchup', 'laundet', 'margbutr', 'mayo', 'milk', 'mustard', 'spagsauc', 'peanbutr', 'rz_bl', 
					   'saltsnck', 'shamp', 'soup', 'sugarsub', 'toitisu', 'toothpa', 'yogurt') 
	category_dirs = c('sugarsub', 'peanbutr', 'saltsnck', 'mustard', 'ketchup')
					 
	# verify whether all data sets can be located
	for (.dir in seq(along=category_dirs)) {
		path = paste0(basepath, '\\', category_dirs[.dir], '\\revision\\fix_distrwidth\\')
		fs = list.files(path)
		fn = grep('varneed[.]sas7bdat', fs, value=T)
		if (!file.exists(paste0(path,fn))) stop('File ', fn, ' in ', path, ' does not exist.')
		}
					  
	require(parallel)
	cl<-makePSOCKcluster(24)
	clusterEvalQ(cl, require(data.table))
	clusterEvalQ(cl, require(sas7bdat))
	
	open_sas <- function(.dir) {
	
		cat(paste0('Processing ', category_dirs[.dir], '...\n'))
		path = paste0(basepath, '\\', category_dirs[.dir], '\\revision\\')
		fs = list.files(path)
		fn = grep('varneed[.]sas7bdat', fs, value=T)
		dt <- data.table(read.sas7bdat(paste0(path, fn)))
		colindex <- 1:grep('cat_name', colnames(dt))
		return(dt[, colindex, with=F])
		#
		}
	
	clusterExport(cl, c('basepath','category_dirs','open_sas'))

	catdata <- clusterApply(cl, seq(along=category_dirs), open_sas)
	names(catdata) <- category_dirs
	stopCluster(cl)
	
save(catdata, file='../temp//iri_sales.RData')
