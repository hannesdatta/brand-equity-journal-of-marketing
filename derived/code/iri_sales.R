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
	category_dirs = c('sugarsub', 'cigets', 'coffee', 'margbutr', 'toitisu', 'yogurt', 'beer', 'milk',
					  'carbbev', 'coldcer', 'deod', 'diapers', 'hhclean', 'laundet')

	for (.dir in seq(along=category_dirs)) {
		cat(paste0('Processing ', category_dirs[.dir], '...\n'))
		path = paste0(basepath, '\\', category_dirs[.dir], '\\revision\\')
		fs = list.files(path)
		fn = grep('varneed[.]sas7bdat', fs, value=T)
		dt <- data.table(read.sas7bdat(paste0(path, fn)))
		colindex <- 1:grep('cat_name', colnames(dt))
		catdata[[.dir]] <- dt[, colindex, with=F]
		}
	
	names(catdata) <- category_dirs
	
save(catdata, file='../temp//iri_sales.RData')


