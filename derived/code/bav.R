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


load('..//temp//iri_sales.RData')

# Gathers all data sets, and saves them as RData
	bavdata <- NULL

	basepath <- '..\\..\\..\\'
	# Define available categories
	category_dirs = names(catdata)				 
					  
	open_sas <- function(.dir) {
	
		cat(paste0('Processing ', category_dirs[.dir], '...\n'))
		path = paste0(basepath, '\\', category_dirs[.dir], '\\revision\\')
		fs = list.files(path)
		fn = grep('brand[_]year.*[.]sas7bdat', fs, value=T)
		dt <- data.table(read.sas7bdat(paste0(path, fn)))
		return(dt)
		}
	
	bavdata <- lapply(seq(along=category_dirs), open_sas)
	names(bavdata) <- category_dirs
	
save(bavdata, file='../temp//bav.RData')
