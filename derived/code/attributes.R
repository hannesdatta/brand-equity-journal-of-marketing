#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

									  
# Requires R-32 Bit.

### LOAD DATA SETS
require(data.table)
require(RODBC)
require(stringr)

# Gather attribute information
	attrfn = '..\\..\\..\\Attributes_check\\cat_attrdummy_lookup.xlsx'
	ch = odbcConnectExcel2007(attrfn)
	
# read in all sheets
	tables <- sqlTables(ch)
	
	attr <- NULL
	for (tb in seq(along=tables$TABLE_NAME)) {
		tb_name = tables$TABLE_NAME[tb]
		attr[[tb]] <- sqlFetch(ch, tb_name)
		colnames(attr[[tb]]) <- gsub(' ', '_', str_trim(tolower(colnames(attr[[tb]]))))
		attr[[tb]]$cat_name = gsub('[$]', '', tb_name)
		}
	names(attr) <- gsub('[$]', '', tables$TABLE_NAME)

	odbcCloseAll()
	
save(attr, file='../temp//attributes.RData')

