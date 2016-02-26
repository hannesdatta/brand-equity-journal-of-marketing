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
	fn = '..\\..\\raw\\bav_deletes\\bav_deletes.xlsx'
	ch = odbcConnectExcel2007(fn)
	
# read in all sheets
	tables <- sqlTables(ch)
	
	bav_deletes <- NULL
	tb_name = 'Sheet1'
	bav_deletes <- sqlFetch(ch, tb_name)
	colnames(bav_deletes) <- gsub(' ', '_', str_trim(tolower(colnames(bav_deletes))))
	
	#bav_deletes$brand_name = gsub('[^a-zA-Z]', '', bav_deletes$brand_name)
	
	odbcCloseAll()
bav_deletes=data.table(bav_deletes)

save(bav_deletes, file='../temp//bav_deletes.RData')

