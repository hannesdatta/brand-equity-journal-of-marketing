### LOAD DATA SETS
require(data.table)
require(RODBC)
require(stringr)

# Gathers all data sets, and saves them as RData
	aggr <- NULL

	basepath <- '..\\..\\..\\other\\matching\\Revision\\'

	fs = list.files(basepath, pattern = 'xlsx', full.names=T)
	
	# load data
	for (fn in seq(along=fs)) {
		cat(paste0(fs[fn], '...\n'))
		ch = odbcConnectExcel2007(fs[fn])
		tables <- data.table(sqlTables(ch))
	
		# spot correct table name
		tb_name = tables[grepl('[_]MATCH', TABLE_NAME)&!grepl('NO[_]MATCH', TABLE_NAME)&TABLE_TYPE=='TABLE']$TABLE_NAME
		
		qry <- paste0("SELECT * FROM `", tb_name, "$`")
		tmp <- sqlQuery(ch, qry)

		odbcCloseAll()
		
		cat_name_iri = tolower(gsub('[_]match.*', '', rev(strsplit(fs[fn], '\\\\')[[1]])[1]))
		
		aggr[[fn]] <- list(data=tmp, cat_name_iri=cat_name_iri)
		}

		
save(aggr, file='../temp//iri_aggr.RData')
