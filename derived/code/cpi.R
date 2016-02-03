#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     

									  
# Opens CPI data from SPSS.

require(data.table)
require(foreign)
require(lattice)
require(latticeExtra)

# read data
	cpi <- data.table(read.spss('../../raw/cpi/CPI and GDP figures.sav', to.data.frame = T))
	
# drop unnecessary columns
	cpi <- cpi[,!colnames(cpi)%in%grep('gdp',colnames(cpi), ignore.case=T,value=T),with=F]
	
# melt
	cpi <- melt(cpi, id.vars=c('year', 'month'))
	setnames(cpi, 'variable', 'cpi_category')
	setkey(cpi, cpi_category)
	
# I need to create a match between every category type and my actual category labels
	cpi_match <- fread('../../raw/cpi/catmapping.csv')
	setkey(cpi_match, cpi_category)

	cpi <- merge(cpi_match, cpi, by=c('cpi_category'), all.x=T, all.y=T,allow.cartesian=TRUE)
	

# Drop categories to-be-ignored
	cpi <- cpi[!grepl('ignore', cat_name)]
	cpi[, date := as.Date(paste0(year, '-', formatC(month,width=2,flag='0'), '-01'))]
	
	cpi[, ':=' (cpi_category=NULL, comparison_with_harald=NULL, year = as.numeric(year), month=as.numeric(month))]
	
	save(cpi, file = '../temp/cpi.RData')
	
# Generate plot
	path='../audit/cpi/'
	unlink(paste0(path,'*'))
	dir.create(path)

	png(paste0(path, 'cpi.png'), res=200, units='in', height=8, width=12)
	print(xyplot(value~date|cat_name,data=cpi,type='l', scales = list(y = list(relation = "free")), main = 'CPI by category'))

	dev.off()
