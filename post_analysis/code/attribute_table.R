# Load data.table
require(data.table)
options(width=1000)
require(reshape2)

# Load data 
	load('..//..//derived//output//datasets.RData')

# Extract attributes
	dt <- lapply(datasets, function(x) x[, c('cat_name', grep('attr[_]', colnames(x),value=T)), with=F])

	
	
	dt <- rbindlist(lapply(datasets, function(x) {
	
		tmp = x[year>=2002, lapply(.SD, function(x) c(min=min(x), mean=mean(x), max=max(x))), by='cat_name', .SDcols=grep('attr[_]', colnames(x),value=T)]
		tmp2 = melt(tmp, id.vars=c('cat_name'))
		tmp2[, stat:=rep(c('min', 'mean', 'max'), length.out=nrow(tmp2))]
		res = dcast(tmp2, cat_name+variable~stat)
		setcolorder(res, c('cat_name', 'variable', 'min', 'mean', 'max'))
		res
		}))
	
	dt <- dt[!min==max]
	
	dt[, attribute_variables := gsub('attr[_]', '', variable)]
	dt[, attribute_variables := gsub('[_]bt', '', attribute_variables)]
	
load('../../derived/temp/attributes.RData')

attrs <- rbindlist(lapply(attr[names(datasets)], function(x) x[, c('attribute', 'attribute_variables', 'cat_name')]))

for (i in 2:nrow(attrs)) {
	if (is.na(attrs$attribute[i])) attrs$attribute[i] <- attrs$attribute[i-1]
	}
attrs[, order := seq(length.out=.N), by=c('cat_name', 'attribute')]

# Remove attributes that are NOT included

attrs <- merge(attrs, dt, by=c('cat_name', 'attribute_variables'), all.x=T)
attrs[, attr_order := .GRP, by=c('cat_name', 'attribute')]

# Make attribute variables printable (first letter uppercase; rest lowercase)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

attrs[, print_attr_var := sapply(attribute_variables, function(x) {
	paste0(toupper(substr(trim(x), 1,1)), tolower(substr(trim(x),2,nchar(trim(x)))))
	})]
	
# Reset other* to 'Other'
attrs[grepl('other', attribute_variables, ignore.case=TRUE), print_attr_var:='Other']

# Change order
attrs[print_attr_var=='Other', order := 100]
setorder(attrs, cat_name, attr_order, order)

attrs[is.na(min), print_attr_var := paste0(print_attr_var, '*')]

attrs[, included_dimension := !all(is.na(mean)), by=c('cat_name', 'attribute')]

# Make dimensions printable
attrs[, printable_dimension := sapply(as.character(attribute), function(x) {
	tmp=strsplit(x, '_',fixed=T)
	tmp = lapply(tmp, function(x) paste0(toupper(substr(trim(x), 1,1)), tolower(substr(trim(x),2,nchar(trim(x))))))[[1]]
	tmp[which(tmp=='Of')] <- 'of'
	paste(tmp, collapse = ' ')

	})]
#attrs[grepl('type', printable_dimension, ignore.case=T), printable_dimension := 'Type']
#attrs[grepl('type', printable_dimension, ignore.case=T), printable_dimension := 'Type']

# Replace category names
cat_names <- fread('../../raw/category_names/category_names.txt')

attrs <- merge(attrs, cat_names, by=c('cat_name'))

overview <- attrs[!is.na(attribute_variables) & included_dimension==T, list(variables = paste(print_attr_var, collapse=', ')), by=c('cat_name_full', 'printable_dimension')]
setnames(overview, c('Category', 'Attribute', 'Levels'))


write.table(overview, '../output/attribute_overview.csv', row.names=F)


