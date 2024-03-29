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
	load('..//temp//iri_sales.RData')
	load('..//temp//attributes.RData')
	load('..//temp//cpi.RData')
	load('..//temp//bav.RData')
	load('..//temp//bav_deletes.RData')
	load('..//temp//iri_aggr.RData')
# Brand selection has already been implemented in upstream-data preparation (Kusum, sas7bdat):
# brands are selected that account for a cumulative market share of 90%.

# In the latest edition of the data, there are duplicates in three weeks for brand Quaker Oats in coldcer. These observations (three starting in 2002) will be deleted.
	catdata$coldcer <- catdata$coldcer[!(brand_name=='Quaker Oats'&Brand_ID=='')]

################################
# PREPARE ATTRIBUTE-LEVEL DATA #
################################

	# Combine attribute information with panel data
	relevant_attr = which(names(attr)%in%names(catdata))

	nonavailability = names(catdata)[!names(catdata)%in%names(attr)]
	print(nonavailability)
	
	#if (!length(relevant_attr)==length(catdata)) stop('Not all attribute levels available, or a problem in the panel data')
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
		attrs <- attr[cat_name==unique(catdata[[i]]$cat_name)]
		attrvars = paste0(attrs$attribute_variables, '_bt')
		
		if (!nrow(attrs)==length(attrvars)) stop('Not all attribute-level combinations found in column headers for ', unique(catdata[[i]]$cat_name))
		
		attrs_rem = paste0(attrs[keep==F]$attribute_variables, '_bt')
		
		dt = catdata[[i]]
		
		setkeyv(dt, setdiff(colnames(dt), 'AdStock_bt'))
		dt <- unique(dt)
		dt[, brand_name_orig := brand_name]
		dt[, brand_name_orig := gsub("`", "\'", brand_name_orig)]
		dt[which(tolower(brand_name_orig)=="all"), brand_name_orig := "All"]
		
		simpleCap <- function(x) {
		  s <- strsplit(x, " ")[[1]]
		  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
			  sep="", collapse=" ")
		}
		
		dt[, brand_name_orig := sapply(brand_name_orig, simpleCap)]
		dt[, brand_name_orig := gsub('[(].*', '', brand_name_orig)]
		
		setcolorder(dt, c('brand_name', 'brand_name_orig', setdiff(colnames(dt), c('brand_name','brand_name_orig'))))
		
		vars <- colnames(dt)
		
		vars_keep = vars[1:grep("cat_name", vars)]
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

########################
# ADD CPIs to DATASETS #
########################

	for (i in seq(along=dat)) {
		dt = dat[[i]]
		
		#####################################
		# Convert IRI weeks to actual dates #
		#####################################
		
		# retrieved from: http://pubsonline.informs.org/doi/suppl/10.1287/mksc.1080.0450/suppl_file/mksc.1080.0450-sm_technical_appendix.pdf [accessed 3 February 2016]
		
		#This provides a translation from the IRI week number used in the files to the standard
		#calendar. [TBD: extend backward to beginning of actual data]
		#The following conversion formulas will also work in an excel context, assuming the week
		#is in cell A9
		#(equation 1) End date = (A9-400)*7+31900
		#(equation 2) Start date = (A9-400)*7+31900-6
		#So, IRI week 1369 evaluates to a start date of 11/21/2005 and an end date of
		#11/27/2005. 
		
		# return max occurence
		max_occurence <- function(x, what = '%Y') {
			.x <- seq(from=x, to=x+6, by = '1 day')
			.tmp = sapply(.x, format, what)
			if (length(unique(.tmp))==1) return(as.numeric(unique(.tmp)))
			
			# which one is max?!
			.tmp <- table(.tmp)
			as.numeric(names(which(.tmp==max(.tmp))))
			}
	
		#max_occurence(as.Date('2012-01-01'), what = '%m')
	
		dates <- data.table(week = unique(dt$week))
		dates[, week_start := as.Date('2005-11-21') + 7*(week - 1369)]
		dates[, year_iri := sapply(week_start, max_occurence, '%Y')]
		dates[, month_iri := sapply(week_start, max_occurence, '%m')]
		setkey(dates, week)
		setkey(dt, week)
		
		dt[dates, ':=' (year_iri = i.year_iri, month_iri=i.month_iri)]
		
		cpi_match = cpi
		cpi_match=cpi_match[cat_name==unique(dt$cat_name)]
		setorder(cpi_match, year,month)
		cpi_match[, cpi_normalized := value/value[1]]
		
		setkey(cpi_match, year, month)
		setkey(dt, year_iri, month_iri)
		
		dt[cpi_match, cpi:=i.cpi_normalized]

		# normalize CPI by the first week observation in our data set
		
		
		#################################################
		# Change definition of year to IRI's definition #
		#################################################
				
		dt[, ':=' (year = NULL)]
		setnames(dt, 'year_iri', 'year')
		setnames(dt, 'month_iri', 'month')
		
		##################################
		# Deflate monetary series by CPI #
		##################################
		
		vars <- c('act_pr_bt', 'reg_pr_bt', 'rev_bt') # but not advertising.
		
		for (.var in vars) {
			dt[, paste0('r', .var) := get(.var)/cpi,with=F]
			}
		
		dat[[i]] <- dt
		
		
		}

#####################################################################
# ADD BAV DATA AND                                                  #
# ADD INDICATORS WHETHER BAV BRANDS ARE TO BE "DEACTIVATED"/DELETED #
# (e.g., Lipton in soup (as its major category is ice tea)          #
#####################################################################

setkey(bav_deletes, cat_name, brand_name)

	for (i in seq(along=dat)) {
		dt = dat[[i]]
		dt[, brand_name_charact := gsub('[^a-zA-Z]', '', brand_name)]

		bav = bavdata[[i]]
		
		setkey(dt, cat_name, brand_name, year)
		setkey(bav, cat_name, brand_name, year)
		dt[bav, ':=' (bav_energizeddiff = i.Energized_Differentiation_C, 
					  bav_relevance = i.Relevance_C,
					  bav_esteem = i.Esteem_C,
					  bav_knowledge = i.Knowledge_C,
					  bav_asset = i.Brand_Asset_C)]
		
		setkey(dt, cat_name, brand_name_charact)
		dt[bav_deletes, ':=' (brnd_delete = i.delete, seccat_bav = i.bav_secondarycat, newbrnd_bav = i.bav_newbrnd, dyingbrnd_bav = i.bav_dyingbrnd,
							  upd_noseccatnew=i.upd_noseccatnew, upd_seccat = i.upd_seccat, upd_new = i.upd_new, upd_seccatandnew = i.upd_seccatandnew)]
		dt[is.na(brnd_delete), brnd_delete := 0]
		
		dat[[i]] <- dt
		}


#################
# SELECT BRANDS #
#################

# Rule: Select all brands (BAV, and NON-BAV), provided we have at least three years of consecutive data for model estimation.

	brandsales <- rbindlist(lapply(dat, function(x) return(x[, c('cat_name', 'brand_name', 'brand_id', 'sales_bt', 'year', 'week', 'brnd_delete'), with=F])))
	
	# calculate year-based market shares
	brandsales_yr <- brandsales[, list(sales = sum(sales_bt)), by = c('cat_name','brand_name','brand_id','week', 'year', 'brnd_delete')]
	brandsales_yr[, ms := sales/sum(sales), by = c('cat_name','week', 'year')]
	
# check whether we always have consecutive observations
	brandsales_yr <- brandsales_yr[year>=2002]
	brandsales_yr[, consec_weeks := ifelse(c(min(week)-1, week[-.N])==week-1,1,0), by=c('cat_name','brand_name','brand_id')]
	brandsales_yr[, stretch_indicator := cumsum(1-consec_weeks), by=c('cat_name','brand_name')]
	brandsales_yr[, consec_weeks := ifelse(1:.N==1, 1, consec_weeks), by=c('cat_name','brand_name','stretch_indicator')]
	brandsales_yr[, stretch_length := sum(consec_weeks), by=c('cat_name','brand_name','stretch_indicator')]
	brandsales_yr[, max_stretch_length := max(stretch_length),by=c('cat_name','brand_name')]
	
	y=2 # minimum number of years required
	
	brandsales_yr[, selected := stretch_length==max(stretch_length) & stretch_length >= y*52 & brnd_delete==0,by=c('cat_name','brand_name')] # whether brand meets criteria in a given year

	selected_brands = brandsales_yr[, list(min_ms = round(min(ms),4), max_ms=round(max(ms),4), no_months = max(stretch_length),
										   selected = any(selected)), by=c('cat_name','brand_name', 'brand_id', 'brnd_delete')]

    # prepare overview
	sel_overview = selected_brands[, list(total_brands = length(unique(brand_name)), 
										total_brands_for_estimation = length(unique(brand_name[selected==T])),
										total_bav_brands = length(unique(brand_name[!brand_id==''])),
										total_bav_brands_for_estimation =  length(unique(brand_name[!brand_id=='' & selected == T]))
									), by=c('cat_name')]
	sel_overview_total = cbind(cat_name='total (sum)', sel_overview[, lapply(.SD, sum), .SDcols=grep('total[_]', colnames(sel_overview),value=T)])
	
	sel_overview_totalu = selected_brands[, list(total_brands = length(unique(brand_name)), 
										total_brands_for_estimation = length(unique(brand_name[selected==T])),
										total_bav_brands = length(unique(brand_name[!brand_id==''])),
										total_bav_brands_for_estimation =  length(unique(brand_name[!brand_id=='' & selected == T]))
									)]
	
	sel_overview[, cat_name:=as.character(cat_name)]
	sel_overview_totalu[, cat_name:='total (unique)']
	sel_overview=rbindlist(list(sel_overview, sel_overview_total, sel_overview_totalu),fill=T)
	
	options(width=600)
	sink('..//audit//brand_selection.txt')
	cat(paste0('Overview about selected brands\n===========================================\n\nRule: All BAV brands and all other brands given at least ', y, ' years of consecutive data.\n\n'))
	print(sel_overview)
	cat('\n\n\n')
	options(scipen=999)
	print(data.frame(selected_brands),digits=4)
	write.table(selected_brands, sep='\t', file='..//audit//brand_selection.csv',row.names=F)
	sink()

	selected_brands = selected_brands[selected==T]
	setkey(selected_brands, cat_name, brand_name)

# Assess whether brand_name uniquely identifies a brand correctly
# (e.g., this identifier would be erroneous if it linked to different CBBE metrics by brand/category)

	tmp=rbindlist(lapply(bavdata, function(x) x[, c('cat_name', 'brand_name','Brand_ID', 'year', 'Brand_Asset_C'), with=F]))
	tmp=tmp[!is.na(Brand_Asset_C)]

	setkey(tmp, cat_name, brand_name)
	setkey(selected_brands, cat_name, brand_name)

	tmp2=tmp[selected_brands]
	setkey(tmp2, cat_name, brand_name)

	unique(tmp2) # --> is 441. ok.

	# verify whether Brand Asset score is unique by brand_name.
	setkey(tmp2, brand_name, year)
	unique(tmp2)
	setkey(tmp2, brand_name, Brand_Asset_C)
	unique(tmp2)

	tmp2[, N:=length(unique(Brand_Asset_C)), by = c('brand_name', 'year')]
	table(tmp2$N) # --> all is 1. Hence, all is good.

#########################################################################
# PRODUCE AGGREGATION INFORMATION (LINKING TO IRI) FOR SELECTED BRANDS  #
#########################################################################


bav <- rbindlist(lapply(aggr, function(x) cbind(cat_name_iri = x$cat_name_iri, 
												iri_brand_name = x$data[,c(seq(from=which(colnames(x$data)=='iri_brand_name'), length.out=3))],
												x$data[, grep('orig', colnames(x$data)[1], ignore.case=T)])),fill=T)

nonbav <- rbindlist(lapply(aggr, function(x) data.frame(cat_name_iri = x$cat_name_iri, 
												   iri_brand_name = as.character(x$data[,which(colnames(x$data)=='iri_brand_name1')]), 
												   brand_name = as.character(x$data[,which(colnames(x$data)=='iri_brand_name1')+1]),
												   x$data[, which(colnames(x$data)=='iri_cat')])
												   ),fill=T)

setnames(bav, c('cat_name_iri', 'iri_brand_name', 'brand_name', 'bav_id', 'iri_cat'))
setnames(nonbav, c('cat_name_iri', 'iri_brand_name', 'brand_name', 'iri_cat'))

brands <- rbind(bav, nonbav, fill=T)
brands <- brands[!is.na(iri_brand_name)]

# trim strings
require(stringr)
brands[, iri_brand_name := str_trim(iri_brand_name)]
brands[, brand_name := str_trim(brand_name)]

# all matched
table(selected_brands$brand_name%in%brands$brand_name)

# Merge our own category names to it
brands = merge(brands, selected_brands[selected==T,c('cat_name', 'brand_name'),with=F], by=c('brand_name'), all.x=F, all.y=T)

# Reclassify brands into original categories
setnames(brands, 'cat_name_iri', 'cat_name_iri_old')

brands[cat_name_iri_old == 'beer', cat_name_iri := 'BEER']
brands[cat_name_iri_old == 'carbbev', cat_name_iri := 'CARBONATED BEVARAGES']
brands[cat_name_iri_old == 'cigets', cat_name_iri := 'CIGARETTES']
brands[cat_name_iri_old == 'coffee', cat_name_iri := 'COFFEE']
brands[cat_name_iri_old == 'coldcer', cat_name_iri := 'COLD CEREAL']
brands[cat_name_iri_old == 'deod', cat_name_iri := 'DEODORANT']
brands[cat_name_iri_old == 'diapers', cat_name_iri := 'DIAPERS']
brands[cat_name_iri_old == 'hhclean', cat_name_iri := 'HOUSEHOLD CLEANER']
brands[cat_name_iri_old == 'laundet', cat_name_iri := 'LAUNDRY DETERGENT']
brands[cat_name_iri_old == 'margbutr', cat_name_iri := 'MARGARINE/SPREADS/BUTT']
brands[cat_name_iri_old == 'mayo', cat_name_iri := 'MAYONNAISE']
brands[cat_name_iri_old == 'milk', cat_name_iri := 'MILK']
brands[cat_name_iri_old == 'mustketc', cat_name_iri := 'MUSTARD & KETCHUP']
brands[cat_name_iri_old == 'pastasauc', cat_name_iri := 'SPAGHETTI/ITALIAN SAUC']
brands[cat_name_iri_old == 'peanbutr', cat_name_iri := 'PEANUT BUTTER']
brands[cat_name_iri_old == 'saltsnck', cat_name_iri := 'SALTY SNACKS']
brands[cat_name_iri_old == 'shamp', cat_name_iri := 'SHAMPOO']
brands[cat_name_iri_old == 'soup', cat_name_iri := 'SOUP']
brands[cat_name_iri_old == 'sugarsub', cat_name_iri := 'SUGAR SUBSTITUTES']
brands[cat_name_iri_old == 'toitisu', cat_name_iri := 'TOILET TISSUE']
brands[cat_name_iri_old == 'toothpa', cat_name_iri := 'TOOTHPASTE']
brands[cat_name_iri_old == 'yogurt', cat_name_iri := 'YOGURT']

brands[iri_cat == 'FZDINENT' | iri_cat == 'CATEGORY - FZ DINNERS/ENTREES', cat_name_iri := 'FZ DINNERS/ENTREES']
brands[iri_cat == 'PIZZA' | iri_cat == 'CATEGORY - FZ PIZZA', cat_name_iri := 'FZ PIZZA']
brands[iri_cat == 'Blades' | iri_cat == 'CATEGORY - BLADES', cat_name_iri := 'BLADES']
brands[iri_cat == 'Razors' | iri_cat == 'CATEGORY - RAZORS', cat_name_iri := 'RAZORS']

# keep brands that we have in our data set
#brands = brands[brand_name %in% selected_brands$brand_name]

# keep only matching categories
brands[, cat_name_iri := as.character(cat_name_iri)]
brands[, cat_name_iri_old := as.character(cat_name_iri_old)]
brands[, cat_name := as.character(cat_name)]

brands <- brands[cat_name==cat_name_iri_old | cat_name == 'pz_di' & cat_name_iri_old=='fz_pz' | cat_name == 'spagsauc' & cat_name_iri_old == 'pastasauc' | cat_name == 'rz_bl' & cat_name_iri_old == 'razors_blades' | 
			     cat_name == 'ketchup' & cat_name_iri_old == 'mustketc' | cat_name == 'mustard' & cat_name_iri_old == 'mustketc']
	
# Remove unnecessary columns
brands[, ':=' (cat_name_iri_old = NULL, bav_id = NULL, iri_cat = NULL)]

# Rename
setnames(brands, 'cat_name_iri', 'iri_cat_name')

# Reorder columns
setcolorder(brands, c('iri_cat_name', 'iri_brand_name', 'brand_name', 'cat_name'))
setorder(brands, iri_cat_name, iri_brand_name, brand_name, cat_name)
	
# count unique brands
brands[, list(.N), by = c('cat_name', 'brand_name')]

# filter uniques
setkey(brands, iri_cat_name, brand_name, iri_brand_name, cat_name)
brands= unique(brands)
setorder(brands, iri_cat_name, brand_name, iri_brand_name, cat_name)

# save file
write.csv(brands, file='..//output//brands.csv', row.names=F, quote=F)

###########################################################################################
# KEEP SELECTED BRANDS, AND TAKE LONGEST CONSECUTIVE STRETCH OF AVAILABLE DATA per brand  #
###########################################################################################
require(zoo)

sink('..//audit//brand_selection.txt', append = T)
	
for (i in seq(along=dat)) {
	# variables to be used to determine availability
	vars = colnames(dat[[i]])

	# set zero sales to NA
	dat[[i]][sales_bt==0, sales_bt := NA]
	
	# retain only selected brands
	setkey(dat[[i]], cat_name, brand_name)
	
	dat[[i]][selected_brands, selected_brand := i.selected]
	dat[[i]] <- dat[[i]][selected_brand==T]
	dat[[i]][, selected_brand := NULL]
	
	dat[[i]][, ':=' (brand_name = as.factor(tolower(as.character(brand_name))), brand_id = as.factor(tolower(as.character(brand_id))))]
	
	tmp <- split(dat[[i]], paste0(dat[[i]]$cat_name, '_', dat[[i]]$brand_name))

	dt <- rbindlist(lapply(tmp, function(x) {
		.zoo=zoo(x[, !grepl('bav[_]|[_]bav', colnames(x)),with=F])
		.out = na.contiguous(.zoo)
		suppressWarnings(res <- x[, selected:=!1:.N %in% as.numeric(attr(.out, 'na.action'))])
		res
		}))

	# --> number of obs.
	cat(paste0('\n\nCATEGORY: ', unique(dat[[i]]$cat_name), '\n'))
	print(dt[selected==T, list(nobs = .N), by = c('cat_name', 'brand_name')])
	
	# make brand names ASCII compatible (retain only characters, remove spaces)
	dt[, brand_name := gsub('[^a-zA-Z]', '', brand_name)]
	
	# delete pre-computed ad-stock variable
	dt[, adstock_bt := NULL]
	
	# keep only data for 2002 onwards
	
	# delete non-selected brands
	dt <- dt[selected==T]
	dt[, selected:=NULL]
	
	dt[, pct_store_skus_bt := distrwidth_bt * distrdepth_bt]
	
	# compute market share
	dt[, ms_bt := sales_bt/sum(sales_bt), by=c('week')]

	# scale advertising expenditure from 1,000 spending to 1 US$ spending.
	dt[, advertising_bt := advertising_bt*1000]
	
	# add quarterly dummies
	dt[, quarter:=as.numeric(cut(month,c(0,3,6,9,12)))]
	setkey(dt, brand_name, week)
	dat[[i]] <- dt
	}

sink()

# save
	datasets <- dat
	save(datasets, file=c('..//output//datasets.RData'))
