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

# Brand selection has already been implemented in upstream-data preparation:
# brands are selected that account for a cumulative market share of 90%.

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
		vars <- colnames(catdata[[i]])
		
		attrs <- attr[cat_name==unique(catdata[[i]]$cat_name)]
		attrvars = paste0(attrs$attribute_variables, '_bt')
		
		if (!nrow(attrs)==length(attrvars)) stop('Not all attribute-level combinations found in column headers for ', unique(catdata[[i]]$cat_name))
		
		attrs_rem = paste0(attrs[keep==F]$attribute_variables, '_bt')
		
		dt = catdata[[i]]
		
		setkeyv(dt, setdiff(colnames(dt), 'AdStock_bt'))
		dt <- unique(dt)

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

# Verify whether data can be matched using brand_id (this requires that deletion information is completely identified using brand_id, and not brand_name)
test=bav_deletes[, list(check=length(unique(delete))==1),by=c('cat_name', 'brand_id')]
if(nrow(test[check==F])>0) stop('Please verify matching with BAV deletes.')
rm(test)
setkey(bav_deletes, cat_name, brand_id)

	for (i in seq(along=dat)) {
		dt = dat[[i]]
		
		bav = bavdata[[i]]
		
		setkey(dt, cat_name, brand_name, year)
		setkey(bav, cat_name, brand_name, year)
		dt[bav, ':=' (bav_energizeddiff = i.Energized_Differentiation_C, 
					  bav_relevance = i.Relevance_C,
					  bav_esteem = i.Esteem_C,
					  bav_knowledge = i.Knowledge_C,
					  bav_asset = i.Brand_Asset_C)]
		
		setkey(dt, cat_name, brand_id)
		dt[bav_deletes, ':=' (delete_bav = i.delete, newbrnd_bav = i.new_brnd, dyingbrnd_bav = i.dying_brnd)]
		dt[is.na(delete_bav), delete_bav := 0]
		
		dat[[i]] <- dt
		}

#################
# SELECT BRANDS #
#################

# Rule: Select all brands (BAV, and NON-BAV), provided we have at least three years of consecutive data for model estimation.

	brandsales <- rbindlist(lapply(dat, function(x) return(x[, c('cat_name', 'brand_name', 'brand_id', 'sales_bt', 'year', 'week', 'delete_bav'), with=F])))
	
	# calculate year-based market shares
	brandsales_yr <- brandsales[, list(sales = sum(sales_bt)), by = c('cat_name','brand_name','brand_id','week', 'year', 'delete_bav')]
	brandsales_yr[, ms := sales/sum(sales), by = c('cat_name','week', 'year')]
	
# check whether we always have consecutive observations
	brandsales_yr <- brandsales_yr[year>=2002]
	brandsales_yr[, consec_weeks := ifelse(c(min(week)-1, week[-.N])==week-1,1,0), by=c('cat_name','brand_name','brand_id')]
	brandsales_yr[, stretch_indicator := cumsum(1-consec_weeks), by=c('cat_name','brand_name')]
	brandsales_yr[, consec_weeks := ifelse(1:.N==1, 1, consec_weeks), by=c('cat_name','brand_name','stretch_indicator')]
	brandsales_yr[, stretch_length := sum(consec_weeks), by=c('cat_name','brand_name','stretch_indicator')]
	brandsales_yr[, max_stretch_length := max(stretch_length),by=c('cat_name','brand_name')]
	
	y=2 # minimum number of years required
	
	brandsales_yr[, selected := stretch_length==max(stretch_length) & stretch_length >= y*52,by=c('cat_name','brand_name')] # whether brand meets criteria in a given year

	selected_brands = brandsales_yr[, list(min_ms = round(min(ms),4), max_ms=round(max(ms),4), no_months = max(stretch_length),
										   selected = any(selected)), by=c('cat_name','brand_name', 'brand_id', 'delete_bav')]

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
	
	dat[[i]][, ':=' (brand_name = as.factor(as.character(brand_name)), brand_id = as.factor(as.character(brand_id)))]
	
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

	# add quarterly dummies
	dt[,quarter:=as.numeric(cut(month,c(0,3,6,9,12)))]
	
	dat[[i]] <- dt
	}

sink()

# save
	datasets <- dat
	save(datasets, file=c('..//output//datasets.RData'))
