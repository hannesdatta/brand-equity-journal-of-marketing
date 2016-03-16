#################################
## PREPARE META CHARACTERISTICS #
#################################

# category characteristics
cat_char = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2002]
	cat_name=unique(dt$cat_name)
	
	# concentration
		tmp=dt[, list(sales=sum(sales_bt,na.rm=T)),by=c('brand_name')]
		tmp[, ms := sales/sum(sales)]
		setorderv(tmp, 'sales',order=-1L)
		
		c4= sum(tmp$sales[1:4])/sum(tmp$sales)
		H = sum(tmp$ms^2)
		
	# growth rate
		tmp=dt[, list(sales_first=sum(sales_bt[year==min(year)],na.rm=T),
					  sales_last=sum(sales_bt[year==max(year)],na.rm=T),
					  nyears = length(unique(year)))]
		growth = (tmp$sales_last/tmp$sales_first)^(1/tmp$nyears)
	
	# category dummies
		# (1) 
		fooddrinks = ifelse(cat_name %in% c('beer', 'carbbev', 'coffee', 'coldcer', 'pz_di', 'ketchup', 'margbutr', 'mayo', 'milk', 'mustard', 'spagsauc', 'peanbutr', 'saltsnck', 'soup', 'sugarsub', 'yogurt'),1,0)

		# (2)
		hygiene = ifelse(cat_name %in% c('deod', 'diapers', 'rz_bl', 'shamp', 'toitisu', 'toothpa'), 1,0)
		hhclean = ifelse(cat_name %in% c('hhclean', 'laundet'), 1,0)
		food = ifelse(cat_name %in% c('coldcer', 'pz_di', 'ketchup', 'margbutr', 'mayo', 'mustard', 'spagsauc', 'peanbutr', 'saltsnck', 'soup', 'sugarsub', 'yogurt'),1,0)
		drinks = ifelse(cat_name %in% c('beer', 'carbbev', 'milk', 'coffee'), 1,0)
		cigs = ifelse(cat_name %in% c("cigets"), 1,0)
		
		#	perishable = ifelse(cat_name %in% c('coldcer', 'mayo', 'milk', 'yogurt'),1,0)
		#	impulse = ifelse(cat_name %in% c('saltsnck'),1,0)
		#	fooddrinks = ifelse(cat_name %in% c('beer', 'carbbev', 'coffee', 'coldcer', 'pz_di', 'ketchup', 'margbutr', 'mayo', 'milk', 'mustard', 'spagsauc', 'peanbutr', 'saltsnck', 'soup', 'sugarsub', 'yogurt'),1,0)
	
	data.frame(cat_name=cat_name, c4=c4, herf=H, cat_growth_abs = growth,
			   fooddrinks, hygiene, hhclean, food, drinks, cigs)
	}))

# brand characteristics
brand_char = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2002]
	
	# mean marketing mix, and sales/market share in a year
	tmp=dt[, list(sales=sum(sales_bt,na.rm=T), 
				  meanprice = mean(rreg_pr_bt,na.rm=T), sdpriceindex = sd(pi_bt,na.rm=T),
				  meanad = mean(advertising_bt,na.rm=T), secondary_category = as.numeric(unique(delete_bav)==1),
				  newbrnd_bav=unique(newbrnd_bav),
				  sales_yrfirst = sum(sales_bt[year==min(year)], na.rm=T),
				  sales_yrlast =  sum(sales_bt[year==max(year)], na.rm=T),
				  growth_years = max(year)-min(year)+1),
				  by=c('cat_name', 'brand_name')]
				  
	tmp[, ':=' (ms = sales/sum(sales), ms_yrfirst = sales_yrfirst/sum(sales_yrfirst), ms_yrlast = sales_yrlast/sum(sales_yrlast))]
	
	# function to scale x between -1 and +1
	std_by1 = function(x) {
		-1+2*(x/max(x,na.rm=T))
		}
		
	tmp=tmp[, list(brand_name=brand_name, ms=ms, 
				   pricepos = std_by1(meanprice), 
				   dealdepth = sdpriceindex, 
				   relad = std_by1(meanad), 
				   #secondary_cat = secondary_category,
				   new_brand = newbrnd_bav,
				   brand_growth_abs = (sales_yrlast/sales_yrfirst)^(1/growth_years), 
				   brand_growth_rel = (ms_yrlast/ms_yrfirst)^(1/growth_years)),
				   by=c('cat_name')]
				   
	tmp[, brand_light := ifelse(grepl('light', brand_name,ignore.case = TRUE), 1,0)]
	return(tmp)
	}))

setkey(cat_char, cat_name)
setkey(brand_char, cat_name) # by time, e.g., year?!
meta_char = brand_char[cat_char]

