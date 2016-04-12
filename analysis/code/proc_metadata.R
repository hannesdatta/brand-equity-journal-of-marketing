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
		
		c2= sum(tmp$sales[1:2])/sum(tmp$sales)
		c3= sum(tmp$sales[1:3])/sum(tmp$sales)
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
	
	data.frame(cat_name=cat_name, c2=c2, c3=c3, c4=c4, herf=H,
			   fooddrinks, hygiene, hhclean, food, drinks, cigs, catgrowth_abs = growth)
	}))

cat_char_ms = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2002]
	cat_name=unique(dt$cat_name)
	tmp=dt[, list(sales_first=sum(sales_bt[year==min(year)],na.rm=T),
				  sales_last=sum(sales_bt[year==max(year)],na.rm=T),
				  nyears = length(unique(year))), by=c('cat_name')]
	}))
	
cat_char_ms[, ':=' (ms_first = sales_first/sum(sales_first), ms_last = sales_last/sum(sales_last))]
cat_char_ms[, catgrowth_rel := (ms_last/ms_first)^(1/nyears)]

setkey(cat_char_ms, cat_name)
setkey(cat_char, cat_name)

cat_char[cat_char_ms, catgrowth_rel := i.catgrowth_rel]

# brand characteristics
brand_char = rbindlist(lapply(datasets, function(x) {
	dt=x[year>=2002]
	
	# mean marketing mix, and sales/market share in a year
	tmp=dt[, list(sales=sum(sales_bt,na.rm=T), 
				  meanprice = mean(rreg_pr_bt,na.rm=T), sdpriceindex = sd(pi_bt,na.rm=T),
				  meanad = mean(advertising_bt,na.rm=T),
				  newbrnd_bav=unique(newbrnd_bav),
				  seccat_bav=unique(seccat_bav),
				  dyingbrnd_bav=unique(dyingbrnd_bav),
				  
				  sales_yrfirst = sum(sales_bt[year==min(year)], na.rm=T),
				  sales_yrlast =  sum(sales_bt[year==max(year)], na.rm=T),
				  growth_years = max(year)-min(year)+1),
				  by=c('cat_name', 'brand_name')]
				  
	tmp[, ':=' (ms = sales/sum(sales), ms_yrfirst = sales_yrfirst/sum(sales_yrfirst), ms_yrlast = sales_yrlast/sum(sales_yrlast))]
	
	# function to scale x between -1 and +1
	std_by1 = function(x) {
		-1+2*(x/max(x,na.rm=T))
		}
		
	tmp=tmp[, list(brand_name=brand_name, avg_ms=ms, 
				   pricepos = std_by1(meanprice), 
				   dealdepth = sdpriceindex, 
				   relad = std_by1(meanad), 
				   seccat = seccat_bav,
				   newbrnd = newbrnd_bav,
				   dyingbrnd = dyingbrnd_bav,
				   brndgrowth_abs = (sales_yrlast/sales_yrfirst)^(1/growth_years), 
				   brndgrowth_rel = (ms_yrlast/ms_yrfirst)^(1/growth_years)),
				   by=c('cat_name')]
				   
	tmp[, brand_light := ifelse(grepl('light|diet|weightwatch', brand_name,ignore.case = TRUE), 1,0)]
	return(tmp)
	}))

setkey(brand_char, cat_name) # by time, e.g., year?!

meta_char = brand_char[cat_char]

