# 
require(data.table)
require(lattice)
require(latticeExtra)

load('../output//datasets.RData')

##############################################
### Plot: Total sales in category by brand ###
##############################################

	path='../audit/sales_by_cat_brand/'
	unlink(paste0(path,'*'))
	dir.create(path)

	for (i in seq(along=datasets)) {
		dt = datasets[[i]]
		catname = unique(dt$cat_name)
		print(catname)
		
		png(paste0(path, 'sales_', catname, '.png'), res=200, units='in', height=8, width=12)
		print(xyplot(sales_bt~week, groups=brand_name,data=dt, auto.key=list(space="bottom", columns=2, 
							   title="Brands", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free")),
					
					main=paste('Category: ',catname),sep=''))
					
		dev.off()

	}

##############################################
### Plot: Total sales in category by brand ###
##############################################

	path='../audit/sales_by_cat/'
	unlink(paste0(path,'*'))
	dir.create(path)

	for (i in seq(along=datasets)) {
		dt = datasets[[i]]
		catname = unique(dt$cat_name)
		dt = dt[, list(sales_bt = sum(sales_bt)), by=c('week')][order(week)]
		print(catname)
		
		png(paste0(path, 'sales_', catname, '.png'), res=200, units='in', height=8, width=12)
		print(xyplot(sales_bt~week, data=dt, type='l', main=paste('Category: ',catname),sep=''))
					
		dev.off()

	}
	
	
###############################################
### Plot: Each variable by category and brand #
###############################################

	path='../audit/variables_by_cat/'
	unlink(paste0(path,'*'))
	dir.create(path)
	vars=c('sales_bt', 'rreg_pr_bt', 'pi_bt', 'pct_store_skus_bt','advertising_bt')
	
	for (i in seq(along=datasets)) {
		dt = melt(datasets[[i]][, colnames(datasets[[i]])%in%c(vars, 'brand_name', 'week'),with=F], id.vars=c('brand_name', 'week'))
		catname = unique(datasets[[i]]$cat_name)
		print(catname)
		
		png(paste0(path, 'sales_', catname, '.png'), res=200, units='in', height=8, width=12)
		print(xyplot(value~week|variable, groups=brand_name,data=dt, auto.key=list(space="bottom", columns=2, 
							   title="Brands", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free")),
					
					main=paste('Category: ',catname),sep=''))
					
		dev.off()

	}
	

##############################################
### Plot: Total sales in category by brand ###
##############################################

	path='../audit/variables_by_brand/'
	unlink(paste0(path,'*'))
	dir.create(path)
	vars=c('sales_bt', 'ms_bt', 'rreg_pr_bt', 'ract_pr_bt', 'reg_pr_bt', 'act_pr_bt', 'pi_bt', 'fd_bt', 'pct_store_skus_bt', 'distrwidth_bt', 'distrdepth_bt', 'advertising_bt', 'cpi')
	
	for (i in seq(along=datasets)) {
		dt = melt(datasets[[i]][, colnames(datasets[[i]])%in%c(vars, 'brand_name', 'week'),with=F], id.vars=c('brand_name', 'week'))
		catname = unique(datasets[[i]]$cat_name)
		print(catname)
		tmp=split(dt, dt$brand)
		for (j in seq(along=tmp)) {
			bname = names(tmp)[j]
			png(paste0(path, 'sales_', catname, '_', bname, '.png'), res=200, units='in', height=8, width=12)
			print(xyplot(value~week|variable, groups=brand_name,data=tmp[[j]],type='l', scales = list(y = list(relation = "free")),
						
						main=paste0('Category: ',catname, ' - Brand: ', bname),sep=''))
						
			dev.off()
			}

	}

