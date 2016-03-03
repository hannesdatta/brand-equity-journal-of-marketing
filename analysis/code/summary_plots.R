
############
# PLOTTING #
############

	##########################
	# MARKETING ELASTICITIES #
	##########################
	
	## HISTOGRAM

	path='../audit/elast_hist_by_cat/'
	unlink(paste0(path,'*'))
	dir.create(path)
		
	for (j in unique(elast$cat_name)) {
		png(paste0(path, 'elasticities_', j, '.png'), res=200, units='in', height=8, width=12)
			par(mfrow=c(2,2))
		for (i in unique(elast[cat_name==j]$var_name)) {
			print(hist(elast[cat_name==j&var_name==i]$elast,main=paste0(j, ': ', i),xlab = 'elasticity'))
			}
		dev.off()

		}

	## BARPLOT
	path='../audit/elast_bar_by_cat/'
	unlink(paste0(path,'*'))
	dir.create(path)
		
	error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
		if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
		stop("vectors must be same length")
		print(arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...))
		}


	for (j in unique(elast$cat_name)) {
		png(paste0(path, 'elasticities_', j, '.png'), res=200, units='in', height=8, width=12)
		par(mfrow=c(2,2))
		for (i in unique(elast[cat_name==j&!is.na(elast)]$var_name)) {
			df = elast[cat_name==j&var_name==i]
			setorder(df, elast)
			barx <- barplot(df$elast, names.arg=df$brand_name,main=paste0(j, ': ', i),ylab = 'elasticity',ylim=c(min(df$elast-1.96*df$elast_se,na.rm=T),max(df$elast+1.96*df$elast_se,na.rm=T)))
			error.bar(barx,df$elast, 1.96*df$elast_se) 
			#print(hist(elast[cat_name==j&var_name==i]$elast,main=paste0(j, ': ', i),xlab = 'elasticity'))
			}
		dev.off()

		}	
	
	# SCATTERPLOT BY CBBE
	path='../audit/elast_cbbe_scatter_by_cat/'
	unlink(paste0(path,'*'))
	dir.create(path)
	
	
		for (i in unique(elast$var_name)) {
			png(paste0(path, 'elast_cbbe_', i, '.png'), res=200, units='in', height=8, width=12)
		
			df = elast[var_name==i]
			with(df, plot(x=elast_STD, y=F_RelEst_STD, main = paste0('Elasticities vs. CBBE: ', i),ylab='CBBE dimension'))
			with(df, points(x=elast_STD, y=F_Knowledge_STD,col='green'))
			with(df, points(x=elast_STD, y=F_EnergDiff_STD, col='blue'))
			
			legend("topleft", legend = c("RelEst", "Knowledge", "EnergDiff"),col=c('black', 'green', 'blue'),pch=1)
			
			dev.off()
			}

	
	########
	# SBBE #
	########
	
	## HISTOGRAM
	require(lattice)
	path='../audit/sbbe_by_cat/'
	unlink(paste0(path,'*'))
	dir.create(path)
	source('..//..//draft//code//proc_plots.R')
	
	for (j in unique(equity$cat_name)) {
		png(paste0(path, 'sbbe_', j, '.png'), res=200, units='in', height=8, width=12)
		df=equity[cat_name==j]
		df[, ':=' (upper=sbbe+1.96*sbbe_se, lower=sbbe-1.96*sbbe_se)]
		df[, grp:=1]

			
	print(xyplot(sbbe~year|brand_name, data = df, groups = grp,
		 upper = df$upper, lower = df$lower,
		 panel = function(x, y, ...){
		 panel.superpose(x, y, panel.groups = my.panel.bands, type='l', col='gray',...)
		 panel.xyplot(x, y, type='b', cex=0.6, lty=1,...)
		 }, main = paste0(j,'\n', 'Estimated SBBE with 1.96 x SE confidence bounds')))
		dev.off()

	}


