
############
# PLOTTING #
############

	##########################
	# MARKETING ELASTICITIES #
	##########################
	elast[!is.na(bav_asset)]
	## HISTOGRAM
	if (0){
	path=paste0('../output/', r, '/elast_hist/')
	unlink(paste0(path,'*'))
	dir.create(path)
		
	for (j in unique(elast$cat_name)) {
		png(paste0(path, 'elasticities_', j, '.png'), res=200, units='in', height=8, width=12)
		par(mfrow=c(2,3))
		for (i in unique(elast[cat_name==j]$var_name)) {
			hist(elast[cat_name==j&var_name==i]$elast,main=paste0(j, ': ', i),xlab = 'elasticity')
			}
		dev.off()

		}

	## BARPLOT
	path=paste0('../output/', r, '/elast_bar/')
	unlink(paste0(path,'*'))
	dir.create(path)
		
	error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
		if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
		stop("vectors must be same length")
		print(arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...))
		}


	for (j in unique(elast$cat_name)) {
		png(paste0(path, 'elasticities_', j, '.png'), res=200, units='in', height=8, width=12)
		par(mfrow=c(2,3))
		for (i in unique(elast[cat_name==j&!is.na(elast)]$var_name)) {
			df = elast[cat_name==j&var_name==i]
			setorder(df, elast)
			barx <- barplot(df$elast, names.arg=df$brand_name,main=paste0(j, ': ', i),ylab = 'elasticity',ylim=c(min(df$elast-1.96*df$elast_se,na.rm=T),max(df$elast+1.96*df$elast_se,na.rm=T)))
			error.bar(barx,df$elast, 1.96*df$elast_se) 
			}
		dev.off()

		}	
	
	
	# SCATTERPLOT BY CATEGORY
	path=paste0('../output/', r, '/elast_vs_cbbe/')
	unlink(paste0(path,'*'))
	dir.create(path)
	require(lattice)
	
		for (i in unique(elast$var_name)) {
			png(paste0(path, 'elast_cbbe_', i, '.png'), res=200, units='in', height=16, width=16)
		
			df = elast[var_name==i]
			print(xyplot(elast~F_RelEstKnow+F_EnergDiff|cat_name,data=df,auto.key=T, main = paste0('Elasticities vs. CBBE: ', i),scales = list(y = list(relation = "free"))))
			
			dev.off()
			}

	########
	# SBBE #
	########
	
	## LONGITUDINAL OBSERVATIONS BY CATEGORY
	
	require(lattice)
	path=paste0('../output/', r, '/sbbe/')
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

	}
	
	# SCATTERPLOT SBBE vs. CBBE
	path=paste0('../output/', r, '/sbbe_vs_cbbe/')
	unlink(paste0(path,'*'))
	dir.create(path)
	
	
	for (j in unique(equity$cat_name)) {
		png(paste0(path, 'sbbe_cbbe_', j, '.png'), res=200, units='in', height=8, width=12)
		
		df = equity[cat_name==j][!is.na(bav_asset)]
		
		par(mfrow=c(1,2))
		
		# RelEstKnow
		for (.var in c('F_RelEstKnow_STD', 'F_EnergDiff_STD')) {
			df[, xvar := get(.var)]
			with(df[year==2011], plot(y=sbbe_STD, x=xvar, main = paste0(j,': ', .var, ' vs. SBBE'),ylab='SBBE', xlab='F_RelEstKnow (year 2011)\ngreen: new brands, red: secondary categories, black: all other brands', col = 'black', pch=20))
			if (nrow(df[year==2011&newbrnd==1])>0) with(df[year==2011&newbrnd==1], points(y=sbbe_STD, x=xvar, col = 'green', pch=20))
			if (nrow(df[year==2011&seccat==1])>0) with(df[year==2011&seccat==1], points(y=sbbe_STD, x=xvar, col = 'red', pch=20))
			with(df[year==2011], text(y=sbbe_STD, x=xvar, labels = brand_name, cex=.6,pos=1))
			
			# regresion line
			mpred <- lm(sbbe_STD~1+xvar, data = df[year==2011])
			newdat = data.table(xvar = seq(from = min(df[year==2011]$xvar,na.rm=T), to=max(df[year==2011]$xvar,na.rm=T), by=.1))
			pred=data.table(predict(mpred, newdat, interval= 'confidence'))
			newdat[, ':=' (sbbe_STD = pred$fit, lwr=pred$lwr, upr=pred$upr)]
			
			with(newdat, lines(xvar, sbbe_STD))
			with(newdat, lines(xvar, lwr,lty=2))
			with(newdat, lines(xvar, upr,lty=2))
			mtext(paste0('R2: ', formatC(summary(mpred)$r.squared,digits=4)), cex=.6)
			
		}
		
		dev.off()
		}
