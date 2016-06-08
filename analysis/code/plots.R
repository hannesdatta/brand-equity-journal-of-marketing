# Set path for focal model
.dirs = list.dirs('../output',recursive=F)
require(data.table)	
		
for (path in .dirs) {
	equity <- fread(paste0(path, '/equity.csv'))
	elast <- fread(paste0(path, '/elasticities.csv'))

	############
	# PLOTTING #
	############

	##########################
	# MARKETING ELASTICITIES #
	##########################
	elast[!is.na(bav_asset)]
	
	## HISTOGRAM

	fpath=paste0(path, '/elast_hist/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)
		
	for (j in unique(elast$cat_name)) {
		png(paste0(fpath, 'elasticities_', j, '.png'), res=200, units='in', height=8, width=12)
		par(mfrow=c(2,3))
		for (i in unique(elast[cat_name==j]$var_name)) {
			hist(elast[cat_name==j&var_name==i]$elast,main=paste0(j, ': ', i),xlab = 'elasticity')
			}
		dev.off()

		}

	## BARPLOT
	fpath=paste0(path, '/elast_bar/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)
		
	error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
		if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
		stop("vectors must be same length")
		print(arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...))
		}


	for (j in unique(elast$cat_name)) {
		png(paste0(fpath, 'elasticities_', j, '.png'), res=200, units='in', height=8, width=12)
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
	fpath=paste0(path, '/elast_vs_cbbe/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)

	require(lattice)
	
		for (i in unique(elast$var_name)) {
			
			png(paste0(fpath, 'elast_cbbe_', i, '_2factors.png'), res=200, units='in', height=16, width=16)
			df = elast[var_name==i]
			print(xyplot(elast~F2_PC1+F2_PC2|cat_name,data=df,auto.key=T, main = paste0('Elasticities vs. CBBE: ', i),scales = list(y = list(relation = "free"))))
			dev.off()
			
			png(paste0(fpath, 'elast_cbbe_', i, '_3factors.png'), res=200, units='in', height=16, width=16)
			df = elast[var_name==i]
			print(xyplot(elast~F3_PC1+F3_PC2+F3_PC3|cat_name,data=df,auto.key=T, main = paste0('Elasticities vs. CBBE: ', i),scales = list(y = list(relation = "free"))))
			dev.off()
			
			}

	########
	# SBBE #
	########
	
	## LONGITUDINAL OBSERVATIONS BY CATEGORY
	
	require(lattice)
	fpath=paste0(path, '/sbbe/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)

	source('../../draft/code/proc_plots.R')
	
	for (j in unique(equity$cat_name)) {
		png(paste0(fpath, 'sbbe_', j, '.png'), res=200, units='in', height=8, width=12)
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

	
	# SCATTERPLOT SBBE vs. CBBE
	fpath=paste0(path, '/sbbe_vs_cbbe/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)

	
	for (j in unique(equity$cat_name)) {
		
		for (nfactors in c(2,3)) {
			if (nfactors==2) fac = c('F2_PC1_STD', 'F2_PC2_STD')
			if (nfactors==3) fac = c('F3_PC1_STD', 'F3_PC2_STD','F3_PC3_STD')
			
			png(paste0(fpath, 'sbbe_cbbe_', j, '_', nfactors, '_factors.png'), res=200, units='in', height=8, width=12)
			df = equity[cat_name==j][!is.na(bav_asset)]
			
			par(mfrow=c(1,nfactors))
			
			for (.var in fac) {
				df[, xvar := get(.var)]
				with(df[year==2011], plot(y=sbbe_STD, x=xvar, main = paste0(j,': ', .var, ' vs. SBBE'),ylab='SBBE', xlab='Factor score (year 2011)\ngreen: new brands, red: secondary categories, black: all other brands', col = 'black', pch=20))
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
		
	}
	
	
	
	
	# SBBE vs CBBE plots for paper
	fpath=paste0(path, '/sbbe_vs_cbbe_paper/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)

	library(ggrepel)
	library(ggplot2)
	
	plotfkt <- function(iv, sel_cat, xlabel, title = sel_cat, fn = NULL) {
		#iv = 'bav_asset_STD' 
		#sel_cat = 'beer' 
		#xlabel = 'Brand Asset Score' 
		set.seed(45)
		df = equity[cat_name==sel_cat & year == 2011][!is.na(bav_asset)]
		df[, xvar := get(iv)]
		
		# compute regression line
			mpred <- lm(sbbe_YSTD~1+xvar, data = df[year==2011])
			predict_dat = data.table(xvar = seq(from = min(df[year==2011]$xvar,na.rm=T), to=max(df[year==2011]$xvar,na.rm=T), by=.1))
			pred=data.table(predict(mpred, predict_dat, interval= 'confidence'))
			predict_dat[, ':=' (sbbe_STD_plot = pred$fit, lwr=pred$lwr, upr=pred$upr)]
			
		df_plot <- rbind(df, predict_dat, fill=T)
		
		if (!is.null(fn)) png(fn, res=200, units='in', height=6, width=8)
		pl <- ggplot(df_plot) + geom_point(aes(xvar,sbbe_YSTD), color = 'black') + geom_text_repel(aes(xvar, sbbe_YSTD, label = brand_name_orig)) +
			         labs(x = xlabel) + labs(y = 'SBBE') + geom_path(aes(x=xvar, y = sbbe_STD_plot)) + geom_path(aes(x=xvar, y = lwr), linetype=2) + geom_path(aes(x=xvar, y = upr), linetype=2) +
					 ggtitle(title) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
					 theme(panel.grid.major = element_blank(), 
						   panel.grid.minor = element_blank(),
						   plot.title=element_text(face="bold"),
						   axis.title=element_text(face="bold"),
						   panel.border = element_rect(colour = "black", fill=NA))
						   
		print(pl)
		if (!is.null(fn)) dev.off()
	}
	
	
	
	####### FIGURE 2 ########
	equity[!is.na(bav_asset), bav_asset_YSTD := (bav_asset-mean(bav_asset,na.rm=T))/sd(bav_asset,na.rm=T), by=c('cat_name', 'year')]
	equity[!is.na(bav_asset), sbbe_YSTD := (sbbe-mean(sbbe,na.rm=T))/sd(sbbe,na.rm=T), by=c('cat_name', 'year')]
	
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'beer', xlabel = 'Brand Asset Score', title = 'Beer', fn = paste0(fpath, 'figure2a.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'carbbev', xlabel = 'Brand Asset Score', title = 'Carbonated Soft Drinks', fn = paste0(fpath, 'figure2b.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'cigets', xlabel = 'Brand Asset Score', title = 'Cigarettes', fn = paste0(fpath, 'figure2c.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'coffee', xlabel = 'Brand Asset Score', title = 'Coffee', fn = paste0(fpath, 'figure2d.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'deod', xlabel = 'Brand Asset Score', title = 'Deodorants', fn = paste0(fpath, 'figure2d.png'))
	
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'coldcer', xlabel = 'Brand Asset Score', title = 'Cold (RTE) Cereals', fn = paste0(fpath, 'figure2e.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'pz_di', xlabel = 'Brand Asset Score', title = 'Frozen Pizza and Dinners', fn = paste0(fpath, 'figure2f.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'laundet', xlabel = 'Brand Asset Score', title = 'Laundry Detergents', fn = paste0(fpath, 'figure2g.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'saltsnck', xlabel = 'Brand Asset Score', title = 'Salty Snacks', fn = paste0(fpath, 'figure2h.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'shamp', xlabel = 'Brand Asset Score', title = 'Shampoo', fn = paste0(fpath, 'figure2h.png'))

	# do this for all categories
	for (catn in unique(equity$cat_name)) plotfkt(iv = 'bav_asset_YSTD', sel_cat = catn, xlabel = 'Brand Asset Score', title = catn, fn = paste0(fpath, 'brandasset_', catn, '.png'))
		
	####### FIGURE 3 ########
	equity[, bav_rel_YSTD := (bav_relevance-mean(bav_relevance,na.rm=T))/sd(bav_relevance,na.rm=T), by=c('cat_name', 'year')]
	equity[, bav_est_YSTD := (bav_esteem-mean(bav_esteem,na.rm=T))/sd(bav_esteem,na.rm=T), by=c('cat_name', 'year')]
	equity[, bav_know_YSTD := (bav_knowledge-mean(bav_knowledge,na.rm=T))/sd(bav_knowledge,na.rm=T), by=c('cat_name', 'year')]
	equity[, bav_energ_YSTD := (bav_energizeddiff-mean(bav_energizeddiff,na.rm=T))/sd(bav_energizeddiff,na.rm=T), by=c('cat_name', 'year')]
	
	plotfkt(iv = 'bav_rel_YSTD', sel_cat = 'beer', xlabel = 'Relevance', title = '', fn = paste0(fpath, 'figure3a.png'))
	plotfkt(iv = 'bav_est_YSTD', sel_cat = 'beer', xlabel = 'Esteem', title = ' Soft Drinks', fn = paste0(fpath, 'figure3b.png'))
	plotfkt(iv = 'bav_know_YSTD', sel_cat = 'beer', xlabel = 'Knowledge', title = '', fn = paste0(fpath, 'figure3c.png'))
	plotfkt(iv = 'bav_energ_YSTD', sel_cat = 'beer', xlabel = 'Energized Differentiation', title = '', fn = paste0(fpath, 'figure3d.png'))
	
	# do this for all categories
	for (catn in unique(equity$cat_name)) {
		plotfkt(iv = 'bav_rel_YSTD', sel_cat = catn, xlabel = 'Relevance', title = '', fn = paste0(fpath, 'bavdims_rel_', catn, '.png'))
		plotfkt(iv = 'bav_est_YSTD', sel_cat = catn, xlabel = 'Esteem', title = ' Soft Drinks', fn = paste0(fpath, 'bavdims_est_', catn, '.png'))
		plotfkt(iv = 'bav_know_YSTD', sel_cat = catn, xlabel = 'Knowledge', title = '', fn = paste0(fpath, 'bavdims_kno_', catn, '.png'))
		plotfkt(iv = 'bav_energ_YSTD', sel_cat = catn, xlabel = 'Energized Differentiation', title = '', fn = paste0(fpath, 'bavdims_energ_', catn, '.png'))
		}
	

	# SCATTERPLOT SBBE vs. CBBE
	fpath=paste0(path, '/sbbe_vs_cbbe/')
	unlink(paste0(fpath,'*'))
	dir.create(fpath)

	
	for (j in unique(equity$cat_name)) {
		
		for (nfactors in c(2,3)) {
			if (nfactors==2) fac = c('F2_PC1_STD', 'F2_PC2_STD')
			if (nfactors==3) fac = c('F3_PC1_STD', 'F3_PC2_STD','F3_PC3_STD')
			
			png(paste0(fpath, 'sbbe_cbbe_', j, '_', nfactors, '_factors.png'), res=200, units='in', height=8, width=12)
			df = equity[cat_name==j][!is.na(bav_asset)]
			
			par(mfrow=c(1,nfactors))
			
			for (.var in fac) {
				df[, xvar := get(.var)]
				with(df[year==2011], plot(y=sbbe_STD, x=xvar, main = paste0(j,': ', .var, ' vs. SBBE'),ylab='SBBE', xlab='Factor score (year 2011)\ngreen: new brands, red: secondary categories, black: all other brands', col = 'black', pch=20))
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
		
	}
	
		
}