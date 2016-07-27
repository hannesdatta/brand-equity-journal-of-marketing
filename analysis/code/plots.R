# Set path for focal model
.dirs = list.dirs('../output',recursive=F)
require(data.table)	

path = .dirs[1]
	

#for (path in .dirs) {
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

	#source('../../draft/code/proc_plots.R')
	
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
	if (0) {
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
	
	
	# SBBE vs CBBE plots for paper
	library(ggrepel)
	library(ggplot2)
	
	plotfkt <- function(iv, sel_cat, xlabel, title = sel_cat, fn = NULL, dv = 'sbbe_YSTD', ylabel = 'SBBE', selyear = 2011, confidence=TRUE, lims_fixed = FALSE, seed = 45, xlim = c(-3,3), ylim = c(-3,3)) {
		#iv = 'bav_asset_STD' 
		#sel_cat = 'beer' 
		#xlabel = 'Brand Asset Score' 
		set.seed(seed)
		df = equity[cat_name%in%sel_cat & year == selyear][!is.na(bav_asset)]
		df[, xvar := get(iv)]
		df[, dv := get(dv)]
		
		
		# compute regression line
			mpred <- lm(dv~1+xvar, data = df[year==selyear])
			predict_dat = data.table(xvar = seq(from = min(df[year==selyear]$xvar,na.rm=T), to=max(df[year==selyear]$xvar,na.rm=T), by=.1))
			pred=data.table(predict(mpred, predict_dat, interval= 'confidence'))
			predict_dat[, ':=' (sbbe_STD_plot = pred$fit, lwr=pred$lwr, upr=pred$upr)]
			
		df_plot <- rbind(df, predict_dat, fill=T)
		
		if (!is.null(fn)) png(fn, res=200, units='in', height=6, width=8)
		pl <- ggplot(df_plot) + geom_point(aes(xvar,dv), color = 'black') + geom_text_repel(aes(xvar, dv, label = brand_name_orig)) +
			         labs(x = xlabel) + labs(y = ylabel) + geom_path(aes(x=xvar, y = sbbe_STD_plot)) + 
					 ggtitle(title) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
					 theme(panel.grid.major = element_blank(), 
						   panel.grid.minor = element_blank(),
						   panel.background	= element_rect(fill=NA),
						   plot.title=element_text(face="bold"),
						   axis.title=element_text(face="bold"),
						   panel.border = element_rect(colour = "black", fill=NA))
					
		if (confidence==T) pl <- pl + geom_path(aes(x=xvar, y = lwr), linetype=2) + geom_path(aes(x=xvar, y = upr), linetype=2)
		if (lims_fixed==T) pl <- pl + ylim(ylim[1], ylim[2]) + xlim(xlim[1], xlim[2]) 
		print(pl)
		if (!is.null(fn)) dev.off()
	}
	
	
	# try plots
	
	#plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'carbbev', xlabel = 'Brand Asset Score', title = 'Carbonated Soft Drinks', fn = NULL)
	
	
	# apply standardization for plots
	loop_vars <- list(bav_asset_YSTD = 'bav_asset',
					  sbbe_YSTD = 'sbbe',
					  bav_rel_YSTD = 'bav_relevance', 
					  bav_est_YSTD = 'bav_esteem',
					  bav_know_YSTD = 'bav_knowledge',
					  bav_energ_YSTD = 'bav_energizeddiff',
					  relstat_YSTD = 'F2_PC1_STD',
					  energdiff_YSTD = 'F2_PC2_STD')
					  
	for (l in seq(along=loop_vars)) equity[!is.na(bav_asset), names(loop_vars)[l] := (get(loop_vars[[l]])-mean(get(loop_vars[[l]]),na.rm=T))/sd(get(loop_vars[[l]]),na.rm=T), by=c('cat_name', 'year'), with = F]
	
	# create directory structure
	for (p in c('sbbe_vs_cbbe', 'figure2', 'figure5', 'marketshare_vs_cbbe', 'sbbe_vs_cbbe_year')) {
		fpath=paste0(path, '/', p, '/')
		unlink(paste0(fpath,'*'))
		dir.create(fpath)
		}

	# execute plotting by category
	for (catn in unique(equity$cat_name)) {	
		for (dv in c('sbbe_YSTD', 'annual_avgms')) {
			
			if (dv=='sbbe_YSTD') {
				ylabel = 'SBBE'
				wpath = paste0(path, '/sbbe_vs_cbbe/')
				}
				
			if (dv=='annual_avgms') {
				ylabel = 'Market share'
				wpath = paste0(path, '/marketshare_vs_cbbe/')
				}
			
			# BAV Asset
			plotfkt(iv = 'bav_asset_YSTD', sel_cat = catn, xlabel = 'Brand Asset Score', title = catn, fn = paste0(wpath, 'brandasset_', catn, '.png'), dv = dv, ylabel = ylabel)
			
			# BAV Pillars
			plotfkt(iv = 'bav_rel_YSTD', sel_cat = catn, xlabel = 'Relevance', title = '', fn = paste0(wpath, 'bavdims_rel_', catn, '.png'), dv = dv, ylabel = ylabel)
			plotfkt(iv = 'bav_est_YSTD', sel_cat = catn, xlabel = 'Esteem', title = '', fn = paste0(wpath, 'bavdims_est_', catn, '.png'), dv = dv, ylabel = ylabel)
			plotfkt(iv = 'bav_know_YSTD', sel_cat = catn, xlabel = 'Knowledge', title = '', fn = paste0(wpath, 'bavdims_kno_', catn, '.png'), dv = dv, ylabel = ylabel)
			plotfkt(iv = 'bav_energ_YSTD', sel_cat = catn, xlabel = 'Energized Differentiation', title = '', fn = paste0(wpath, 'bavdims_energ_', catn, '.png'), dv = dv, ylabel = ylabel)
			
			# BAV Factors
			plotfkt(iv = 'relstat_YSTD', sel_cat = catn, xlabel = 'Relevant Stature (RelStat)', title = '', fn = paste0(wpath, 'bavfactors_relstat_', catn, '.png'), dv = dv, ylabel = ylabel)
			plotfkt(iv = 'energdiff_YSTD', sel_cat = catn, xlabel = 'Energized Differentiation (EnDif)', title = '', fn = paste0(wpath, 'bavfactors_energ_', catn, '.png'), dv = dv, ylabel = ylabel)
		}
	}

	####### FIGURE 2 ################
	# SBBE / Market share VS CBBE   #
	# with proper labeling          #
	#################################

	wpath = paste0(path, '/figure2/')
		
	loop_vars = list(beer = 'Beer',
					 laundet = 'Laundry Detergents')
				
    cntr = 1				
	
	for (l in seq(along=loop_vars)) {
	
		plotfkt(iv = 'bav_asset_YSTD', dv = 'sbbe_YSTD', xlabel = 'Brand Asset Score', ylabel = 'SBBE', 
			sel_cat = names(loop_vars)[l], title = loop_vars[[l]], fn = paste0(wpath, 'figure2', letters[cntr], '.png'), confidence = FALSE, lims_fixed = TRUE, xlim = c(-1.5,3.2), ylim = c(-2.3, 2.3))
	
		plotfkt(iv = 'bav_asset_YSTD', dv = 'annual_avgms', xlabel = 'Brand Asset Score', ylabel = 'Market share', 
				sel_cat = names(loop_vars)[l], title = loop_vars[[l]], fn = paste0(wpath, 'figure2', letters[cntr+1], '.png'), confidence = FALSE, lims_fixed=TRUE, xlim = c(-1.5,3.2), ylim = c(-.01,.3))
		cntr = cntr+2
		}

	##### FIGURE 5 ############
	wpath = paste0(path, '/figure5/')
	
	plotfkt(iv = 'relstat_YSTD', sel_cat = 'beer', xlabel = 'Relevant Stature (RelStat)', title = '', fn = paste0(wpath, 'a_bavfactors_relstat_', catn, '.png'), dv = 'sbbe_YSTD', ylabel = 'SBBE')
	plotfkt(iv = 'energdiff_YSTD', sel_cat = 'beer', xlabel = 'Energized Differentiation (EnDif)', title = '', fn = paste0(wpath, 'b_bavfactors_energ_', catn, '.png'), dv = 'sbbe_YSTD', ylabel = 'SBBE')
	
	
	if(0){
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'carbbev', xlabel = 'Brand Asset Score', title = 'Carbonated Soft Drinks', fn = paste0(fpath, 'figure2b.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'cigets', xlabel = 'Brand Asset Score', title = 'Cigarettes', fn = paste0(fpath, 'figure2c.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'coffee', xlabel = 'Brand Asset Score', title = 'Coffee', fn = paste0(fpath, 'figure2d.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'deod', xlabel = 'Brand Asset Score', title = 'Deodorants', fn = paste0(fpath, 'figure2d.png'))
	
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'coldcer', xlabel = 'Brand Asset Score', title = 'Cold (RTE) Cereals', fn = paste0(fpath, 'figure2e.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'pz_di', xlabel = 'Brand Asset Score', title = 'Frozen Pizza and Dinners', fn = paste0(fpath, 'figure2f.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'laundet', xlabel = 'Brand Asset Score', title = 'Laundry Detergents', fn = paste0(fpath, 'figure2g.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'saltsnck', xlabel = 'Brand Asset Score', title = 'Salty Snacks', fn = paste0(fpath, 'figure2h.png'))
	plotfkt(iv = 'bav_asset_YSTD', sel_cat = 'shamp', xlabel = 'Brand Asset Score', title = 'Shampoo', fn = paste0(fpath, 'figure2h.png'))
	}
		
	####### FIGURE 3 ##################
	# SBBE versus each CBBE dimension #
	###################################
	if (0) {
	plotfkt(iv = 'bav_rel_YSTD', sel_cat = 'beer', xlabel = 'Relevance', title = '', fn = paste0(fpath, 'figure3a.png'))
	plotfkt(iv = 'bav_est_YSTD', sel_cat = 'beer', xlabel = 'Esteem', title = '', fn = paste0(fpath, 'figure3b.png'))
	plotfkt(iv = 'bav_know_YSTD', sel_cat = 'beer', xlabel = 'Knowledge', title = '', fn = paste0(fpath, 'figure3c.png'))
	plotfkt(iv = 'bav_energ_YSTD', sel_cat = 'beer', xlabel = 'Energized Differentiation', title = '', fn = paste0(fpath, 'figure3d.png'))
	}
	
	plotfkt2 <- function(iv, sel_cat, xlabel, title = sel_cat, fn = NULL, dv = 'sbbe_YSTD', ylabel = 'SBBE') {
		#iv = 'bav_asset_STD' 
		#sel_cat = 'beer' 
		#xlabel = 'Brand Asset Score' 
		set.seed(45)
		df = equity[cat_name%in%sel_cat & year == 2011][!is.na(bav_asset)]
		df[, xvar := get(iv)]
		df[, dv := get(dv)]
		
		
		# compute regression line
			mpred <- lm(dv~1+xvar, data = df[year==2011])
			predict_dat = data.table(xvar = seq(from = min(df[year==2011]$xvar,na.rm=T), to=max(df[year==2011]$xvar,na.rm=T), by=.1))
			pred=data.table(predict(mpred, predict_dat, interval= 'confidence'))
			predict_dat[, ':=' (sbbe_STD_plot = pred$fit, lwr=pred$lwr, upr=pred$upr)]
			
		df_plot <- rbind(df, predict_dat, fill=T)
		
		if (!is.null(fn)) png(fn, res=200, units='in', height=20, width=20)
		pl <- ggplot(df_plot) + geom_point(aes(xvar,dv), color = 'black') + geom_text_repel(aes(xvar, dv, label = brand_name_orig)) +
			         labs(x = xlabel) + labs(y = ylabel) + geom_path(aes(x=xvar, y = sbbe_STD_plot)) + 
					 geom_path(aes(x=xvar, y = lwr), linetype=2) + geom_path(aes(x=xvar, y = upr), linetype=2) +
					 ggtitle(title) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
					 theme(panel.grid.major = element_blank(), 
						   panel.grid.minor = element_blank(),
						   plot.title=element_text(face="bold"),
						   axis.title=element_text(face="bold"),
						   panel.border = element_rect(colour = "black", fill=NA))
						   
		print(pl)
		if (!is.null(fn)) dev.off()
	}

	plotfkt2(iv = 'relstat_YSTD', sel_cat = unique(equity$cat_name), xlabel = 'Relevant Stature (RelStat)', title = '', fn = paste0(path, '/all_relevantstature.png'), dv = 'sbbe_YSTD', ylabel = 'SBBE')
	plotfkt2(iv = 'energdiff_YSTD', sel_cat = unique(equity$cat_name), xlabel = 'Energized Differentiation (EnDif)', title = '', fn = paste0(path, '/all_energdiff.png'), dv = 'sbbe_YSTD', ylabel = 'SBBE')

	equity[, relstat_YSTD := (F2_PC1_STD-mean(F2_PC1_STD,na.rm=T))/sd(F2_PC1_STD,na.rm=T), by=c('cat_name')]
	equity[, energdiff_YSTD := (F2_PC2_STD-mean(F2_PC2_STD,na.rm=T))/sd(F2_PC2_STD,na.rm=T), by=c('cat_name')]
	equity[!is.na(bav_asset), sbbe_YSTD := (sbbe-mean(sbbe,na.rm=T))/sd(sbbe,na.rm=T), by=c('cat_name')]

	for (year in 2002:2011) {
		# BAV Factors
			wpath = paste0(path, '/sbbe_vs_cbbe_year/')
			plotfkt(iv = 'relstat_YSTD', sel_cat = 'beer', xlabel = 'Relevant Stature (RelStat)', title = paste0('Beer: ', year), fn = paste0(wpath, 'bavfactors_relstat_', year, '_beer.png'), dv = 'sbbe_YSTD', ylabel = 'SBBE', selyear=year)
			plotfkt(iv = 'energdiff_YSTD', sel_cat = 'beer', xlabel = 'Energized Differentiation (EnDif)', title = paste0('Beer: ', year), fn = paste0(wpath, 'bavfactors_energdiff_', year, '_beer.png'), dv = 'sbbe_YSTD', ylabel = 'SBBE', selyear=year)
		}
		
	
#}