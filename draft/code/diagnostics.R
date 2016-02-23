
vifs <- rbindlist(lapply(1:length(results), function(iter) {
	x <- results[[iter]]$model
	tmp <- split(data.frame(as.matrix(x$X)), x$dates_brands$brand)
	
	vifs<-lapply(tmp, function(dt) {
	})

	vifs<-unlist(vifs)[!is.na(unlist(vifs))]
	
	vifs<-data.table(variable=names(vifs),vif=vifs)
	setorder(vifs, variable)
	
	vifs[, equation := sapply(variable, function(x) strsplit(x, '.',fixed=T)[[1]][1])]
	vifs[, coefficient := sapply(variable, function(x) strsplit(x, '.',fixed=T)[[1]][2])]
	vifs[, variable:=NULL]
	setcolorder(vifs, c('equation','coefficient','vif'))
	vifs[, cat_name := results[[iter]]$cat_name]
	
	}))
	

tmp = vifs[order(vifs$vif, decreasing=T)]
options(width=1000)
print(data.frame(tmp))
