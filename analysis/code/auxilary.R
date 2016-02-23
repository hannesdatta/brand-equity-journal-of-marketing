
vif_fkt <- function(dt) {
	# Take non-zero columns
	cols <- colnames(dt)[!colSums(dt)==0]
		
	res <- lapply(cols, function(col) {
		m<-lm(as.matrix(dt[,col])~-1+as.matrix(dt[, setdiff(cols, col)]))
		names(m$coefficients) <- gsub('as.matrix(dt[, setdiff(cols, col)])', '', names(m$coefficients),fixed=T)
		return(m)
		#1/(1-summary(m)$r.squared)
		})
	
	vifs = unlist(lapply(res, function(x) 1/(1-summary(x)$r.squared)))
	names(vifs) <- cols
	names(res) <- cols
	return(list(all = res, vifs = vifs))
	}

	