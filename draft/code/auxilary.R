
vif_fkt <- function(dt) {
	# Take non-zero columns
	cols <- colnames(dt)[!colSums(dt)==0]
		
	vifs <- lapply(cols, function(col) {
		m<-lm(as.matrix(dt[,col])~-1+as.matrix(dt[, setdiff(cols, col)]))
		return(m)
		#1/(1-summary(m)$r.squared)
		})
	
	return(all = vifs, vifs = unlist(vifs, function(x) 1/(1-summary(x)$r.squared))) 
	}

	