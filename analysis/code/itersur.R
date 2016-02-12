	
	itersur <- function (X, Y, dates_brands, maxiter = 1000) {

		# verify correct data classes
		if (!class(X)=='matrix'|!class(Y)=='matrix') stop('X and Y need to be matrices')
		
		# verify order of dates_brands
		if (!all(order(dates_brands[,2], dates_brands[,1])==1:nrow(X))) stop('Data needs to be stacked by brands')
		

		# initialize starting values for iterative SUR
			beta_ols = solve(t(X) %*% X) %*% t(X) %*% Y
			beta_hat = beta_ols
		
		# iterate through SUR
		for (i in 1:maxiter) {
			beta_old = beta_hat
			
			pred = X %*% beta_hat
			resid = Y - pred
			
			resid_by_brand = dcast(data.frame(dates_brands, resid = matrix(resid)), 
				date ~ brand, value.var = "resid")
				
			resid_y_by_brand = data.frame(dates_brands, y = Y, resid = matrix(resid)) # really needed?
			
			sigma <- matrix(double(length(unique(dates_brands$brand))^2), 
				ncol = length(unique(dates_brands$brand)))
			
			for (.i in 1:ncol(sigma)) {
				for (.j in 1:ncol(sigma)) {
					resids = cbind(resid_by_brand[, .i + 1], resid_by_brand[, 
						.j + 1])
					compl.cases = complete.cases(resids)
					if (length(which(compl.cases == TRUE)) <= 1) {
						sigma[.i, .j] <- 0
					}
					else {
						tmax = nrow(resids)
						resids = resids[complete.cases(resids), ]
						sigma[.i, .j] <- (1/tmax) * sum(resids[, 1] * 
						  resids[, 2])
					}
				}
			}
			
			
			max_t = length(unique(resid_by_brand$date))
			obsperbrand = apply(resid_by_brand[, -1], 2, function(x) length(which(!is.na(x))))
			sigma_inv = solve(sigma)
			#sigma = (1/tobs) * crossprod(as.matrix(resid_by_brand[,-1]))
			
			
			inew = NULL
			
			# calculate the cross product for unequal observations
			for (.i in 1:ncol(sigma_inv)) {
				jnew = NULL
				for (.j in 1:ncol(sigma_inv)) {
					zeros = matrix(double(obsperbrand[.i] * obsperbrand[.j]), 
						nrow = obsperbrand[.i], ncol = obsperbrand[.j])
					diag(zeros) <- sigma_inv[.i, .j]
					jnew = cbind(jnew, zeros)
				}
				inew = rbind(inew, jnew)
			}
			
			omega_inverse = inew
			varcovar = solve(t(X) %*% omega_inverse %*% X)
			beta_hat = varcovar %*% (t(X) %*% omega_inverse %*% Y)
			
			# check convergence, based on criterium in Greene (2002), p. 566
			delta = drop(t(beta_hat - beta_old) %*% (t(X)%*%omega_inverse%*%X) %*% (beta_hat - beta_old)) # the middle part belongs to the Hessian
			if (delta<10^-7) break
		}
		
	res = NULL
	varcovar = solve(t(X)%*%omega_inverse%*%X)
	ses = sqrt(diag(varcovar))

	res$coefficients = data.frame(variable = colnames(X), coef = drop(beta_hat), se = ses)
    k = ncol(varcovar)
    N = length(resid)
    res$bic = log(sum(resid^2)/N) + (k * log(N))/N
    res$predicted = X %*% beta_hat
    res$resid = Y - X %*% beta_hat
    res$X <- X
    res$Y <- Y
    res$dates_brands <- dates_brands
	res$sigma <- sigma
	res$iteration = i 
    return(res)
	}
