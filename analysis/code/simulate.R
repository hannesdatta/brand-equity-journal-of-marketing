#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     


# Simulation 

# Step 1: homogenous market share
# Step 2: heterogenous market share
# Step 3: heterogenous market share plus homogenous attributes

# All with correlations, all using SUR or Maximum Likelihood.

# Features:
# (a) Equal observations, and unequal observations (works in SUR)
# (b) Mixture of homogenous and heterogenous coefficients CHECK
# (c) SUR versus Maximum Likelihood CHECK

# To-be-estimated using the base-brand approach. Equivalence with geometric-centering is shown.


# Next development steps:
# - Implement unequal observations in maximum likelihood
# - MCI vs. MNL: check whether that works, too.
# - Implement concentrated likelihood


# Preamble
	require(MASS)
	#require(data.table)
	require(reshape2)

# --> Come up with a standarized way to prepare data and implement it in package.

n_brands = 3
set.seed(1984)

tint = c(-1,-2,0) #c(runif(n_brands-1),0) # true intercepts, of which the first one should be zero always

tbeta = matrix(c(.5, 1, # true betas (note: heterogenous coefficients)
				 0, 1,
				-1,-1), ncol=2, byrow=T)

tattr = c(.5,1,-.5)

tobs = 1000 # number of observations

xsim = matrix(runif(tobs*(length(tbeta)+length(tattr)*n_brands)),ncol=ncol(tbeta)+length(tattr))
pars_heterog = 1:ncol(tbeta)
pars_homog = (ncol(tbeta)+1):ncol(xsim)

# set benchmark brand to last brand in this simulation.
benchm <- n_brands

sigma_tilde <- matrix(c(1,.25, .25, .5), ncol=2)

if (!length(unique(c(n_brands, length(tint), ncol(sigma_tilde)+1, nrow(sigma_tilde)+1)))==1) stop('Check true parameters; some problem with dimensions.')
if (!nrow(tbeta)==n_brands) stop('Heterogenous coefficients not specified properly')

set.seed(1984)

y <- double(tobs*n_brands)

for (p in 1:tobs) { # time periods
	nu = mvrnorm(n=1, mu=rep(0, ncol(sigma_tilde)), Sigma = sigma_tilde)
	
	index = seq(from=p*n_brands-(n_brands-1), length.out=n_brands)
	
	tbetaattr <- cbind(tbeta, matrix(rep(tattr, n_brands),ncol=length(tattr),byrow=T))
	
	xbeta = xsim[index,] ^ rbind(tbetaattr[-benchm,], -tbetaattr[benchm,])
	
	# compute m_it
	m <- double(n_brands)
	m[benchm] <- 1
	m[-benchm] <- exp(tint[-benchm]+nu) * 
				  apply(xbeta[-benchm,], 1, "prod") * 
				  rep(prod(xbeta[benchm,]), n_brands-1)
	
	M=m/sum(m)
	
	y[index] <- M
	}


	if(0){
	# Plot market shares
	require(lattice)
	dat <- data.frame(y, t=rep(1:tobs, each=n_brands), brand=rep(1:n_brands, tobs))
	xyplot(y~t,groups=brand, data=dat, type='l', auto.key=T)
	}
	
	
##########################	
# Recover the parameters #
##########################

# Define transformation matrices (see Fok 2001)

	Hlc = matrix(rep(-1/n_brands, n_brands*n_brands),ncol=n_brands)
	diag(Hlc) <- 1-1/n_brands
	
	Hlc %*% cbind(rep(1,n_brands)) # --> is zero. true.

	Hbb = diag(n_brands-1)
	Hbb = cbind(Hbb, -1)

	# Hlc: used for geometric centering
	# Hbb: used for base-brand approach
	
# Get transformed X and Y matrices
	colnames(xsim)<-paste0('var', 1:ncol(xsim))
	
	datx_het = do.call('rbind', lapply(split(data.frame(log(xsim[,pars_heterog])), rep(1:tobs, each=n_brands)), function(x) {
		matrix(apply(Hbb, 1, function(h) c(h*as.matrix(x))),nrow=nrow(Hbb),byrow=T)
		}))
		
		
	datx_hom = do.call('rbind', lapply(split(data.frame(log(xsim[,pars_homog])), rep(1:tobs, each=n_brands)), function(x) Hbb%*%as.matrix(x)))
		
	daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) Hbb%*%as.matrix(x)))
	
	index = data.frame(var=as.factor(rep(1:(n_brands-1), tobs)), t=rep(1:tobs,each=n_brands-1))
	dummies = model.matrix( ~ var - 1, data=index)

# Transform for estimation
	X = as.matrix(cbind(datx_het, datx_hom, dummies))
	Y = as.matrix(daty)
	index=index

# Put into correct order
	# order data set by brands, then dates
	iorder <- order(index$var,index$t)
		
	X=as.matrix(X[iorder,])
	Y=as.matrix(Y[iorder,])
	index = index[iorder,]
			
# SUR system
	require(marketingtools)
	
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

	m <- itersur(X=X,Y=Y, dates_brands=data.frame(date=index$t,brand=index$var))
	
	m$coefficients$variable <- c(paste0(rep(paste0('var', 1:ncol(xsim[, pars_heterog]), '_'), each=n_brands), rep(paste0('b', 1:n_brands), ncol(xsim[, pars_heterog]))),
								 paste0(rep(paste0('attr', 1:ncol(xsim[, pars_homog]), '_'), each=1)), 
								 paste0('dum', 1:(n_brands-1)))

	ols=solve(t(X)%*%X)%*%t(X)%*%Y
	m$coefficients$true=c(matrix(tbeta, nrow=length(unlist(tbeta)), byrow=T), tattr, tint[-n_brands])
	m$coefficients$ols=ols

	print(m$sigma)
	m$coefficients

	#################################
	# Maximum Likelihood Estimation #
	#################################
	
	T = tobs
	I = n_brands
	
	diagT=diag(T)
	
	# works only with stacked data sets (first all i=1's, then i=2's, etc.)
	ll <- function(par) {
		lchol = diag(2)
		lchol[upper.tri(lchol,diag=T)]<-par[1:3]
		
		sigma_tilde_inv = chol2inv(lchol)
		
		betas <- par[4:(4+ncol(X)-1)]
		
		y_xbeta = Y-X%*%betas
		sigma_tilde_inv_X_identity = sigma_tilde_inv %x% diagT
		mult = t(y_xbeta) %*% sigma_tilde_inv_X_identity %*% y_xbeta
		
		llik = - .5*(T*(I-1)) * log(2*pi) + .5*T * log(det(sigma_tilde_inv)) - .5 * mult
		return(drop(llik))
		}
	
	# real parameters
	alpha0 = c(chol(sigma_tilde)[upper.tri(sigma_tilde,diag=T)], tbeta, tint[-1])
	# start from OLS estimates
	naive=solve(t(X)%*%X)%*%t(X)%*%Y
	
	alpha0 = c(1,0,1, naive)
	ll(alpha0)
	
	out <- optim(par = alpha0, fn = ll, method = "BFGS", hessian = F, control=list(maxit=10000, trace=99, REPORT=10, fnscale=-1, ndeps=rep(1e-3,length(alpha0)),reltol=1e-5))
		
	m$coefficients$maxlik = out$par[4:length(out$par)]
	
	lchol = diag(2)
	lchol[upper.tri(lchol,diag=T)]<-out$par[1:3]
	
	sigma_pred_inv <- chol2inv(lchol)
	
	varcovar=solve(t(X)%*%(sigma_pred_inv %x% diagT)%*%X)
	
	m$coefficients$maxlik_se <- sqrt(diag(varcovar))
	
	sigma = crossprod(lchol)
	sigma
	
	print(m$coefficients)
	

	
####################################
# TRANSFORM TO GEOMETRIC-AVERAGING #
####################################
	
# Get transformed X and Y matrices with Hlc: DOES NOT WORK IN THIS APPROACH YET.
	colnames(xsim)<-paste0('var', 1:ncol(xsim))
	
	datx= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) {
		matrix(apply(Hlc, 1, function(h) c(h*as.matrix(x))),nrow=nrow(Hlc),byrow=T)
		}))
		
	daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
	
	index = data.frame(var=as.factor(rep(1:(n_brands), tobs)), t=rep(1:tobs,each=n_brands))
	dummies = model.matrix( ~ var - 1, data=index)[,-n_brands]

	#dummies= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) {
	#	matrix(apply(Hlc, 1, function(h) c(h*as.matrix(x))),nrow=nrow(Hlc),byrow=T)
	#	}))
	dummies <- matrix(rep(Hlc, tobs), ncol=3, byrow=T)
	dummies <- NULL
	}
		
# Do by equation
	
	summary(lm(daty ~ -1 + dummies + datx, subset = seq(from=1, by=3, length.out=tobs)))
	summary(lm(daty ~ -1 + dummies + datx, subset = seq(from=2, by=3, length.out=tobs)))
	summary(lm(daty ~ -1 + dummies + datx, subset = seq(from=3, by=3, length.out=tobs)))
	
	
	summary(lm(daty ~ -1 + dummies + datx))
	
# This is the same as in the base-brand approach, see also discussion in Fok (2001)


###################
# Transformations #
###################

# see Fok (2001).	
		
# Transform to geometric:
	C = diag(n_brands-1)
	C = cbind(C, -1)
	C = rbind(C, 1)

# check
	C%*%Hlc # is equal to Hbb
	Hbb
	
# do it the other way around
	Cplus = matrix(rep(-1/n_brands, (n_brands-1)^2),ncol=n_brands-1)
	diag(Cplus) <- 1-1/n_brands
	Cplus = rbind(Cplus, -1/n_brands)

	Cplus %*% Hbb # is equal to Hlc. Good.
	Hlc

