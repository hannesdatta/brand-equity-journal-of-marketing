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
# Step 2: homegenous market share plus attributes
# Step 3: heterogenous market share

# All with correlations, all using SUR.

# Features:
# (a) Equal observations, and unequal observations
# (b) Mixture of homogenous and heterogenous coefficients
# (c) SUR versus Maximum Likelihood

# To-be-estimated using the base-brand approach, but TRY to convert it to the geometric one, too.

# Preamble
	require(MASS)
	require(data.table)
	require(reshape2)

# --> Come up with a standarized way to prepare data and implement it in package.

n_brands = 3
set.seed(1984)

tint = c(-1,-2,0)#c(runif(n_brands-1),0) # true intercepts, of which the first one should be zero always

tbeta = matrix(c(.5, 1, # true betas (note: heterogenous coefficients)
				 0, 1,
				0,0 ), ncol=2)

				 			 
tobs = 1000 # number of observations
L = 100 # number of simulation iterations

xsim = matrix(runif(tobs*length(tbeta)),ncol=ncol(tbeta))


attrspec = c('mci') # chose the attraction specification, e.g. mci, mnl... I try one with restricted effects right now.

# benchmark brand is always the last brand in this simulation.
benchm <- n_brands

#sigma_tilde = matrix(diag(rep(0.5, n_brands-1)), ncol=n_brands-1)
sigma_tilde <- matrix(c(.5,0, 0, .5), ncol=2)

if (!length(unique(c(n_brands, length(tint), ncol(sigma_tilde)+1, nrow(sigma_tilde)+1)))==1) stop('Check true parameters; some problem with dimensions.')
if (!nrow(tbeta)==n_brands) stop('Heterogenous coefficients not specified properly')

set.seed(1984)

y <- double(tobs*n_brands)

for (p in 1:tobs) { # time periods
	nu = mvrnorm(n=L, mu=rep(0, ncol(sigma_tilde)), Sigma = sigma_tilde)
	
	index = seq(from=p*n_brands-(n_brands-1), length.out=n_brands)
	
	# normalize the Xs (works only for a homogenous model)
	# xnorm = xsim[index[-benchm],]/matrix(rep(xsim[index[benchm],], n_brands-1),byrow=T, nrow=n_brands-1)
	#xbeta = xnorm ^ matrix(rep(tbeta,n_brands-1),ncol=length(tbeta),byrow=T)
	
	#m[,-benchm] <- exp(matrix(rep(tint[-benchm],L),byrow=T,nrow=L)+nu) * 
	#				   matrix(rep(apply(xbeta[-benchm,], 1, "prod"), L),byrow=T, nrow=L) * 
	
	xbeta = xsim[index,] ^ rbind(tbeta[-benchm,],  -tbeta[benchm,])
	
	# compute m_it
	m <- matrix(double(n_brands*L), nrow=L)
	m[,benchm] <- 1
	m[,-benchm] <- exp(matrix(rep(tint[-benchm],L),byrow=T,nrow=L)+nu) * 
					   matrix(rep(apply(xbeta[-benchm,], 1, "prod"), L),byrow=T, nrow=L) * 
					   matrix(rep(prod(xbeta[benchm,]), L*(n_brands-1)),byrow=T, nrow=L)
	
	M=apply(m, 1, function(x) x/sum(x))
	
	# average market shares
	Mbar = rowMeans(M)
	y[index] <- rowMeans(M)
	
	# this model does not have any lagged variables
	}

	# Let's try to recover it from here (manually).
	
	dt <- data.table(y=y, xsim, brand=paste0('b',rep(1:n_brands, tobs)), t=rep(1:tobs, each=n_brands))
	dtmelt <- melt.data.table(dt, id.vars=c('brand', 't'))
	dtcast <- dcast.data.table(dtmelt, t ~ brand+variable)
	dtcast[, dv := log(b1_y)-log(b3_y)]
	summary(lm(dv ~ 1 + log(b1_var1) + log(b1_var2) , data=dtcast))
	
	
	+ log(b3_var1) + log(b3_var2)
	
#newindex = seq(from = 1, to = 
# Transform for estimation
##	X = cbind(datx, dummies)#[newindex,]
#	Y = daty#[newindex]
	#index=index#[newindex,]
#	dates_brands=data.frame(date=index$t,brand=index$var)
	
	
require(lattice)
dat <- data.frame(y, t=rep(1:tobs, each=n_brands), brand=rep(1:n_brands, tobs))
xyplot(y~t,groups=brand, data=dat, type='l', auto.key=T)

# Data needs to be reshuffled


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
	
	datx= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) {
		matrix(apply(Hbb, 1, function(h) c(h*as.matrix(x))),nrow=nrow(Hbb),byrow=T)
		}))
		
	daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) Hbb%*%as.matrix(x)))
	
	index = data.frame(var=as.factor(rep(1:(n_brands-1), tobs)), t=rep(1:tobs,each=n_brands-1))
	dummies = model.matrix( ~ var - 1, data=index)


# Get transformed X and Y matrices with Hlc
	if(0){
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
	dummies <- matrix(rep(Hlc, tobs), ncol=3, byrow=T)[,-3]
	dummies <- NULL
	}
	
# Reshuffle
	newindex = c(seq(from=1, by=2, length.out=tobs),seq(from=2, by=2, length.out=tobs))

# Transform for estimation
	X = cbind(datx, dummies)#[newindex,]
	Y = daty#[newindex]
	#index=index#[newindex,]
	dates_brands=data.frame(date=index$t,brand=index$var)
	
	
	x1=X[seq(from=1,by=2,length.out=1000),]
	y1=Y[seq(from=1,by=2,length.out=1000)]
	summary(lm(y1~-1+x1))

	
	
# Need for SUR system
	require(marketingtools)
	m <- itersur(X=X,Y=Y, dates_brands=data.frame(date=index$t,brand=index$var))
	
	betas <- NULL
	
#	itersur <- function (X, Y, dates_brands) {
  
	beta_ols = solve(t(X) %*% X) %*% t(X) %*% Y
	beta_hat = beta_ols
	print(beta_hat)
	
	for (i in 1:10) {
	pred = X %*% beta_hat
    resid = Y - pred
    
	resid_by_brand = dcast(data.frame(dates_brands, resid = matrix(resid)), 
        date ~ brand, value.var = "resid")
    resid_y_by_brand = data.table(data.frame(dates_brands, y = Y, 
        resid = matrix(resid)))
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
	
	print(beta_hat)
	print(sigma)
	
	betas[[i]] <- beta_hat
	}
	
	#print(
	#converged?#
	
	# Correlation properly estimated? No!
	
	
	
	
    data.frame(matrix(betas), matrix(beta_hat))
    res = NULL
    res$coefficients = data.table(variable = colnames(X), coef = drop(beta_hat), 
        se = sqrt(diag(varcovar)))
    k = ncol(varcovar)
    N = length(resid)
    res$bic = log(sum(resid^2)/N) + (k * log(N))/N
    res$predicted = X %*% beta_hat
    res$resid = Y - X %*% beta_hat
    res$X <- X
    res$Y <- Y
    res$dates_brands <- dates_brands
    return(res)
}

	
	
	colnames(X) <- c(paste0(rep(paste0('var', 1:ncol(xsim), '_'), each=n_brands), rep(paste0('b', 1:n_brands), ncol(xsim))), paste0('dum', 1:(n_brands-1)))
	
	
	m$coefficients[, variable:= c(paste0(rep(paste0('var', 1:ncol(xsim), '_'), each=n_brands), rep(paste0('b', 1:n_brands), ncol(xsim))), paste0('dum', 1:(n_brands-1)))]
	
	ols=solve(t(X)%*%X)%*%t(X)%*%Y
	m$coefficients[, true:=c(matrix(tbeta, nrow=length(unlist(tbeta)), byrow=T), tint[-n_brands])]
	m$coefficients[, ols:=ols]

	# there's something strange in recovering the dummy variables; am I missing something?
	m$coefficients
	
	# variance of residuals
	sd(m$resid)
	
	cov(matrix(m$resid, ncol=2, byrow=T))
	
	# the covariance is not properly addressed here.
	
	# can I try maxlik?
	
	diagT=diag(T)
	ll <- function(par) {
		lchol = diag(2)
		lchol[upper.tri(lchol,diag=T)]<-par[1:3]
		
		#sigma_tilde = crossprod(lchol)
		sigma_tilde_inv = chol2inv(lchol) #solve(sigma_tilde)
		
		betas <- par[4:(4+ncol(X)-1)]
		T = tobs
		I = n_brands
		
		y_xbeta = Y-X%*%betas
		sigma_tilde_inv_X_identity = sigma_tilde_inv %x% diagT
		mult = t(y_xbeta) %*% sigma_tilde_inv_X_identity %*% y_xbeta
		
		llik = - .5*(T*(I-1)) * log(2*pi) + .5*T * log(det(sigma_tilde_inv)) - .5 * mult
		return(drop(llik))
		}
	
	alpha0 = c(.5,0,.5, m$coefficients$true)
	#alpha0 = rep(1, length(alpha0))
	ll(alpha0)
	
	out1 <- optim(par = alpha0, fn = ll, method = "BFGS", hessian = F, control=list(maxit=10000, trace=99, REPORT=10, fnscale=-1, ndeps=rep(1e-3,length(alpha0)),reltol=1e-5))
	out <- optim(par = out1$par, fn = ll, method = "BFGS", hessian = F, control=list(maxit=10000, trace=99, REPORT=10, fnscale=-1, ndeps=rep(1e-5,length(alpha0)),reltol=1e-7))
	
	
	m$coefficients[, maxlik := out$par[4:length(out$par)]]
	
	lchol = diag(2)
	lchol[upper.tri(lchol,diag=T)]<-out$par[1:3]
	
	sigma_pred_inv <- chol2inv(lchol)
	
	ses=solve(t(X)%*%(sigma_pred_inv %x% diagT)%*%X)
	sqrt(diag(ses))
	
	
	
	sigma = crossprod(lchol)
	sigma
	
	# get standard errors
	
	
	
	
	# It's difficult to bring together Hbb in a differential effects world. The standard errors are really small.
	require(systemfit)
	
	head(xsim)
	tmp = data.frame(log(xsim), y=log(y), var=as.factor(paste0('b',rep(1:(n_brands), tobs))), t=rep(1:tobs,each=n_brands))
	tmp=melt(tmp, id.vars=c('var', 't'))
	dtmp <- dcast(tmp, t~var+variable)
	
	m2<-systemfit( list(eq1 = I(b1_y - b3_y) ~ b1_var1+b1_var2+b1_var3+b1_var4 + b3_var1 + b3_var2 + b3_var3 + b3_var4,
					eq2 = I(b2_y - b3_y)  ~ b2_var1+b2_var2+b2_var3+b2_var4 + b3_var1 + b3_var2 + b3_var3 + b3_var4),
					data=dtmp, restrict.matrix  = c( "eq1_b3_var1 - eq2_b3_var1 - 0",
					"eq1_b3_var2 - eq2_b3_var2 - 0",
					"eq1_b3_var3 - eq2_b3_var3 - 0",
					"eq1_b3_var4 - eq2_b3_var4 - 0"), method = 'SUR')

	summary(m2)
	m$coefficients
	
	# the covariance matrix slightly diverges
	#sqrt(diag(m2$coefCov))

	
	
out <- seq(from=n_brands, to = nrow(datx), by=n_brands)
datx <- datx[-out,]
daty <- daty[-out]
datdummies <- datdummies[-out,]

# Ok... let's try to estmiate the geometric centred version

datx= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
index = data.frame(var=as.factor(rep(1:n_brands, tobs)))
dummies = model.matrix( ~ var - 1, data=index)
datdummies = do.call('rbind', lapply(split(data.frame(dummies), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
datdummies <- datdummies[,-benchm]

# die transformation muss so vorgenommen werden dass 10 abgezogen wird von 1-9.



# take out every 10th equation

gmodel <- lm(daty ~ -1 + datdummies + datx)
summary(gmodel)
summary(model)
	
	
	
	
# Recover the parameters
tmpx <- do.call('rbind', lapply(split(data.frame(xsim), rep(1:tobs, each=n_brands)), function(x) {x<-as.matrix(x);x[-benchm,]/matrix(rep(x[benchm,],n_brands-1),byrow=T,nrow=n_brands-1)}))
tmpx <- log(tmpx)

tmpy <- unlist(lapply(split(data.frame(y), rep(1:tobs, each=n_brands)), function(x) {x<-as.matrix(x);x[-benchm]/x[benchm]}))
tmpy=log(tmpy)

indicbrands = 1:n_brands
indicbrands <- indicbrands[-benchm]

indicators <- rep(indicbrands,tobs)

# regression model
model <- lm(tmpy~-1+as.factor(indicators)+tmpx)
summary(model)



# Transform to geometric:
C = diag(n_brands-1)
C = cbind(C, -1)
C = rbind(C, 1)

# check
C%*%Hlc # is equal to Hbb


Cplus = matrix(rep(-1/n_brands, (n_brands-1)^2),ncol=n_brands-1)
diag(Cplus) <- 1-1/n_brands
Cplus = rbind(Cplus, -1/n_brands)

Cplus %*% Hbb # is equal to Hlc. Good.


mcoef <- model$coefficients[n_brands:length(model$coefficients)]
mcoef <- matrix(rep(mcoef, n_brands-1), byrow=T, nrow=n_brands-1)

Cplus %*% mcoef




# Ok... let's try to estmiate the geometric centred version
datx= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) C%*%Hlc%*%as.matrix(x)))
daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) C%*%Hlc%*%as.matrix(x)))
index = data.frame(var=as.factor(rep(1:n_brands, tobs)))
dummies = model.matrix( ~ var - 1, data=index)
datdummies = do.call('rbind', lapply(split(data.frame(dummies), rep(1:tobs, each=n_brands)), function(x) C%*%Hlc%*%as.matrix(x)))
datdummies <- datdummies[,-benchm]

out <- seq(from=n_brands, to = nrow(datx), by=n_brands)
datx <- datx[-out,]
daty <- daty[-out]
datdummies <- datdummies[-out,]

# Ok... let's try to estmiate the geometric centred version

datx= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
index = data.frame(var=as.factor(rep(1:n_brands, tobs)))
dummies = model.matrix( ~ var - 1, data=index)
datdummies = do.call('rbind', lapply(split(data.frame(dummies), rep(1:tobs, each=n_brands)), function(x) Hlc%*%as.matrix(x)))
datdummies <- datdummies[,-benchm]

# die transformation muss so vorgenommen werden dass 10 abgezogen wird von 1-9.



# take out every 10th equation

gmodel <- lm(daty ~ -1 + datdummies + datx)
summary(gmodel)
summary(model)



# FROM BASE BRAND TO GEOMETRIC MEAN CENTERING

	# Ok... let's try to estmiate the geometric centred version
	datx= do.call('rbind', lapply(split(data.frame(log(xsim)), rep(1:tobs, each=n_brands)), function(x) Hbb%*%as.matrix(x)))
	daty= unlist(lapply(split(data.frame(log(y)), rep(1:tobs, each=n_brands)), function(x) Hbb%*%as.matrix(x)))
	index = data.frame(var=as.factor(rep(1:n_brands, tobs)))
	dummies = model.matrix( ~ var - 1, data=index)
	datdummies = do.call('rbind', lapply(split(data.frame(dummies), rep(1:tobs, each=n_brands)), function(x) Hbb%*%as.matrix(x)))
	datdummies <- datdummies[,-benchm]

	
	bmodel <- lm(daty ~ -1 + datdummies + datx)
	summary(bmodel)

	Cplus * bmodel$coefficients[11]
	


# sweet as!

mcoef2 <- gmodel$coefficients[(n_brands):length(gmodel$coefficients)]


Cplus %*% diag(rep(mcoef2[1],9))



 # -> geometric to base brand
diag(C*mcoef2[2]) # -> geometric to base brand


# transform to geometric brand



dat[, brand := rep(1:n_brands, tobs)]
dat[, t := rep(1:tobs, each=n_brands)]



### Extend model to be heterogenous


# geometric centering



# normalize to last brand, for simplicity


#  yes: got it :).
# build it up from here.

# @Harald: how to retrieve SEs?


# I have to go from general to specific.

# I have to build a simulation function.

# Transform to geometric approach and verify



# Transform to geometrically averaged parameters


# Then: introduce lagged DVs (?)


	
# I need a data class with BRAND AND TIME INDECES, and with an indicator which normalization has been applied









# APPROACH 1: Start off with one-equation OLS/Maxlik

t = 1000
b = c(5,2,3)
set.seed(1984)
intercepts = c(1)
X = cbind(matrix(runif((length(b))*t), ncol=length(b)))
sd = 2
err <- rnorm(t, 0, sd)

y1 = intercepts + X%*%b + err

y2 = apply(X, 1, function(x) prod(x^b)) * exp(err) * exp(intercepts)

summary(lm(y1~1+X))

summary(lm(log(y2)~1+log(X)))

# -> parameters are recovered.

# APPROACH 2: Normalize simulated coefs by 1

y3 = y2/(1+y2)
yinv = 1/(1+y2)

summary(lm(I(log(y3)-1*log(yinv))~1+log(X))) # gets off

# i get it out *exactly*.



# completely recover the 


# Then, extend with two equations. Recover variance-covariance matrix.
# Then, turn it into a SUR


