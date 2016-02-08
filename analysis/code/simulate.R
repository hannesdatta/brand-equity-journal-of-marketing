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


# --> Come up with a standarized way to prepare data and implement it in package.

n_brands = 10
set.seed(1984)

tint = c(0, runif(n_brands-1)) # true intercepts, of which the first one should be zero always

tbeta = c(.5, -.5, 1, 2) # true beta

tobs = 100 # number of observations
L = 100 # number of simulation iterations

xsim = matrix(runif(tobs*length(tbeta)*n_brands),ncol=length(tbeta))


attrspec = c('mci') # chose the attraction specification, e.g. mci, mnl... I try one with restricted effects right now.

# benchmark brand is always brand 1 in this simulation.

sigma_tilde = matrix(diag(rep(.5, n_brands-1)), ncol=n_brands-1)

if (!length(unique(c(n_brands, length(tint), ncol(sigma_tilde)+1, nrow(sigma_tilde)+1)))==1) stop('Check true parameters; some problem with dimensions.')

set.seed(1984)

benchm <- 10

y <- NULL

for (p in 1:tobs) { # time periods
	nu = mvrnorm(n=L, mu=rep(0, ncol(sigma_tilde)), Sigma = sigma_tilde)
	
	index = seq(from=p*n_brands-(n_brands-1), length.out=n_brands)
	
	# normalize the Xs
	xnorm = xsim[index[-benchm],]/matrix(rep(xsim[index[benchm],], n_brands-1),byrow=T, nrow=n_brands-1)
	xbeta = xnorm ^ matrix(rep(tbeta,n_brands-1),ncol=length(tbeta),byrow=T)

	xbeta2 = (log(xnorm) * matrix(rep(tbeta,n_brands-1),ncol=length(tbeta),byrow=T))
	exp(rowSums(xbeta2))
	
	#
	#apply(exp(xbeta2), 1, 'prod')
	
	
	# compute m_it
	m <- matrix(double(n_brands*L), nrow=L)
	m[,benchm] <- 1
	m[,-benchm] <- exp(matrix(rep(tint[-1],L),byrow=T,nrow=L)+nu) * matrix(rep(apply(xbeta, 1, "prod"), L),byrow=T, nrow=L)
	
	M=apply(m, 1, function(x) x/sum(x))
	
	# average market shares
	Mbar = rowMeans(M)
	y[index] <- rowMeans(M)
	
	# this model does not have any lagged variables
	}



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


Hlc = matrix(rep(-1/n_brands, n_brands*n_brands),ncol=n_brands)
diag(Hlc) <- 1-1/n_brands

Hlc %*% cbind(rep(1,n_brands)) # --> is zero. true.

Hbb = diag(n_brands-1)
Hbb = cbind(Hbb, -1)



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

