#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     


# Simulation 

# Preamble
	require(MASS)
	#require(data.table)
	require(reshape2)
	require(marketingtools)
	
# --> Come up with a standarized way to prepare data and implement it in package.

n_brands = 3
set.seed(1984)

tint = c(-1,-2,0) #c(runif(n_brands-1),0) # true intercepts, of which the first one should be zero always

tbeta = matrix(c(.5, 1, # true betas (note: heterogenous coefficients)
				 .5, 1,
				 .5, 1), ncol=2, byrow=T)

tattr = c(.5,1,0)

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
	m[benchm] <- exp(tint[benchm])
	m[-benchm] <- exp(tint[-benchm]+nu) * 
				  apply(xbeta[-benchm,], 1, "prod") * 
				  rep(prod(xbeta[benchm,]), n_brands-1)
	
	M=m/sum(m)
	
	y[index] <- M
	}

#index = data.frame(var=as.factor(rep(1:(n_brands-1), tobs)), t=rep(1:tobs,each=n_brands-1))
	
	if(0){
	# Plot market shares
	require(lattice)
	dat <- data.frame(y, t=rep(1:tobs, each=n_brands), brand=rep(1:n_brands, tobs))
	xyplot(y~t,groups=brand, data=dat, type='l', auto.key=T)
	}
	
	
##########################	
# Recover the parameters #
##########################
index = data.frame(var=as.factor(rep(1:(n_brands), tobs)), t=rep(1:tobs,each=n_brands))
neworder = order(index$var,index$t)

df <- data.frame(xsim, y=y, index)[neworder,]
index=index[neworder,]

dtbb = attraction_data(formula=y~X3+X4+X5|X1+X2|var+t, data=df)

m<-itersur(X=as.matrix(dtbb@X), Y=as.matrix(dtbb@y), index=data.frame(period=dtbb@period,brand=dtbb@individ))


