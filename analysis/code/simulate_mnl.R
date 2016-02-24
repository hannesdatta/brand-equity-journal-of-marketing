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

# Convert simulated data to MNL


n_brands = 3
set.seed(1984)

tint = c(-1,-2,0)

tbeta = matrix(c(.5, 1, # true betas (note: heterogenous coefficients)
				 .5, 1,
				 .5,1), ncol=2, byrow=T)

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
	
	xbeta = xsim[index,] *  rbind(tbetaattr[-benchm,], -tbetaattr[benchm,])
	
	# compute m_it
	m <- double(n_brands)
	m[benchm] <- exp(tint[benchm])
	m[-benchm] <- exp(tint[-benchm]+nu + apply(xbeta[-benchm,], 1, "sum") + rep(sum(xbeta[benchm,]), n_brands-1))
	
	M=m/sum(m)
	
	y[index] <- M
	}

index = data.frame(var=as.factor(rep(1:(n_brands), tobs)), t=rep(1:tobs,each=n_brands))

colnames(xsim) <- paste0('var_', 1:5)

require(marketingtools)
require(Formula)


data = data.frame(xsim, y=y, ind=index[,1], time=index[,2])
#data$var_1 = NA
#data$var_2 = NA

data <- cbind(data[,1:5]-colMeans(data[,1:5]), data[, 6:8])

dat <- attraction_data(formula=y~var_3+var_4+var_5 + var_2 | var_1 | ind+time, data = data, model = "MNL")

X=dat@X
y=dat@y

m <- itersur(X=dat@X,Y=as.matrix(dat@y), index=data.frame(date=dat@period,brand=dat@individ))





#####

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)


mydata2 <- mydata
mydata2$gre = mydata2$gre-mean(mydata2$gre)
mydata2$gpa = mydata2$gpa-mean(mydata2$gpa)
mydata2$rank = mydata2$rank-mean(mydata2$rank)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
mylogit2 <- glm(admit ~ gre + gpa + rank, data = mydata2, family = "binomial")
summary(mylogit)
summary(mylogit2)


#data.frame(as.numeric(predict(mylogit, type = 'response')), as.numeric(predict(mylogit2, type='response')))




