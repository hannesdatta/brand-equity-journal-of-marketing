source('attraction.R')
source('itersur.R')
source('attraction_data.R')

#setClass("attr.bb", representation(X="matrix",
#								   y="numeric",
#								   individ = "numeric",
#								   period = "numeric"),
#					contains = "attr.raw")
					
dt = new("attr.raw")
#validObject(dt)

rawdata <- attraction_data()

# Populate dt
	dt@X <- as.matrix(rawdata$X)
	dt@y <- as.numeric(rawdata$y)
	dt@individ <- as.character(rawdata$index$var)
	dt@period <- rawdata$index$t
	validObject(dt)
			
	show(dt)

# Convert to base-brand system (using the last brand as benchmark)
	dtbb <- convertbb(dt, heterogenous = c(1,1,0,0,0))

# Estimate the system
	m <- itersur(X=dtbb@X,Y=as.matrix(dtbb@y), dates_brands=data.frame(date=dtbb@period,brand=dtbb@individ))
	
	m$coefficients$variable <- c(paste0(rep(paste0('var', 1:ncol(xsim[, pars_heterog]), '_'), each=n_brands), rep(paste0('b', 1:n_brands), ncol(xsim[, pars_heterog]))),
								 paste0(rep(paste0('attr', 1:ncol(xsim[, pars_homog]), '_'), each=1)), 
								 paste0('dum', 1:(n_brands-1)))

	ols=solve(t(X)%*%X)%*%t(X)%*%Y
	#m$coefficients$true=c(matrix(tbeta, nrow=length(unlist(tbeta)), byrow=T), tattr, tint[-n_brands])
	m$coefficients$ols=ols

	print(m$sigma)
	m$coefficients

# Next steps: calculate elasticities
