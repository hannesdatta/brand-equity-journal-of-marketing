require(data.table)
require(reshape2)	
	
# Introduction to making classes in R

setClass("attr.raw",
	representation(X="matrix",
				   y="numeric",
				   individ = "character",
				   period = "numeric"),
	validity = function(object) {
		retval=NULL
		
		if (!length(unique(colnames(object@X)))==ncol(object@X)) retval <- c(retval, 'Column names of X not properly specified')
		if (ncol(object@X)==0|nrow(object@X)==0) retval <- c(retval, 'X has 0-dimension')
		
		if (is.null(retval)) return(TRUE) else return(retval)
		})

setMethod("show", "attr.raw", function(object) {
			cat('Attraction model: Summary of raw data\n')
			cat('    Number of total cross-sectional units         : ', length(unique(object@individ)), '\n')
			cat('    Number of total periods                       : ', length(unique(object@period)), '\n')
			cat('    Number of variables                           : ', ncol(object@X), '\n')
			cat('    Observations per cross-sectional unit         : ', table(object@individ), '\n')
			cat('    Column names of X                             : ',paste(colnames(object@X), collapse= ', '),'\n')
			cat('\n')
		
			# check number of overlapping observations
			
			
			})


setGeneric("convertbb", function(object, ...) {})


setMethod("convertbb", "attr.raw", function(object, model = 'MCI', heterogenous = rep(0, ncol(object@X))) {
	#print(heterogenous)
	#print(object)
	n_individ = length(unique(object@individ))
	
	# Fixes base-brand to be the last brand (for prototyping)
	
	# create conversion matrix for base-brand approach
	Hbb = diag(n_individ-1)
	Hbb = cbind(Hbb, -1)

	# Transformed X and Y matrices
	#x_hom <- object@X[,heterogenous==0]
	#x_het <- object@X[,heterogenous==1]
	
	dtmelt <- melt(data.frame(object@X, y=object@y, individ=object@individ, period=object@period), id.vars=c('individ', 'period'))
	
	# select benchmark brand: the one with most available observations (at a tie, take last one)
	tmp <- table(dtmelt[which(dtmelt$variable=='y' & !is.na(dtmelt$value)), c('individ')])
	bindivid = rev(names(tmp)[which(tmp==min(tmp))])[1]
	aindivid = names(tmp)[!names(tmp)==bindivid]
	
	# stacked data set
	dtcast = rbindlist(lapply(aindivid, function(x) {
		tmp=dcast(dtmelt[dtmelt$individ%in%c(x, bindivid),], period ~ individ + variable)
		# kick out variables which are unavailable throughout the whole period
		tmp=tmp[,!colSums(is.na(tmp))==nrow(tmp)]
		tmp[complete.cases(tmp),]
		}), fill=TRUE)
	
	
	iindex = aindivid[apply(dtcast[, mget(paste0(aindivid,'_y'))],1, function(x) which(!is.na(x)))]
	
	#dtcast[, individ:=iindex]
	
	# apply transformations
	if (model=='MCI') tfkt <- function(x) log(x)
	if (model=='MNL') tfkt <- function(x) x
	
	# heterogenous transformation:
	# find variables which are heterogenous
	x_hom <- colnames(object@X)[which(heterogenous==0)]
	x_het <- colnames(object@X)[which(heterogenous==1)]
	
	x_hom2 <- grep(paste(x_hom,collapse='|'), colnames(dtcast),value=T)
	x_het2 <- grep(paste(x_het,collapse='|'), colnames(dtcast),value=T)
	y_hom2 <- grep(paste('y',collapse='|'), colnames(dtcast),value=T)
	
	dttrans <- dtcast
	
	# transformation of 'heterogenous' coefficients
	for (.var in x_het2) {
		if (grepl(paste0('^',bindivid),.var)) {
			# variable pertaining to base brand
			dttrans[, .var:= -tfkt(get(.var)),with=F]
			} else {
			# variable pertaining to other brands
			dttrans[, .var:= tfkt(get(.var)),with=F]
			}
		}
		
	# transformation of 'homogenous' coefficients
	for (.var in c(x_hom)) {
		# identify target cols
		tmp1=rowSums(dttrans[, mget(paste0(aindivid, '_', .var))], na.rm=T)
		tmpbase=dttrans[, mget(paste0(bindivid, '_', .var))]
		
		dttrans[, paste0('hom_', .var) := tfkt(tmp1) - tfkt(tmpbase),with=F]
		dttrans[, c(paste0(bindivid, '_', .var),paste0(aindivid, '_', .var)):=NULL, with=F]
		}

	# transformation of 'y' coefficients
	for (.var in c('y')) {
		# identify target cols
		tmp1=rowSums(dttrans[, mget(paste0(aindivid, '_', .var))], na.rm=T)
		tmpbase=dttrans[, mget(paste0(bindivid, '_', .var))]
		
		dttrans[, paste0('hom_', .var) := log(tmp1/tmpbase),with=F]
		dttrans[, c(paste0(bindivid, '_', .var),paste0(aindivid, '_', .var)):=NULL, with=F]
		}

	dummies = as.matrix(model.matrix( ~ as.factor(iindex) - 1))
	colnames(dummies)<-paste0(aindivid,'_dum')

	
	# remove homogenous (pre-transformation columns) from dttrans
	dttrans[is.na(dttrans)]<-0
	object@X <- as.matrix(cbind(dttrans[, !grep('hom_y|period', colnames(dttrans),value=T),with=F], dummies))
	object@y <- as.numeric(dttrans$hom_y)
	object@period <- as.numeric(dttrans$period)
	object@individ <- iindex
	return(object)
	})


