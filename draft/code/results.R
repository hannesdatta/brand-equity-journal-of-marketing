#     _    ____     __      __  _____           _           _      _  
#    | |  |  _ \   /\ \    / / |  __ \         (_)         | |    | | 
#   / __) | |_) | /  \ \  / /  | |__) | __ ___  _  ___  ___| |_  / __)
#   \__ \ |  _ < / /\ \ \/ /   |  ___/ '__/ _ \| |/ _ \/ __| __| \__ \
#   (   / | |_) / ____ \  /    | |   | | | (_) | |  __/ (__| |_  (   /
#    |_|  |____/_/    \_\/     |_|   |_|  \___/| |\___|\___|\__|  |_| 
#                                             _/ |                    
#                                            |__/                     


# Load data sets and results
source('load.R')
require(knitr)


###############################
#                             #
#                             #
#  M O D E L   R E S U L T S  #
#                             #
#                             #
###############################

options(marketingtools_rename=paste0('renaming.txt'))

signstars <- function(zscore) { # converts a z-score into a signifance asteriks
	  if (length(zscore)==0) return("   ")
	  if (is.nan(zscore) | !is.numeric(zscore) | is.na(zscore)) return("   ")
	  ret <- "ns."
	  #if (abs(zscore)>qnorm(1-(0.1))) ret <- c(paste("  ", rawToChar(as.raw(134)), sep=''))
	  if (abs(zscore)>qnorm(1-(0.1/2))) ret <- c("  .")
	  if (abs(zscore)>qnorm(1-(0.05/2))) ret <- c("  *")
	  if (abs(zscore)>qnorm(1-(0.01/2))) ret <- c(" **")
	  if (abs(zscore)>qnorm(1-(0.001/2))) ret <- c("***")
	  return(ret)
	  }
	
# BUILD LATEX REPORTS FOR EACH SUBELEMENT OF RESULTS
for (r in 1:2) {# seq(along=results)) {

	# Run report
	savewd = getwd()
	setwd('..//temp')
	results=allres[[r]]
	checked=checked_all[[r]]
	knit("..//code//template.Rnw", output = 'results.tex')
	shell(paste0('pdflatex results.tex -job-name=', paste0('results_', r, ''), ' -output-directory=..//output'))
	setwd(savewd)
	
	sink(paste0("..//output//vifs_", r, ".txt"))
	source('diagnostics.R')
	sink()
	
	}

	