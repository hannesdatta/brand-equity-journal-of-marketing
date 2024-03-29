#########################################################################
# Category characteristics survey for the IRI Marketing Science dataset #
#########################################################################

# Converts raw data to file with means and standard deviations per construct
# Author: h.datta@tilburguniversity.edu



# Load packages
library(foreign)
library(data.table)
library(reshape2)
library(psych)

# Load data
raw_survey <- data.table(read.spss('grocery_survey.sav', to.data.frame=T))
setnames(raw_survey, 'X..', 'user_id')

# Write raw data to CSV (to ensure data is more persistent)
fwrite(raw_survey, 'grocery_survey.csv', row.names=F)

# Procedure to reformat the data
items <- c('important', 'interest', 'fun', 'enjoyable', 'practical', 'necessary', 'muchtolose', 'largedifferences', 'tellalot', 'sayswhotheyare')

for (i in items) {
	setnames(raw_survey, gsub(i, paste0(i, '_'), colnames(raw_survey), ignore.case=T))
	}

tmp = melt(raw_survey, id.vars=c('user_id', 'Age', 'Gender'), measure.vars=grep(paste0(items, collapse='|'), colnames(raw_survey), ignore.case=T, value=T))
tmp[, category := sapply(as.character(variable), function(x) strsplit(x, '_')[[1]][2])]
tmp[, metric := sapply(as.character(variable), function(x) strsplit(x, '_')[[1]][1])]

tmp2 = melt(raw_survey, id.vars=c('user_id', 'Age', 'Gender'), measure.vars=grep('purchase', colnames(raw_survey),value=T, ignore.case=T))
tmp2[, purchased := grepl('Yes', value)]
tmp2[, category := gsub('Purchase', '', variable)]
setkey(tmp2, user_id, category)

survey <- data.table(dcast(tmp, user_id + Age + Gender + category ~ metric, value.var='value'))
setkey(survey, user_id, category)
survey[tmp2, purchase := i.purchased]

# map category names
survey[category=='Beer', category := 'beer']
survey[category=='Breakfastcereal', category := 'coldcer']
survey[category=='Carbonatedsoftdrinks', category := 'carbbev']
survey[category=='Cigarettes', category := 'cigets']
survey[category=='Coffee', category := 'coffee']
survey[category=='Deodorants', category := 'deod']
survey[category=='Disposablediapers', category := 'diapers']
survey[category=='Frozendinners', category := 'pz_di']
survey[category=='Householdcleaningproducts', category := 'hhclean']
survey[category=='Ketchup', category := 'ketchup']
survey[category=='Laundrydetergents', category := 'laundet']
survey[category=='MargarineampSpreads', category := 'margbutr']
survey[category=='Mayonnaise', category := 'mayo']
survey[category=='Milk', category := 'milk']
survey[category=='Mustard', category := 'mustard']
survey[category=='Pastasauce', category := 'spagsauc']
survey[category=='Peanutbutter', category := 'peanbutr']
survey[category=='Shampoo', category := 'shamp']
survey[category=='Shavingrazors', category := 'rz_bl']
survey[category=='Snackchips', category := 'saltsnck']
survey[category=='Soup', category := 'soup']
survey[category=='Sugarsubstitutes', category := 'sugarsub']
survey[category=='Toilettissue', category := 'toitisu']
survey[category=='Toothpaste', category := 'toothpa']
survey[category=='Yogurt', category := 'yogurt']

# retain only observations for which respondends indicated they purchased in the category
survey = survey[purchase==T]


###################
# Evaluate scales #
###################

scales <- list(cat_hedonic = c('enjoyable','fun'),
			   cat_perfrisk = c('muchtolose','largedifferences'),
			   cat_socdemon = c('tellalot', 'sayswhotheyare'),
			   cat_invol = c('important', 'interest'),
			   cat_utilit = c('practical', 'necessary'))


# Compute mean of constructs 
for (i in seq(along=scales)) {
	# Category-means
	eval(parse(text=paste0('survey[, ', names(scales)[i], ' := rowMeans(data.frame(', paste(scales[[i]], collapse=','), '),na.rm=T)]')))
	}

# Summarize means	and standard deviations
sink('survey_report.txt')
	
means <- survey[, lapply(.SD, function(x) mean(x,na.rm=T)), by=c('category'), .SDcols=names(scales)]
sds <- survey[, lapply(.SD, function(x) sd(x,na.rm=T)), by=c('category'), .SDcols=names(scales)]

setnames(means, 'category', 'cat_name')
setorder(means, cat_name)
setnames(sds, 'category', 'cat_name')
setorder(sds, cat_name)

cat('SUMMARY STATISTICS FOR THE MTURK SURVEY\n============================================\n\n')
cat('Construct means:\n')
print(means)

cat('\n\nConstruct standard deviations:\n')
print(sds)

cat('\n\nMeans and standard deviations of category means across categories:\n')
tmp <- data.frame(apply(means[,-1,with=F], 2, mean))
tmp <- cbind(tmp, apply(means[,-1,with=F], 2, sd))
colnames(tmp) <- c('mean','sd')
print(tmp)

# Compute Cronbach Alpha's
alphas <- lapply(scales, function(x) {
	if (length(x)<2) return(NA)
	alpha_test = alpha(as.matrix(survey[, x, with=F]))
	c(alpha=alpha_test$total$raw_alpha, cor = cor(as.matrix(survey[, x, with=F]), use = 'complete.obs')[1,2])
	})

cat('\n\nCronbach alphas:\n')
print(do.call('rbind', alphas))


sresults <- merge(means, sds, by=c('cat_name'),all.x=T)

setnames(sresults, gsub('[.]x', '_mean', colnames(sresults)))
setnames(sresults, gsub('[.]y', '_sd', colnames(sresults)))

fwrite(sresults, file='survey.csv', row.names=F)
