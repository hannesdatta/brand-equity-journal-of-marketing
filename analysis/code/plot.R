
require(lattice)
require(latticeExtra)


	#xyplot(value ~ week|variable, groups=brand_name, data=tmp, scales = list(y = list(relation = "free")),type='l',#par.settings = theEconomist.theme(box = "transparent"),
	#	lattice.options = theEconomist.opts(),
	# auto.key=list(space="bottom", columns=2, 
	#						   title="Categories", cex.title=1, lines=T, points=F))



				   
						   
						   
						   
for (i in seq(along=tmp)) {
	print(i)
	tmp[[i]][, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
	levels(tmp[[i]]$country)<-rename.fkt(levels(tmp[[i]]$country))
	levels(tmp[[i]]$category)<-rename.fkt(levels(tmp[[i]]$category))

	png(paste0('../output/salesplots_', i, '.png'), res=200, units='in', height=8, width=12)
	print(xyplot(sales~date|country, groups=category,data=tmp[[i]], auto.key=list(space="bottom", columns=2, 
						   title="Categories", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free")),
				
				main=paste('Categories: ', paste(cats[[i]],collapse=', '),sep=''),
				
	dev.off()


}
