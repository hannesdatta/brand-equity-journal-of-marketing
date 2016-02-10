	require(lattice)
	require(latticeExtra)
	# glayer 
	# code by http://www.r-bloggers.com/confidence-bands-with-lattice-and-r/
	
	my.panel.bands <- function(x, y, upper, lower, fill, col,
		 subscripts, ..., font, fontface)
		 {
		 upper <- upper[subscripts]
		 lower <- lower[subscripts]
		 panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
		 col = fill, border = FALSE,
		 ...)
		 }
