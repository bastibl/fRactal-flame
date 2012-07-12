require(XML)
source('variations.R')

load.flame <- function(filename) {

	funId <- 0
	weights <- c()
	colors <- c()
	affine <- matrix(ncol=6, nrow=0)
	funSet <- data.frame()

	root <- xmlRoot(xmlTreeParse(filename))
	nodes <- xmlChildren(root[[1]])

	### image size
	attr <- xmlAttrs(root[[1]])
	size <- as.numeric(scan(text=attr[['size']]))
	center <- as.double(scan(text=attr[['center']]))
	scale <- as.double(attr[['scale']])

	for(node in nodes) {

		### one function (that possibly consists of multiple variations)
		if(xmlName(node) == "xform") {
			funId <- funId + 1
			attr <- xmlAttrs(node)

			weights <- c(weights, as.double(attr['weight']))
			colors <- c(colors, as.double(attr['color']))
			affine <- rbind(affine, as.double(scan(text=attr['coefs'])))

			for(i in 1:length(attr)) {
				if(!(names(attr)[i] %in% c('weight', 'color', 'coefs'))) {
					varWeight <- as.double(attr[[i]])
					varId <- which(variations$name == names(attr)[i])
					if(length(varId) != 1) {
						error(paste('variation not fund: ', names(attr)[i]))
					}
					varId <- varId[1]
					funSet <- rbind(funSet, c(funId, varId, varWeight))
				}
			}

		### the color palette
		} else if (xmlName(node) == "palette") {
			if(xmlAttrs(node)[['format']] != 'RGB') {
				error(paste('color format not implemented: ',
				xmlAttrs(node)[['format']]))
			}
			palette <- xmlValue(node)
			palette <- gsub('[ ,\n]', '', palette)              # remove space and line break
			#palette <- gsub("(.{6})", "#\\1 ", palette)        # split pairs of 2 chars
			#palette <- strsplit(palette, ' ')                   # split in list of substrings
			#palette <- palette[[1]]
			palette <- matrix(palette[[1]], ncol=3) # convert number
			palette <- gsub("(.{2})", "0x\\1 ", palette)        # split pairs of 2 chars
			palette <- strsplit(palette, ' ')                   # split in list of substrings
			palette <- matrix(as.numeric(palette[[1]]), ncol=3) # convert number

		### something we don't know
		} else {
			warning(paste('unknown node: ', xmlName(node)))
		}
	}


	weights <- scale.weights(weights)
	names(funSet) <- c('funId', 'varId', 'varWeight')
	return(list(weights=weights, colors=colors, affine=affine,
		funSet=funSet, palette=palette, size=size, center=center, scale=scale))

}

### create cdf from weights
scale.weights <- function(weights) {
	return(cumsum(weights)/sum(weights))
}
