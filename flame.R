source('import_flame.R')

### parameter
numIterations <- 400
numRepetitions <- 100

### load flame
flame <- load.flame('linearsnow.flame')
#flame <- load.flame('whirly_cross.flame')
weights <- flame[['weights']]
affine  <- flame[['affine']]
funSet  <- flame[['funSet']]
colors  <- flame[['colors']]
palette <- flame[['palette']] / 255
width   <- flame[['size']][1] # TODO: right order?
height  <- flame[['size']][2]
scale   <- flame[['scale']]
center  <- flame[['center']]

### prepare image
img <- array(0, dim=c(height, width, 4))

### prepare bins for image
span <- height / (2 * scale)
row.bins <- seq(center[2] - span, center[2] + span, length.out=height+1)
span <- width / (2 * scale)
col.bins <- seq(center[1] - span, center[1] + span, length.out=width+1)

### uses glabal variable palette
map.color <- function(c) {
	### TODO: fix this
	return(palette[floor(c*255 + 1),])
}

### works on global variable img
record.point <- function(point, color) {

	### search x-y bins
	indx <- findInterval(point[1], col.bins)
	if(indx == 0 | indx == length(col.bins)) {
		warning('point not in range')
	}

	indy <- findInterval(point[2], row.bins)
	if(indy == 0 | indy == length(row.bins)) {
		warning('point not in range')
	}

	### record value and increase alpha
	img[indy, indx,1:3] <<- img[indy, indx, 1:3] + color
	img[indy, indx, 4 ] <<- img[indy, indx, 4]  + 1
}

### works on glabal variable img
scale.image.colors <- function() {
	for(h  in 1:height) {
		for(w in 1:width) {
			alpha <- img[h,w,4]
			if(alpha > 0) {
				scale <- log(alpha) / alpha
				img[h,w,1:3] <<- log(img[h,w,1:3] + 1) * scale
			}
		}
	}
	img[,,1:3] <<- img[,,1:3] / max(img[,,1:3])
}

for(i in 1:numRepetitions) {
	cat('repetition  ', i, '\n')
	point <- runif(2, -1, 1)
	col <- runif(1)
	for(k in 1:numIterations) {

		### select function
		r <- runif(1)
		index <- which(r < weights)[1]

		newPoint <- c(0, 0)
		col <- mean(c(col, colors[index]))
		#cat('col: ', col, '   index: ', index, '   colors: ', colors[index], '\n')

		### apply variations
		vars <- funSet[funSet$funId == index,]
		for(v in 1:nrow(vars)) {
			app <- applyVariation(point, variations$fun[[vars$varId[v]]], affine[index,], vars$varWeight[v])
			newPoint <- newPoint + app
		}

		point <- newPoint

		### skip the first iterations
		if(k > 20) {
			record.point(point, map.color(col))
		}
	}
}

scale.image.colors()

### TODO: plog rgb image
plot(c(0, width), c(0, height), type = "n", xlab="", ylab="")
rasterImage(img[,,1:3], 0, 0, width, height)

