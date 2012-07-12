
### linear
v0 <- function(x, y, init=F, ...) {
    return(c(x, y))
}

### sinusoidal
v1 <- function(x, y, init=F, ...) {
    return(c(sin(x), sin(y)))
}

### spherical
v2 <- function(x, y, init=F, ...) {
    r2 <- x^2 + y^2
    return(c(x/r2, y/r2))
}

### swirl
v3 <- function(x, y, init=F, ...) {
    r2 <- x^2 + y^2
    return(c(x*sin(r2) - y*cos(r2), x*cos(r2) + y*sin(r2)))
}

### horsehoe
v4 <- function(x, y, init=F, ...) {
    r <- sqrt(x^2 + y^2)
    return(1/r * c((x-y) * (x+y), 2*x*y))
}

variations <- rbind(
        cbind(name='linear', fun=v0),
        cbind(name='sinusoidal', fun=v1),
        cbind(name='spherical', fun=v2),
        cbind(name='swirl', fun=v3),
        cbind(name='horsehoe', fun=v4)
        )

variations <- data.frame(variations)

applyVariation <- function(point, var, affine, v) {

    x <- point[1]
    y <- point[2]

    ### affine transformed input to variation
    ax <- affine[1]*x + affine[3]*y + affine[5]
    ay <- affine[2]*x + affine[4]*y + affine[6]

    return(v * var(ax, ay))
}
