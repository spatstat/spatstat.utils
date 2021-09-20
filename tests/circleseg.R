#'  spatstat.utils/tests/circleseg.R

require(spatstat.utils)

#' tests of R/xycircle.R, src/circxseg.c

x0 <- (0:4)/4
y0 <- runif(5)
x1 <- x0 + runif(5)
y1 <- y0 + runif(5)

xc <- runif(4)
yc <- runif(4)
rc <- runif(4)

ansX <- xysegXcircle(xc, yc, rc[1:3], x0, y0, x1, y1)
ansM <- xysegMcircle(xc, yc, matrix(rc, 4, 2), x0, y0, x1, y1)
ansP <- xysegPcircle(xc, yc, rc, x0, y0, x1, y1)
