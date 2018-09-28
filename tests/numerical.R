#'   spatstat.utils/tests/numerical.R
#'   Tests of numerical code

require(spatstat.utils)

#' validity of orderstats, orderwhich
x <- unique(runif(100))
if(!all(orderstats(x, 2:5) == sort(x)[2:5]))
  stop("Incorrect result from orderstats()")
if(!all(orderwhich(x, 2:5) == order(x)[2:5]))
  stop("Incorrect result from orderwhich()")
if(!all(orderstats(x, 2:5, decreasing=TRUE) == sort(x, decreasing=TRUE)[2:5]))
  stop("Incorrect result from orderstats(decreasing=TRUE)")
if(!all(orderwhich(x, 2:5, decreasing=TRUE) == order(x, decreasing=TRUE)[2:5]))
  stop("Incorrect result from orderwhich(decreasing=TRUE)")

#' validity of 'tapplysum'
aa <- factor(letters[1:3])
bb <- factor(letters[1:4])[c(1,2,2)]
xx <- round(runif(3), 3)
yy <- tapplysum(xx, list(A=aa, B=bb), do.names=TRUE)
zz <- tapply(xx, list(A=aa, B=bb), sum)
zz[is.na(zz)] <- 0
if(any(yy != zz))
  stop("tapplysum does not agree with tapply(, sum)")
#' tapplysum with zero-length data
tapplysum(xx[FALSE], list(A=aa[FALSE], B=bb[FALSE]), do.names=TRUE)

#' validity of matchIntegerDataFrames

A <- data.frame(a=sample(1:5), b=sample(1:5, replace=TRUE))
B <- data.frame(u=sample(1:3), w=3:1)
A[4,] <- B[2,]
acode <- paste(A[,1], A[,2])
bcode <- paste(B[,1], B[,2])
stopifnot(identical(matchIntegerDataFrames(A,B), match(acode,bcode)))

