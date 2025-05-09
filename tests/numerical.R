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
y <- fave.order(x)

#' must handle NA's without exiting
x[c(4, 7, 42)] <- NA
aa <- orderstats(x, 2:5)
aa <- orderstats(x, 2:5, decreasing=TRUE)
bb <- orderwhich(x, 2:5)
bb <- orderwhich(x, 2:5, decreasing=TRUE)
x[] <- NA
uu <- orderstats(x, 2:5)
uu <- orderstats(x, 2:5, decreasing=TRUE)
vv <- orderwhich(x, 2:5)
vv <- orderwhich(x, 2:5, decreasing=TRUE)

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
#' tapplysum with NA values in x
xx <- runif(12)
aa <- rep(aa, 4)
bb <- rep(bb, 4)
ee <- sample(aa)
ff <- sample(bb)
xx[2] <- NA
uu1 <- tapplysum(xx, list(aa),             do.names=TRUE)
uu2 <- tapplysum(xx, list(aa, bb),         do.names=TRUE)
uu3 <- tapplysum(xx, list(aa, bb, ee),     do.names=TRUE)
uu4 <- tapplysum(xx, list(aa, bb, ee, ff), do.names=TRUE)

#' validity of matchIntegerDataFrames
#' 3 columns
A <- data.frame(a=sample(1:5), b=sample(1:5, replace=TRUE), c=3)
B <- data.frame(u=sample(1:3), w=3:1,                       v=1)
A[4,] <- B[2,]
a3code <- paste(A[,1], A[,2], A[,3])
b3code <- paste(B[,1], B[,2], B[,3])
stopifnot(identical(matchIntegerDataFrames(A,B,TRUE), match(a3code,b3code)))
stopifnot(identical(matchIntegerDataFrames(A,B,FALSE), match(a3code,b3code)))
#' 2 columns
A <- A[,1:2]
B <- B[,1:2]
a2code <- paste(A[,1], A[,2])
b2code <- paste(B[,1], B[,2])
stopifnot(identical(matchIntegerDataFrames(A,B,TRUE), match(a2code,b2code)))
stopifnot(identical(matchIntegerDataFrames(A,B,FALSE), match(a2code,b2code)))
#' 1 column
A <- A[,1, drop=FALSE]
B <- B[,1, drop=FALSE]
a1code <- paste(A[,1])
b1code <- paste(B[,1])
stopifnot(identical(matchIntegerDataFrames(A,B,TRUE), match(a1code,b1code)))
stopifnot(identical(matchIntegerDataFrames(A,B,FALSE), match(a1code,b1code)))

#'  code in utilseq.R

dropifsingle(list(42))
dropifsingle(1:2)

revcumsum(1:5 * (1 + 2i))

as2vector(3:4)
as2vector(list(x=1, y=1))
ensure2vector(3:4)
ensure2vector(3)

prolongseq(2:5, newrange=c(1,9))

fillseq(c(1:3, 5:7, 9))

geomseq(0.5, 2, 10)

check.in.range(4, c(1,10))

startinrange(runif(1), 1, c(3, 7))

prettyinside(runif(10,max=5))

prettydiscrete(letters)

evenly.spaced(seq(0, 1, length.out=7))

equispaced(seq(0, 1, length.out=7))

adjustthinrange(c(0.0000001, 0.999999), 1, c(0,1))

fastFindInterval(runif(100), seq(0,1,length.out=8), labels=TRUE, dig.lab=2)

ifelseAB(pi > c(3, 3.5, 4), "less", "more")
ifelseXB(pi > c(3, 3.5, 4), rep("less", 3), "more")
ifelseXY(pi > c(3, 3.5, 4), rep("less", 3), rep("more",3))
ifelse1NA(pi > c(3, 3.5, 4))
ifelse0NA(pi > c(3, 3.5, 4))
ifelseNegPos(pi > c(3, 3.5, 4), 1:3)

ratiotweak(c(1,1,0,0), c(1,0,1,0), 42, 24)
natozero(c(1,1,0,0)/c(1,0,1,0))

insertinlist(letters[1:5], 4, rep("hoppity", 3))

dround(pi)
niceround(pi)

## prime numbers 

## code coverage of special cases

eratosthenes(20)

primefactors(8209 * 3)

stopifnot(identical(primefactors(42),
                    primefactors(42, "interpreted")))

stopifnot(is.prime(399137))
