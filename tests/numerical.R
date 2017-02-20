#'   spatstat.utils/tests/numerical.R
#'   Tests of numerical code

require(spatstat.utils)

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
