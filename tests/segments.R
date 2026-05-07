#'  spatstat.utils/tests/segments.R
#'
#'  $Revision: 1.2 $ $Date: 2026/05/07 05:23:48 $

require(spatstat.utils)

#' test of distppll pointed out by Ang Qi Wei

p <- matrix(c(1.5, 0), 1, 2)
l <- matrix(c(0,0,1,0,1,0,2,0), 2, 4, byrow=T)
a <- distppll(p, l, mintype=2, method="interpreted")
d <- distppll(p, l, mintype=2, method="C")
if(a$min.which != d$min.which)
  stop("conflict between C and interpreted code in distppll")

#' check consistency between different C routines
b <- distppllmin(p, l)
if(a$min.which != b$min.which)
  stop("conflict between distppll and distppllmin")

#' check execution of NNdist2segments
u <- NNdist2segments(p[,1], p[,2], l[,1], l[,2], l[,3], l[,4],
                     bigvalue = 100, wantindex=TRUE, wantproj=TRUE)
if(u$index != d$min.which)
  stop("Conflict between C code in distppll and NNdist2segments")
