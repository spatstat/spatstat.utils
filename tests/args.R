#'  spatstat.utils/tests/args.R

require(spatstat.utils)

#' tests of R/utilarg.R

dont.complain.about(anything)

need.all.or.none(whatever=42, something=3)
need.all.or.none(whatever=NULL, something=NULL)

badprobability(c(-1,0,0.5,1,1.5,Inf,NA), FALSE)

