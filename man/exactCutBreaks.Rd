\name{exactCutBreaks}
\alias{exactCutBreaks}
\title{
  Determine Breakpoints for Cut
}
\description{
  Computes the numerical breakpoints used by \code{cut.default}.
}
\usage{
exactCutBreaks(x, breaks)
}
\arguments{
  \item{x}{Numeric vector which would be converted to a factor.}
  \item{breaks}{
    Either a numeric vector of breakpoints,
    or a single integer giving the number of intervals into which
    \code{x} will be cut.
  }
}
\details{
  This function contains a copy of the code in
  \code{\link[base:cut]{cut.default}} which determines the
  numerical breakpoints used to convert \code{x} to a factor.
  It returns the breakpoints only.

  The arguments \code{x} and \code{breaks} have the same interpretation
  as in \code{\link[base:cut]{cut.default}}. Only the range of \code{x}
  is used in the computation, so \code{x} could be replaced by \code{range(x)}.

  This function would normally be used when \code{breaks} is a single
  integer specifying the number of intervals for the cut operation.
  It returns the exact numerical values of the breakpoints which are
  determined, but not returned, by \code{\link[base:cut]{cut.default}}).
}
\value{
  Numeric vector.
}
\author{
  \adrian.
}
\seealso{
  \code{\link[base:cut]{cut.default}}
}
\examples{
   exactCutBreaks(c(0,1), 4)
}
\keyword{manip}
