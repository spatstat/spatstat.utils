\name{ifelseAB}
\alias{ifelse0NA}
\alias{ifelse1NA}
\alias{ifelseAB}
\alias{ifelseAX}
\alias{ifelseXB}
\alias{ifelseXY}
\alias{ifelseNegPos}
\title{
  Conditional Selection
}
\description{
  These low-level functions provide faster alternatives to
  some uses of \code{ifelse}.
}
\usage{
ifelseAB(test, a, b)
ifelseAX(test, a, x)
ifelseXB(test, x, b)
ifelseXY(test, x, y)
ifelseNegPos(test, x)
ifelse0NA(test)
ifelse1NA(test)
}
\arguments{
  \item{test}{A logical vector.}
  \item{a}{A single atomic value.}
  \item{b}{A single atomic value.}
  \item{x}{A vector of values, of the same length as \code{test}.}
  \item{y}{A vector of values, of the same length as \code{test}.}
}
\details{
  These low-level functions provide faster alternatives to
  some uses of \code{\link[base]{ifelse}}. They were developed by
  trial-and-error comparison of computation times of different expressions.

  \code{ifelse0NA(test)} is equivalent to \code{ifelse(test, 0, NA)}.

  \code{ifelse1NA(test)} is equivalent to \code{ifelse(test, 1, NA)}.

  \code{ifelseAB(test, a, b)} is equivalent to \code{ifelse(test, a, b)}
  where \code{a} and \code{b} must be single values.

  \code{ifelseAX(test, a, x)} is equivalent to \code{ifelse(test, a, x)}
  where \code{a} must be a single value, and \code{x} a vector of the
  same length as \code{test}.

  \code{ifelseXB(test, x, b)} is equivalent to \code{ifelse(test, x, b)}
  where \code{b} must be a single value, and \code{x} a vector of the
  same length as \code{test}.

  \code{ifelseXY(test, x, y)} is equivalent to \code{ifelse(test, x, y)}
  where \code{x} and \code{y} must be vectors of the
  same length as \code{test}.

  \code{ifelseNegPos(test, x)} is equivalent to \code{ifelse(test, x, -x)}
  where \code{x} must be a vector of the same length as \code{test}.
}
\value{
  A vector of the same length as \code{test} containing values of the
  same type as \code{a,b,x,y}.
}
\author{
  \spatstatAuthors.
}
\seealso{
  \code{\link[base]{ifelse}}
}
\examples{
   x <- runif(4e5)
   u <- (x < 0.5)
   system.time(ifelse(u, 2, x))
   system.time(ifelseAX(u, 2, x))
}
\keyword{manip}
\keyword{utilities}
