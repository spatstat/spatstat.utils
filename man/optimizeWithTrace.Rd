\name{optimizeWithTrace}
\alias{optimizeWithTrace}
\title{
  One Dimensional Optimization with Tracing 
}
\description{
  Find the minimum or maximum of a function over an interval
  of real numbers, keeping track of the function arguments and
  function values that were evaluated.
}
\usage{
optimizeWithTrace(f, interval, \dots,
                  lower = min(interval), upper = max(interval))
}
\arguments{
  \item{f}{
    The function to be minimized or maximized.
  }
  \item{interval}{
    Numeric vector of length 2 containing the end-points of the interval
    to be searched.
  }
  \item{lower, upper}{
    The lower and upper endpoints of the interval to be searched.
  }
  \item{\dots}{
    Other arguments passed to \code{\link[stats]{optimize}},
    including arguments to the function \code{f}.
  }
}
\details{
  This is a simple wrapper for the optimization routine
  \code{\link[stats]{optimize}}. The function \code{f} will be
  optimized by computing its value at several locations in the interval,
  as described in the help for \code{\link[stats]{optimize}}.
  This wrapper function stores the locations and
  resulting function values, and returns them along with the
  result of the optimization.
}
\value{
  A list with components
  \itemize{
    \item \code{minimum} (or \code{maximum}), the location in the
    search interval which yielded the optimum value;
    \item \code{objective}, the value of the function at this
    location;
    \item \code{x}, the sequence of locations in the interval that
    were considered (in the order considered);
    \item \code{y}, the function values corresponding to \code{x}.
  }
}
\author{
  \spatstatAuthors.
}
\seealso{
  \code{\link{optimize}}
}
\examples{
  f <- function (x, a) (x - a)^2
  result <- optimizeWithTrace(f, c(0, 1), tol = 0.0001, a = 1/3)
  result
  curve(f(x, 1/3))
  with(result, points(x, y, pch=16))
}
\keyword{optimize}
