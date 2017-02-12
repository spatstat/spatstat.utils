\name{check.range}
\alias{check.range}
\alias{check.in.range}
\alias{inside.range}
\alias{intersect.ranges}
\alias{prange}
\title{
  Utilities for Ranges of Values
}
\description{
  These simple functions handle an interval or range of numerical
  values. \code{check.range(r)} checks whether \code{r} specifies a
  range of values, that is, whether \code{r} is a vector of length 2
  with \code{r[1] <= r[2]}. \code{intersect.ranges(r, s)} finds the intersection
  of two ranges \code{r} and \code{s}. \code{inside.range(x, r)} returns
  a logical vector containing \code{TRUE} if the corresponding entry of
  \code{x} falls inside the range \code{r}, and \code{FALSE} if it does
  not. \code{check.in.range(x, r)} checks whether a single number
  \code{x} falls inside the specified range \code{r}.
  Finally \code{prange(r)} produces a character string that represents
  the range \code{r}.
}
\usage{
check.range(r, fatal = TRUE)

check.in.range(x, r, fatal = TRUE)

inside.range(x, r)

intersect.ranges(r, s, fatal = TRUE)

prange(r)
}
\arguments{
  \item{r}{
    A numeric vector of length 2 specifying the endpoints of a range of values.
  }
  \item{x}{
    Numeric vector of data.
  }
  \item{s}{
    A numeric vector of length 2 specifying the endpoints of a range of values.
  }
  \item{fatal}{
    Logical value indicating whether to stop with an error message
    if the data do not pass the check. 
  }
}
\details{
  \code{check.range} checks whether \code{r} specifies a
  range of values, that is, whether \code{r} is a vector of length 2
  with \code{r[1] <= r[2]}. If so, the result is \code{TRUE}. If not,
  then if \code{fatal=TRUE}, an error occurs, while if
  \code{fatal=FALSE} the result is \code{FALSE}.

  \code{intersect.ranges(r, s)} finds the intersection
  of two ranges \code{r} and \code{s}. If the intersection is non-empty,
  the result is a numeric vector of length 2. If the intersection is empty,
  then if \code{fatal=TRUE}, an error occurs, while if
  \code{fatal=FALSE} the result is \code{NULL}.

  \code{inside.range(x, r)} returns
  a logical vector containing \code{TRUE} if the corresponding entry of
  \code{x} falls inside the range \code{r}, and \code{FALSE} if it does
  not.

  \code{check.in.range(x, r)} checks whether a single number
  \code{x} falls inside the specified range \code{r}.
  If so, the result is \code{TRUE}. If not, then if \code{fatal=TRUE},
  an error occurs, while if
  \code{fatal=FALSE} the result is \code{FALSE}.
  
  Finally \code{prange(r)} produces a character string that represents
  the range \code{r}.
}
\value{
  The result of \code{check.range}, \code{check.in.range}
  and \code{inside.range}, is
  a logical value or logical vector. The result of
  \code{intersect.ranges} is a numerical vector of length 2, or \code{NULL}. 
  The result of \code{prange} is a character string.
}
\author{
  \adrian
}
\examples{
   rr <- c(0, 2)
   ss <- c(1, 3)
   x <- seq(0.5, 3.5, by=1)
   check.range(rr)
   check.range(42, fatal=FALSE)
   inside.range(x, rr)
   intersect.ranges(rr, ss)
   prange(rr)   
}
\keyword{programming}
\keyword{utilities}
