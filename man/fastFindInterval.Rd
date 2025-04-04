\name{fastFindInterval}
\alias{fastFindInterval}
\title{
  Find Intervals Containing Given Data
}
\description{
  A faster alternative to \code{findInterval}
  for intervals which are equally-spaced.
}
\usage{
fastFindInterval(x, b, labels = FALSE, reltol = 0.001, dig.lab = 3L,
                 left.open=TRUE)
}
\arguments{
  \item{x}{
    Data. Numeric vector of values that are to be classified.
  }
  \item{b}{
    Breakpoints. Numeric vector of increasing values that are the
    endpoints of the intervals.
  }
  \item{labels}{
    Logical value specifying whether to return a factor,
    whose levels are the string labels of the intervals.
  }
  \item{reltol}{
    Relative tolerance. A positive number.
  }
  \item{dig.lab}{
    Integer. Maximum number of digits to use in the labels
    for the intervals, when \code{labels=TRUE}.
  }
  \item{left.open}{
    Logical value specifying whether intervals are left-open and
    right-closed (\code{left.open=TRUE}, the default)
    or left-closed and right-open (\code{left.open=FALSE}).
  }
}
\details{
  This is an alternative to
  \code{\link[base]{findInterval}(x, b, rightmost.closed=TRUE)}
  which seems to be faster when \code{b} is equally spaced
  and the length of \code{x} is large.

  If \code{labels=FALSE} (the default), the result is an integer vector
  giving, for each value \code{x[i]}, the index \code{j} of the interval that
  contains \code{x[i]}:
  \itemize{
    \item
    If \code{left.open=TRUE} (the default), the intervals are left-open
    and right-closed, except for the first interval. This means that
    \code{x[i]} belongs to the \code{j}th interval if 
    \code{b[j] < x[i] <= b[j+1]} for \code{j > 1}
    and \code{b[1] <= x[i] <= b[2]} for \code{j=1}.
    \item
    If \code{left.open=FALSE}, the intervals are left-closed
    and right-open, except for the last interval. This means that
    \code{x[i]} belongs to the \code{j}th interval if 
    \code{b[j] <= x[i] < b[j+1]} for \code{j < m}
    and \code{b[m] <= x[i] <= b[m+1]} for \code{j=m}
    where \code{m = length(b)-1} is the number of intervals.
  }

  If \code{labels=TRUE}, the result is a factor, and the levels are
  synthetic labels for the intervals, similar to those produced by
  \code{\link[base]{findInterval}}.

  Note that the default value of \code{left.open}
  is \code{TRUE} for \code{fastFindInterval} 
  but \code{FALSE} for \code{\link[base]{findInterval}}.
}
\value{
  Integer vector, or factor.
}
\author{
  \spatstatAuthors.
}
\seealso{
  \code{\link[base]{findInterval}}
}
\examples{
  x <- runif(10)
  b <- seq(0, 1, by=0.2)
  fastFindInterval(x, b, labels=TRUE)
}
\keyword{manip}
