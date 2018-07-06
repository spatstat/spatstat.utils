\name{tapplysum}
\alias{tapplysum}
\title{
  Sum By Factor Level
}
\description{
  A faster equivalent of \code{tapply(FUN=sum)}.
}
\usage{
  tapplysum(x, flist, do.names = FALSE, na.rm = TRUE)
}
\arguments{
  \item{x}{
    Vector of numeric or complex values.
  }
  \item{flist}{
    A list of \code{factor}s of the same length as \code{x}.
  }
  \item{do.names}{
    Logical value indicating whether to attach names to the result.
  }
  \item{na.rm}{
    Logical value indicating whether to remove \code{NA} values
    before computing the sums.
  }
}
\details{
  This function is designed to be a faster alternative to
  the idiom \code{y <- \link[base]{tapply}(x, flist, sum); y[is.na(y)] <- 0}.
  The result \code{y} is a vector, matrix or array of
  dimension equal to the number of factors in \code{flist}. Each
  position in \code{y} represents one of the possible combinations
  of the factor levels. The resulting value in this position
  is the sum of all entries of \code{x}
  where the factors in \code{flist} take this particular combination of
  values. The sum is zero if this combination does not occur.

  Currently this is implemented for the cases where \code{flist}
  has length 1, 2 or 3 (resulting in a vector, matrix or 3D array,
  respectively).
  For other cases we fall back on \code{\link[base]{tapply}}.
}
\value{
  A numeric vector, matrix or array.
}
\author{
  \adrian and Tilman Davies.
}
\seealso{
  \code{\link[base]{tapply}}, \code{\link[base]{table}}
}
\examples{
   x <- 1:12
   a <- factor(rep(LETTERS[1:2], each=6))
   b <- factor(rep(letters[1:4], times=3))
   ff <- list(a, b)
   tapply(x, ff, sum)
   tapplysum(x, ff, do.names=TRUE)
   tapplysum(x + 2i, ff, do.names=TRUE)
}
\keyword{arith}
\keyword{utilities}
