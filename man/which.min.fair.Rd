\name{which.min.fair}
\alias{which.min.fair}
\alias{which.max.fair}
\title{
  Where is the Minimum or Maximum
}
\description{
  Determines the index of the minimum or maximum of a vector.
  If there are multiple entries which achieve the minimum or maximum,
  one of the indices is selected at random.
}
\usage{
which.min.fair(x)
which.max.fair(x)
}
\arguments{
  \item{x}{
    numeric, logical, integer or double vector.
  }
}
\details{
  These functions are alternatives to
  the standard \R functions \code{\link[base]{which.min}}
  and \code{\link[base]{which.max}}.

  The standard functions \code{\link[base]{which.min}}
  and \code{\link[base]{which.max}} find the index of the
  \bold{first} entry in the vector \code{x} which achieves the minimum
  or maximum value. This can cause a bias in some simulation
  experiments.

  The functions \code{which.min.fair} and \code{which.max.fair}
  identify all entries of the vector \code{x} which achieve the
  minimum or maximum respectively, and
  \bold{select one of them at random}. 
}
\value{
  A single integer (or \code{integer(0)} if all entries of \code{x} are
  \code{NA} or \code{NaN}).
}
\author{
  \adrian
}
\seealso{
  \code{\link[base]{which.min}}
}
\examples{
  z <- c(20, 40, 20, 10, 40, 20, 10, 20, 40)
  replicate(5, which.max(z))
  replicate(5, which.max.fair(z))
  replicate(5, which.min.fair(z))
}
\keyword{utilities}
