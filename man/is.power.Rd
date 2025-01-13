\name{is.power}
\alias{is.power}
\alias{is.square}
\alias{is.cube}
\title{
  Recognise a Square, Cube, or Power of an Integer
}
\description{
  Determine whether the given integer is a square number,
  a cube number, or a power of an integer.
}
\usage{
is.square(n)
is.cube(n)
is.power(n)
}
\arguments{
  \item{n}{
    A single integer.
}
}
\details{
  \code{is.square(n)} returns \code{TRUE} if \code{n}
  is a square number, that is, \code{n = m^2} for some integer \code{m},
  and returns \code{FALSE} otherwise.

  \code{is.cube(n)} returns \code{TRUE} if \code{n}
  is the cube of an integer, \code{n = m^3} for some integer \code{m},
  and returns \code{FALSE} otherwise.

  \code{is.power(n)} returns \code{TRUE} if \code{n}
  is an integer power of an integer, \code{n = m^k} for some integers
  \code{m} and \code{k}, and returns \code{FALSE} otherwise.

  These functions use the prime factorisation of \code{n}
  and may be more reliable than testing where \code{sqrt(n)} is an
  integer, etc.

  Negative values of \code{n} are permitted.
}
\value{
  A single logical value.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{primefactors}}
}
\examples{
  is.square(9)
  is.cube(9)
  is.cube(27)
  is.cube(-27)
  is.power(27)
  is.power(3^5)
}
\keyword{math}
