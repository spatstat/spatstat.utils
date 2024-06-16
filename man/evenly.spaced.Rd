\name{evenly.spaced}
\alias{evenly.spaced}
\title{
  Determine Whether a Vector is Evenly Spaced and Increasing
}
\description{
  Determines whether the entries in a numeric vector are evenly spaced
  and increasing.
}
\usage{
evenly.spaced(x, tol = 1e-07)
}
\arguments{
  \item{x}{
    Numeric vector.
}
  \item{tol}{
    Relative tolerance.
  }
}
\details{
  The result is \code{TRUE} if \code{x} is an increasing sequence
  in which the successive differences \code{diff(x)}
  are all equal to one another (within the specified relative tolerance),
  and \code{FALSE} otherwise.
}
\value{
  A single logical value.
}
\author{
  \adrian.
}
\examples{
  evenly.spaced(seq(0, 1, length=4))
}
\keyword{manip}
