\name{percentage}
\alias{percentage}
\title{
  Convert Fraction to Percentage
}
\description{
  This is a programmer's utility which converts a fraction to
  a percentage and encodes the percentage as a character string.
}
\usage{
percentage(x, digits = 3)
}
\arguments{
  \item{x}{
    Either a single number, or a logical vector.
}
  \item{digits}{
    Number of digits accuracy.
  }
}
\details{
  If \code{x} is a single number, it should be a fraction between 0 and 1.
  It will be converted to a percentage and then converted to a character
  string followed by the percentage symbol.

  If \code{x} is a logical vector, the fraction of values which are
  \code{TRUE} will be computed, and used to determine the percentage.
}
\value{
  A character string.
}
\author{
  \adrian
}
\examples{
percentage(1/3)
percentage(runif(20) > 0.2)
}
\keyword{manip}
\keyword{utilities}
