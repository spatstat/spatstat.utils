\name{harmonicmean}
\alias{harmonicmean}
\alias{harmonicsum}
\title{
  Harmonic Mean
}
\description{
  Calculates the harmonic mean of a numeric vector,
  robustly handling special cases.
}
\usage{
harmonicmean(x, na.rm = TRUE)
harmonicsum(x, na.rm = TRUE)
}
\arguments{
  \item{x}{
    Numeric vector.
  }
  \item{na.rm}{
    Logical value specifying whether to remove \code{NA} values
    before processing.
  }
}
\details{
  The harmonic mean of a set of numbers is the reciprocal
  of the mean of the reciprocals of the numbers.

  The function \code{harmonicmean} calculates the harmonic mean
  of \code{x}. The algorithm robustly handles special cases
  where some of the values in \code{x} are very small or are
  exactly equal to zero.

  The function \code{harmonicsum} calculates the reciprocal of the
  \emph{sum} of the reciprocals of the \code{x} values.
}
\value{
  A single numeric value.
}
\author{
  \adrian.
}
\examples{
   harmonicmean(1:3)
}
\keyword{arith}
