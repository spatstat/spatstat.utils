\name{ordinal}
\alias{ordinal}
\alias{ordinalsuffix}
\title{
  Ordinal Numbers
}
\description{
  Returns the appropriate abbreviation in English for an ordinal number
  (for example \code{ordinal(5)} is \code{"5th"}).
}
\usage{
ordinal(k)
ordinalsuffix(k)
}
\arguments{
  \item{k}{An integer or vector of integers.}
}
\details{
  \code{ordinal(k)} returns a character string representing the
  \code{k}th ordinal number. \code{ordinalsuffix(k)} determines the
  appropriate suffix.

  The suffix can be either \code{"st"} (abbreviating
  \emph{first}), \code{"nd"} (abbreviating \emph{second}),
  \code{"rd"} (abbreviating \emph{third}) or
  \code{"th"} (for all other ordinal numbers \code{fourth},
  \code{fifth}, etc).
}
\value{
  A character string or character vector of the same length as \code{k}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{articlebeforenumber}}
}
\examples{
  ordinal(1:7)
  cat(paste("Happy", ordinal(21), "Birthday"), fill=TRUE)
}
\keyword{manip}
\keyword{utilities}
