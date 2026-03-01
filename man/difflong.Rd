\name{difflong}
\alias{difflong}
\title{
  Lagged Differences for a Long Vector
}
\description{
  Calculate the lagged difference of a long vector.
}
\usage{
difflong(x, drop = TRUE)
}
\arguments{
  \item{x}{
    Numeric or integer vector.
  }
  \item{drop}{
    Logical value. See Details.
  }
}
\details{
  A \sQuote{long vector} is a vector with more than
  \eqn{2^{31} - 1}{2^31 - 1} elements (the largest possible 32-bit integer).
  Long vectors are supported in \R on 64-bit computer systems.
  This feature was introduced in \R version 3.0.0.
  
  Although long vectors are permitted, not all functions in \R
  have been extended to handle long vectors.

  The base \R function \code{\link{diff}} currently does not handle
  a long vector, and may cause the entire \R
  session to be terminated.

  The function \code{difflong} is a temporary replacement
  for \code{\link{diff}} in the simplest case:
  \code{difflong(x)} is a replacement for \code{diff(x)}.

  If \code{drop=TRUE} (the default), 
  the result of \code{difflong(x)} is equivalent to \code{diff(x)},
  a vector with length equal to \code{length(x) - 1}.
  If \code{drop=FALSE} the result of \code{difflong(x, FALSE)} has the
  same length as \code{x}; the first entry is zero, and the subsequent
  entries are equivalent to \code{diff(x)}.
}
\value{
  A vector of the same type as \code{x}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{diff}}
}
\examples{
  x <- sample(1:5)
  diff(x)
  difflong(x)
}
\keyword{manip}
