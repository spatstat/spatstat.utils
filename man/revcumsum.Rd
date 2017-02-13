\name{revcumsum}
\alias{revcumsum}
\title{
  Reverse Cumulative Sum
}
\description{
  Returns a vector of cumulative sums of the input values,
  running in reverse order. That is, the \code{i}th entry in the output
  is the sum of entries \code{i} to \code{n} in the input,
  where \code{n} is the length of the input.
}
\usage{
revcumsum(x)
}
\arguments{
  \item{x}{
    A numeric or complex vector. 
  }
}
\details{
  This low-level utility function is a faster alternative to
  \code{\link[base]{rev}(\link[base]{cumsum}(\link[base]{rev}(x)))}
  under certain conditions.
  It computes the reverse cumulative sum of the entries of \code{x}.
  If \code{y <- revcumsum(x)}, then \code{y[i] = sum(x[i:n])} where
  \code{n = length(x)}.

  This function should not be used if \code{x} could contain \code{NA}
  values: this would lead to an error.
}
\value{
  A vector of the same length and type as \code{x}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link[base]{cumsum}}.
}
\examples{
  revcumsum(1:5)
  rev(cumsum(rev(1:5)))
  x <- runif(1e6)
  system.time(rev(cumsum(rev(x))))
  system.time(revcumsum(x))
}
\keyword{arith}
\keyword{utilities}
