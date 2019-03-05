\name{taperoff}
\alias{taperoff}
\title{
  Taper Functions
}
\description{
  Computes a function that tapers smoothly from 0 to 1.
}
\usage{
taperoff(x, zeropoint = 0, onepoint = 1,
         type = c("smooth", "cosine", "Gaussian"))
}
\arguments{
  \item{x}{
    Function argument. A number or a numeric vector.
  }
  \item{zeropoint}{
    Value of \code{x} that should return a function value of 0.
  }
  \item{onepoint}{
    Value of \code{x} that should return a function value of 1.
  }
  \item{type}{
    Character string (partially matched) specifying which taper function
    to use.
  }
}
\details{
  A taper is a mathematical function that exhibits a gradual transition between
  the values 0 and 1.

  By default, the function value \eqn{f(x)} is equal to 0
  if \eqn{x \le 0}, is equal to 1 if \eqn{x \ge 1},
  and lies between 0 and 1 when \eqn{0 < x < 1}.

  If \code{type="cosine"}, the function is the cosine taper
  \eqn{f(x) = (1 - \cos(\pi x))/2}{f(x) = 1 - cos(\pi * x)/2}.
  
  If \code{type="smooth"} the function is the smooth partition of unity
  \eqn{f(x) = \theta(x)/(\theta(x) + \theta(1-x))}
  where \eqn{\theta(x) = \exp(-1/x)}{\theta(x) = exp(-1/x)}.

  If \code{type="Gaussian"} the function is the cumulative distribution
  function of the Gaussian (normal) distribution with mean \eqn{1/2}
  and standard deviation \eqn{1/6}.
  
  If \code{zeropoint} and \code{onepoint} are specified, then
  the function value is equal to 0 when
  \code{x=zeropoint}, equal to 1 when \code{x=onepoint}, and lies
  between 0 and 1 when \code{x} lies between \code{zeropoint} and
  \code{onepoint}.
}
\value{
  A numeric vector of the same length as \code{x}.
}
\author{
  Adrian Baddeley
}
\examples{
  curve(taperoff(x, type="smooth"))
  curve(taperoff(x, type="cosine"), add=TRUE, col="green")
  curve(taperoff(x, type="Gaussian"), add=TRUE, col="blue")
}
\keyword{math}
