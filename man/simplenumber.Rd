\name{simplenumber}
\alias{simplenumber}
\title{
  Simple Rational Number
}
\description{
  Given a numeric value, try to express it
  as a simple rational number.
}
\usage{
simplenumber(x, unit = "", multiply = "*", tol = .Machine$double.eps)
}
\arguments{
  \item{x}{
    A single numeric value.
  }
  \item{unit}{
    Optional. Character string giving the name of the unit 
    in which \code{x} is expressed. Typically an irrational number
    such as \code{pi}. See Examples.
  }
  \item{multiply}{
    Optional. Character string representing multiplication.
  }
  \item{tol}{
    Numerical tolerance.
  }
}
\details{
  The code tries to express \code{x} as an integer \code{x=n}, or as
  the reciprocal of an integer \code{x=1/n}, or
  as a simple rational number \code{x = m/n}, where \code{m,n} are
  small integers.
}
\value{
  A character string representing the simple number,
  or \code{NULL} if not successful.
}
\author{
  \adrian.
}
\examples{
  simplenumber(0.3)
  simplenumber(0.333333333333333333333333)
  x <- pi * 2/3
  simplenumber(x/pi, "pi")
}
\keyword{symbolmath}
