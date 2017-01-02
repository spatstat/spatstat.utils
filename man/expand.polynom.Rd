\name{expand.polynom}
\alias{expand.polynom}
\alias{sympoly}
\title{
  Expand Symbolic Polynomials in a Formula
}
\description{
  Create a formula representing a polynomial,
  or expand polynomials in an existing formula.
}
\usage{
  expand.polynom(f)
  sympoly(x, y, n)
}
\arguments{
  \item{f}{
    A formula.
  }
  \item{x,y}{
    Variable names.
  }
  \item{n}{
    Integer specifying the degree of the polynomial.
    (If \code{n} is missing, \code{y} will be interpreted as the degree.)
  }
}
\details{
  These functions expand a polynomial into its homogeneous terms
  and return a model formula.

  \code{sympoly(x, n)} creates a formula whose right-hand side represents the
  polynomial of degree \code{n} in the variable \code{x}. Each
  homogeneous term \code{x^k} is a separate term in the formula.

  \code{sympoly(x, y, n)} creates a formula representing the
  polynomial of degree \code{n} in the two variables \code{x} and
  \code{y}.

  If \code{f} is a formula containing a term of the form
  \code{polynom(\dots)} then \code{expand.polynom(f)} replaces this term
  by its expansion as a sum of homogeneous terms.
}
\value{
  A \code{formula}.
}
\author{
  \spatstatAuthors.
}
\seealso{
   \code{\link[spatstat]{polynom}}
}
\examples{
   sympoly(A, 4)
   sympoly(A, B, 3)
   expand.polynom(U ~ A + polynom(B, 2))
}
\keyword{utilities}
