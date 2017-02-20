\name{check.nmatrix}
\alias{check.nmatrix}
\title{
  Check for Numeric Matrix with Correct Dimensions
}
\description{
  This is a programmer's utility function
  to check whether the argument is a numeric
  vector of the correct length.
}
\usage{
check.nmatrix(m, npoints = NULL, fatal = TRUE, things = "data points",
              naok = FALSE, squarematrix = TRUE, matchto = "nrow", warn = FALSE)
}
\arguments{
  \item{m}{
    The argument to be checked.
  }
  \item{npoints}{
    The required number of rows and/or columns for the matrix \code{m}.
  }
  \item{fatal}{
    Logical value indicating whether to stop with an error message
    if \code{m} does not satisfy all requirements.
  }
  \item{things}{
    Character string describing what the rows/columns of \code{m} should
    correspond to.
  }
  \item{naok}{
    Logical value indicating whether \code{NA} values are permitted.
  }
  \item{squarematrix}{
    Logical value indicating whether \code{m} must be a square matrix.
  }
  \item{matchto}{
    Character string (either \code{"nrow"} or \code{"ncol"})
    indicating whether it is the rows or the columns
    of \code{m} which must correspond to \code{npoints}. 
  }
  \item{warn}{
    Logical value indicating whether to issue a warning
    if \code{v} does not satisfy all requirements.
  }
}
\details{
  This programmer's utility function checks whether \code{m} is a numeric matrix
  of the correct dimensions, and checks for \code{NA} values.
  If \code{matchto="nrow"} (the default) then
  the number of rows of \code{m} must be equal to \code{npoints}.
  If \code{matchto="ncol"} then the number of columns of \code{m}
  must be equal to \code{npoints}. If \code{squarematrix=TRUE} (the
  default) then the numbers of rows and columns must be equal.
  If \code{naok = FALSE} (the default) then the entries of \code{m}
  must not include \code{NA}.

  If these requirements are all satisfied, the result is the logical
  value \code{TRUE}.

  If not, then if \code{fatal=TRUE} (the default), an error occurs;
  if \code{fatal=FALSE}, the result is the logical value \code{FALSE}
  with an attribute describing the requirement that was not satisfied.
}
\value{
  A logical value indicating whether all the requirements were
  satisfied. 
}
\author{
  \adrian.
}
\seealso{
  \code{\link{check.nvector}}
}
\examples{
  z <- matrix(1:16, 4, 4)
  check.nmatrix(z, 4)
}
\keyword{error}
\keyword{utilities}
