\name{check.1.integer}
\alias{check.1.integer}
\alias{check.1.real}
\alias{check.1.string}
\title{
  Check Argument Type and Length
}
\description{
  These utility functions check whether a given argument is a single value
  of the required type.
}
\usage{
check.1.real(x, context = "", fatal = TRUE)
check.1.integer(x, context = "", fatal = TRUE)
check.1.string(x, context = "", fatal = TRUE)
}
\arguments{
  \item{x}{
    The argument to be checked.
  }
  \item{context}{
    Optional string specifying the context in which the argument
    is checked.
  }
  \item{fatal}{
    Logical value indicating what to do if \code{x} is not of the
    required type.
  }
}
\details{
  These functions check whether the argument \code{x} is a single
  atomic value of type \code{numeric}, \code{integer} or
  \code{character}.

  If \code{x} does have the required length and type, the result
  of the function is the logical value \code{TRUE}.

  If \code{x} does not have the required length and type,
  then if \code{fatal=TRUE} (the default) an error occurs,
  while if \code{fatal=FALSE} a warning is issued and the
  function returns the value \code{FALSE}.
}
\value{
  A logical value (or an error may occur).
}
\author{
  \adrian.
}
\seealso{
  \code{\link{check.named.vector}}
}
\examples{
  x <- pi
  check.1.real(x)
  check.1.integer(x, fatal=FALSE)
  check.1.string(x, fatal=FALSE)
}
\keyword{classes}
\keyword{error}
