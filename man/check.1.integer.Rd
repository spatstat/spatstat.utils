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
check.1.real(x, context = "", fatal = TRUE, warn=TRUE)
check.1.integer(x, context = "", fatal = TRUE, warn=TRUE)
check.1.string(x, context = "", fatal = TRUE, warn=TRUE)
}
\arguments{
  \item{x}{
    The argument to be checked.
  }
  \item{context}{
    Optional string describing the context in which the argument
    is checked.
  }
  \item{fatal}{
    Logical value indicating whether a fatal error should occur when
    \code{x} is not of the required type. 
  }
  \item{warn}{
    Logical value indicating whether to issue a warning message
    if \code{x} is not of the required type. 
  }    
}
\details{
  These functions check whether the argument \code{x} is a single
  atomic value of type \code{numeric}, \code{integer} or
  \code{character}.

  If \code{x} does have the required length and type, the result
  of the function is the logical value \code{TRUE}.

  Otherwise, if \code{fatal=TRUE} (the default) an error occurs,
  while if \code{fatal=FALSE} a warning is issued (if \code{warn=TRUE})
  and the function returns the value \code{FALSE}.
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
  check.1.integer(pi, fatal=FALSE, context="In your dreams,")
  check.1.string(x, fatal=FALSE)
  check.1.integer(x, fatal=FALSE, warn=FALSE)
}
\keyword{classes}
\keyword{error}
