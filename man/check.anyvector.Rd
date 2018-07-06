\name{check.anyvector}
\alias{check.anyvector}
\title{
  Check For Vector or Factor With Correct Length
}
\description{
  This is a programmer's utility function
  to check whether the argument is a 
  vector or factor of the correct length.
}
\usage{
     check.anyvector(v, npoints = NULL, fatal = TRUE, things = "data points",
                     naok = FALSE, warn = FALSE, vname, oneok = FALSE)
}
\arguments{
  \item{v}{
    The argument to be checked.
  }
  \item{npoints}{
    The required length of \code{v}.
  }
  \item{fatal}{
    Logical value indicating whether to stop with an error message
    if \code{v} does not satisfy all requirements.
  }
  \item{things}{
    Character string describing what the entries of \code{v} should
    correspond to.
  }
  \item{naok}{
    Logical value indicating whether \code{NA} values are permitted.
  }
  \item{warn}{
    Logical value indicating whether to issue a warning
    if \code{v} does not satisfy all requirements.
  }
  \item{vname}{
    Character string giving the name of \code{v} to be used in messages.
  }
  \item{oneok}{
    Logical value indicating whether \code{v} is permitted to have
    length 1.
  }
}
\details{
  This function checks whether \code{v} is a vector or factor with
  length equal to \code{npoints} (or length equal to 1 if
  \code{oneok=TRUE}), not containing any \code{NA} values (unless
  \code{naok=TRUE}).

  If these requirements are all satisfied, the result is the logical
  value \code{TRUE}.

  If not, then if \code{fatal=TRUE} (the default), an error occurs;
  if \code{fatal=FALSE}, the result is the logical value \code{FALSE}
  with an attribute describing the requirement that was not satisfied.
}
\value{
  A logical value indicating whether all the requirements were
  satisfied. If \code{FALSE}, then this value has an attribute
  \code{"whinge"}, a character string describing the requirements that
  were not satisfied.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{check.nvector}},
  \code{\link{check.named.vector}}.
}
\examples{
   z <- factor(1:10)
   check.anyvector(z, 5, fatal=FALSE)
   y <- z[1]
   check.anyvector(y, 5, oneok=TRUE)
}
\keyword{error}
\keyword{utilities}
