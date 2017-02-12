\name{do.call.matched}
\alias{do.call.matched}
\title{
  Call a Function, Passing Only Recognised Arguments
}
\description{
  Call a specified function, using only those arguments which are
  known to be acceptable to the function.
}
\usage{
  do.call.matched(fun, arglist, funargs, extrargs = NULL,
                  matchfirst = FALSE, sieve = FALSE, skipargs = NULL)
}
\arguments{
  \item{fun}{
    A function, or a character string giving the name of a function,
    to be called.
  }
  \item{arglist}{
    A named list of arguments.
  }
  \item{funargs}{
    Character vector giving the names of arguments that are recognised
    by \code{fun}. Defaults to the names of the formal arguments of
    \code{fun}. 
  }
  \item{extrargs}{
    Optional. Character vector giving the names of additional arguments
    that can be handled by \code{fun}. 
  }
  \item{skipargs}{
    Optional. Character vector giving the names of arguments which should
    \bold{not} be passed to \code{fun}.
  }
  \item{matchfirst}{
    Logical value indicating whether the first entry
    of \code{arglist} is permitted to have an empty name
    and should be matched to the first argument of \code{fun}.
  }
  \item{sieve}{
    Logical value indicating whether to return the
    un-used arguments as well as the result of the function call.
    See Details.
  }
}
\details{
  This function is a wrapper for \code{\link[base]{do.call}}
  which avoids passing arguments that are unrecognised by \code{fun}.

  In the simplest case \code{do.call.matched(fun, arglist)}
  is like \code{do.call(fun, arglist)}, except that
  entries of \code{arglist} which do not match any formal
  argument of \code{fun} are removed.
  Extra argument names can be permitted using \code{extrargs},
  and argument names can be forbidden using \code{skipargs}.
}
\value{
  If \code{sieve=FALSE} (the default), the result is the
  return value from \code{fun}.

  If \code{sieve=TRUE}, the result is a list with entries
  \code{result} (the return value from \code{fun}) and
  \code{otherargs} (a list of the arguments that were not passed
  to \code{fun}).
}
\author{
  \adrian
}
\seealso{
  \code{\link{resolve.defaults}},
  \code{\link{do.call.without}}.
  
  \code{\link[base]{do.call}}
}
\examples{
  f <- function(x=0,y=0, ...) { paste(x, y, ..., sep=", ") }
  f()
  do.call.matched(f, list(y=2))
  do.call.matched(f, list(y=2, z=5), extrargs="z")
  do.call.matched(f, list(y=2, z=5), extrargs="z", skipargs="y")
}
\keyword{programming}
\keyword{utilities}
