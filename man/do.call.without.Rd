\name{do.call.without}
\alias{do.call.without}
\title{
  Call a Function, Omitting Certain Arguments
}
\description{
  Call a specified function, omitting some arguments which are
  inappropriate to the function.
}
\usage{
  do.call.without(fun, \dots, avoid)
}
\arguments{
  \item{fun}{
    The function to be called. A function name, a
    character string giving the name of the function,
    or an expression that yields a function.
  }
  \item{\dots}{
    Any number of arguments.
  }
  \item{avoid}{
    Vector of character strings, giving the names of arguments that should
    \emph{not} be passed to \code{fun}.
  }
}
\details{
  This is a simple mechanism for preventing some arguments from being 
  passed in a function call. The arguments \code{\dots} are collected in
  a list. A argument is omitted if its name exactly matches
  one of the strings in \code{avoid}. 
}
\value{
  The return value of \code{fun}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{do.call.matched}} for a more complicated and flexible call.
}
\examples{
  do.call.without(paste, 1, 2, z=3, w=4, avoid="z")
}
\keyword{programming}
\keyword{utilities}

