\name{verbalogic}
\alias{verbalogic}
\title{
  Verbal Logic
}
\description{
  Perform the specified logical operation
  on the character vector \code{x}, recognising the
  special strings \code{"TRUE"} and \code{"FALSE"}
  and treating other strings as logical variables.
}
\usage{
verbalogic(x, op = "and")
}
\arguments{
  \item{x}{
     Character vector.
  }
  \item{op}{
    Logical operation: one of the character strings
    \code{"and"}, \code{"or"} or \code{"not"}.
  }
}
\details{
  This function performs simple logical operations
  on character strings that represent human-readable statements.

  The character vector \code{x} may contain any strings:
  the special strings \code{"TRUE"} and \code{"FALSE"} are
  treated as the logical values \code{TRUE} and \code{FALSE},
  while all other strings are treated as if they were
  logical variables.

  If \code{op="and"}, the result is a single string,
  logically equivalent to \code{x[1] && x[2] && ... && x[n]}.
  First, any entries of \code{x} equal to \code{"TRUE"} are removed.
  The result is \code{"FALSE"} if any of the entries of \code{x}
  is \code{"FALSE"}; otherwise it is equivalent to
  \code{paste(x, collapse=" and ")}.

  If \code{op="or"}, the result is a single string, logically equivalent to
  \code{x[1] || x[2] || ... || x[n]}. 
  First, any entries of \code{x} equal to \code{"FALSE"} are removed.
  The result is \code{"TRUE"} if any of the entries of \code{x}
  is \code{"TRUE"}; otherwise it is equivalent to
  \code{paste(x, collapse=" or ")}.

  If \code{op="not"}, the result is a character vector \code{y}
  such that \code{y[i]} is the logical negation of \code{x[i]}.

  The code does not understand English grammar and cannot expand logical
  expressions.
}
\value{
  A character string.
}
\author{
  \adrian.
}
\examples{
   x <- c("The sky is blue", "my name is not Einstein", "FALSE")
   verbalogic(x, "and")
   verbalogic(x, "or")
   verbalogic(x, "not")
}
\keyword{logic}
\keyword{manip}
