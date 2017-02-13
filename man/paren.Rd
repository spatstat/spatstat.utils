\name{paren}
\alias{paren}
\alias{unparen}
\title{
  Add or Remove Parentheses
}
\description{
  Add or remove enclosing parentheses around a string.
}
\usage{
paren(x, type = "(")
unparen(x)
}
\arguments{
  \item{x}{
    A character string, or vector of character strings.
  }
  \item{type}{
    Type of parentheses: either \code{"("}, \code{"["} or \code{"{"}.
  }
}
\details{
  \code{paren(x)} adds enclosing parentheses to the beginning and end of
  the string \code{x}.

  \code{unparen(x)} removes enclosing parentheses if they are present.
}
\value{
  A character string, or vector of character strings of the same length
  as \code{x}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{commasep}}
}
\examples{
  paren("Hello world")
  paren(42, "[")
  paren(letters[1:10])
  unparen(c("(yes)", "[no]", "{42}"))
}
\keyword{manip}
\keyword{utilities}
