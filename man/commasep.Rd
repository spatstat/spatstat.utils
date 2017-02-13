\name{commasep}
\alias{commasep}
\title{
  List of Items Separated By Commas
}
\description{
  Convert the elements of a vector into character strings
  and paste them together, separated by commas.
}
\usage{
commasep(x, join = " and ", flatten = TRUE)
}
\arguments{
  \item{x}{
    Vector of items in the list.
  }
  \item{join}{
    The string to be used to separate the last two items in the list.
  }
  \item{flatten}{
    Logical value indicating whether to return a single character string
    (\code{flatten=TRUE}, the default) or a list (\code{flatten=FALSE}).
  }
}
\value{
  A character string (if \code{flatten=TRUE}, the default)
  or a list of character strings. 
}
\author{
  \adrian.
}
\examples{
   commasep(letters[1:4])
   y <- commasep(sQuote(letters[1:4]))
   cat(y, fill=TRUE) 
}
\keyword{utilities}
\keyword{manip}
