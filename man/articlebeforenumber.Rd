\name{articlebeforenumber}
\alias{articlebeforenumber}
\title{
  Indefinite Article Preceding A Number
}
\description{
  Determines the indefinite article (\emph{an} or \emph{a}) which should
  precede a given number, if the number is read out in English.
}
\usage{
articlebeforenumber(k, teenhundreds=FALSE)
}
\arguments{
  \item{k}{A single number.}
  \item{teenhundreds}{
    Logical value specifying that (for example)
    1800 should be read as \dQuote{eighteen hundred}
    instead of \dQuote{one thousand eight hundred}.
    See Details.
  }
}
\details{
  This function applies the rule that,
  if the English language word or phrase for the number \code{k}
  begins with a vowel, then it should be preceded by \emph{an},
  and otherwise by \emph{a}.

  If \code{teenhundreds=FALSE} (the default),
  the numbers 1100 and 1800 will be read as
  \sQuote{one thousand one hundred} and \sQuote{one thousand eight hundred},
  and the indefinite article will be \emph{a}.
  However if \code{teenhundreds=TRUE}, the numbers 1100 and 1800 
  be read as \sQuote{eleven hundred} and \sQuote{eighteen hundred}
  and the indefinite article will be \emph{an}.
}
\value{
  One of the character strings \code{"an"} or \code{"a"}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{ordinal}}
}
\examples{
  f <- function(k) cat(paste(articlebeforenumber(k),
                       paste0(k, "-fold"),
                       "increase\n"))
  f(8)
  f(18)
  f(28)
}
\keyword{manip}
\keyword{utilities}
