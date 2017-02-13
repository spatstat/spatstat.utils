\name{articlebeforenumber}
\alias{articlebeforenumber}
\title{
  Indefinite Article Preceding A Number
}
\description{
  Determines the indefinite article (\emph{an} or \code{a}) which should
  precede a given number, if the number is read out in English.
}
\usage{
articlebeforenumber(k)
}
\arguments{
  \item{k}{A single integer.}
}
\details{
  This function applies the rule that,
  if the English word for the number \code{k} begins with a vowel, then
  it should be preceded by \code{an}, and otherwise by \code{a}.
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
