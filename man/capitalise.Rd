\name{capitalise}
\alias{capitalise}
\title{
  Make Each Word Start with a Capital Letter 
}
\description{
  Given a character string or vector of character strings containing words,
  change the start of each word to a capital letter.
}
\usage{
capitalise(s, strict = FALSE)
}
\arguments{
  \item{s}{
    Character string or vector of character strings.
  }
  \item{strict}{
    Logical. If \code{TRUE}, all subsequent letters after the first
    letter in each word will be coerced to lower case.
  }
}
\details{
  Each entry of \code{s} is split into separate words separated by
  the space character. Then the first letter of each word is coerced
  to a capital letter using \code{\link[base]{toupper}}, and if
  \code{strict=TRUE}, subsequent letters are coerced to lower case
  using \code{\link[base]{tolower}}. Then the words are replaced in
  their original positions in \code{s}.
}
\value{
  A character string or character vector of the same length as \code{s}.
}
\author{
  Originally documented in base \R. Code copied by \adrian.
}
\seealso{
  \code{\link[base]{toupper}}
}
\examples{
  a <-  c("worKers", "of tHe", "worLd unIte")
  capitalise(a)
  capitalise(a, TRUE)
}
\keyword{manip}
\keyword{utilities}
