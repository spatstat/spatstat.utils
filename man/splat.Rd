\name{splat}
\alias{splat}
\title{
  Print Text Within Margins
}
\description{
  Prints a given character string or strings
  inside the text margin specified by \code{options("width")}.
  Indents the text if required.
}
\usage{
splat(\dots, indent = 0)
}
\arguments{
  \item{\dots}{
    Character strings, or other arguments acceptable to
    \code{\link[base]{paste}}.
  }
  \item{indent}{
    Optional. Indentation of the text.
    Either an integer specifying the number of character positions
    by which the text should be indented, or a character string whose
    length determines the indentation.
  }
}
\details{
  \code{splat} stands for \sQuote{split cat}.

  The command \code{splat(\dots)} is like \code{cat(paste(\dots))}
  except that the output will be split into lines that can be
  printed within the current text margin
  specified by \code{\link[base]{getOption}("width")}.

  The arguments \code{\dots} are first combined into a character vector
  using \code{\link[base]{paste}}. Then they are split into words
  separated by white space. A newline will be inserted whenever the next
  word does not fit in the available text area.
  (Words will not be broken, so the text margin could be exceeded
  if any word is longer than \code{\link[base]{getOption}("width")}).

  If any argument is a vector, each
  element of the vector is treated as a separate line.
  Existing newline characters in \code{\dots} are also respected.
}
\value{
Null.
}
\author{
  \spatstatAuthors.
}
\examples{
  op <- options(width=20)
  splat("There is more than one way to skin a cat.")
  splat("There is more than one", "way to skin a cat.", indent=5)

  options(width=10)
  splat("The value of pi is", pi)
  splat("The value of pi is", signif(pi))
  options(op)
}
\keyword{print}
\keyword{utilities}
