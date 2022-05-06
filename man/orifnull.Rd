\name{orifnull}
\alias{\%orifnull\%}  %DoNotExport
%NAMESPACE export("%orifnull%")
\title{Specify a Default Value}
\description{
  Specify a value together with a default to be used when the first value is null.
}
\usage{
a \%orifnull\% b
}
\arguments{
  \item{a}{
    Any kind of object or expression to be evaluated.
  }
  \item{b}{
    Default value to be used when \code{a} is \code{NULL}.
    Any kind of object or expression to be evaluated.
  }
}
\value{
  The result is \code{a} if \code{a} is not \code{NULL},
  and otherwise the result is \code{b}.
}
\details{
  The operator \code{\%orifnull\%} is designed to improve the
  readability of code.

  \code{a \%orifnull\% b} is equivalent to \code{if(is.null(a)) a else b}.

  That is, \code{a \%orifnull\% b} is equal to 
  \code{a} provided \code{a} is not null, and otherwise
  the result is equal to \code{b}.  

  Expressions are evaluated only when necessary. 
  If \code{a} is a language expression, it is first evaluated. Then if
  \code{is.null(a)} is \code{FALSE}, the result is \code{a}. Otherwise,
  \code{b} is evaluated, and the result is \code{b}.
  Note that \code{b} is not evaluated unless \code{a} is \code{NULL}.

  The operator \code{\%orifnull\%} has higher precedence than the arithmetic
  operators \code{+}, \code{-}, \code{*}, \code{/} but lower precedence than \code{^}.

  The operator is associative, and can be used repeatedly in an
  expression, so that a default value may have its own default. 
  See the Examples.
}
\examples{
   x <- 7
   y <- 42
   z <- w <- NULL
   x \%orifnull\% y
   z \%orifnull\% y
   z \%orifnull\% x \%orifnull\% y
   z \%orifnull\% w \%orifnull\% y
}
\author{
  \spatstatAuthors
}
\keyword{manip}
