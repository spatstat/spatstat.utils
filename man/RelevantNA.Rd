\name{RelevantNA}
\alias{RelevantNA}
\alias{RelevantEmpty}
\alias{RelevantZero}
\alias{isRelevantZero}
\title{
  Missing Value, Zero-length Vector, or Zero Value of the Appropriate Type
}
\description{
  Given any data \code{x}, these functions return 
  the missing value \code{NA}, the empty vector, or the
  equivalent of the number \code{0},
  with the same type as \code{x}.
}
\usage{
RelevantZero(x)
RelevantNA(x)
RelevantEmpty(x)

isRelevantZero(x)
}
\arguments{
  \item{x}{
    Data of any type.
  }
}
\details{
  In the \R system, missing values may have different types.
  For example, if an entry is missing from a numeric vector, it is a
  missing numeric value, not a missing logical value, and \R
  distinguishes between these two types of missing values.

  The function \code{RelevantNA} returns a missing value of the same
  type as the input \code{x} (as defined by \code{\link{typeof}}).
  Thus, \code{RelevantNA(3.2)} returns a missing numeric value
  and \code{RelevantNA(TRUE)} returns a missing logical value.
  
  \code{RelevantEmpty(x)} returns a vector of length zero which has the
  same type as \code{x}. Thus, \code{RelevantEmpty(TRUE)} is equivalent
  to \code{logical(0)}.
  
  \code{RelevantZero(x)} returns a single value, of the same type as
  \code{x}, that is equivalent to the number zero. For example,
  \code{RelevantZero(TRUE)} returns \code{FALSE}.

  The function \code{isRelevantZero} tests whether \code{x} is a single
  zero value, by testing whether \code{x} is identical to
  \code{RelevantZero(x)}. 
}
\value{
  \code{RelevantZero} and \code{RelevantNA} return a single value
  of the same type as \code{x}.

  \code{RelevantEmpty} returns a zero-length vector of the same type
  as \code{x}.

  \code{isRelevantZero} returns a single logical value.
}
\author{
  \adrian.
}
\seealso{\code{\link{typeof}}}
\examples{
   RelevantZero(42)
   RelevantZero(TRUE)
   RelevantZero("hello world")

   RelevantNA(1:3)
   typeof(RelevantNA(1:3))
   typeof(RelevantNA("hello world"))
}
\keyword{manip}
