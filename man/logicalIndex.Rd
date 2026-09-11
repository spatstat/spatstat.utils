\name{logicalIndex}
\alias{logicalIndex}
\title{
  Convert Any Kind of Index to a Logical Vector
}
\description{
  Converts any kind of index to an logical vector.
}
\usage{
logicalIndex(i,  nama=NULL, len = length(nama))
}
\arguments{
  \item{i}{
    Any kind of index for a vector.
  }
  \item{nama}{
    Optional. Character vector of names for the vector being indexed.
  }
  \item{len}{
    Integer. Length of the vector being indexed.
  }
}
\details{
  This utility function interprets any kind of index for a vector
  and converts it to a logical vector.
  
  Suppose that \code{x} is a vector of length \code{len}
  with names \code{names(x) = nama}. Then \R allows the vector \code{x}
  to be indexed as \code{x[i]} where \code{i} may be
  \itemize{
    \item a vector of positive integers between 1 and \code{len}
    specifying the elements to be included;
    \item a vector of negative integers between \code{-1} and
    \code{-len} specifying the elements to be omitted;
    \item a logical vector of length \code{len} specifying whether each
    element of \code{x} is to be included (\code{TRUE}) or excluded
    (\code{FALSE});
    \item a character vector containing elements of
    \code{nama} identifying the elements to be included;
    \item missing (equivalent to specifying all elements of \code{x}).
  }
  The function \code{logicalIndex} interprets any such index \code{i}
  and converts it to a logical vector.

  If argument \code{i} is missing or \code{NULL},
  it is interpreted as a missing index,
  and the result is a vector of \code{TRUE} values.
}
\section{Entries outside the range}{
  Values of \code{i} which imply a position outside the
  range from \code{1} to \code{len} are ignored.
  Values of \code{i} which are \code{NA} are also ignored.
}
\value{
  A logical vector.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{positiveIndex}}
}
\examples{
   logicalIndex(2:4, len=5)
   logicalIndex(-3, len=5)
   logicalIndex(letters[2:3], letters[1:5])
   logicalIndex(len=5)
}
\keyword{manip}
