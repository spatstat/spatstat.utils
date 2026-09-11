\name{positiveIndex}
\alias{positiveIndex}
\title{
  Convert Any Kind of Index to an Integer Vector.
}
\description{
  Converts any kind of index to an integer vector of indices.
}
\usage{
positiveIndex(i, nama=NULL, len = length(nama))
}
\arguments{
  \item{i}{
    Any valid kind of index for a vector.
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
  and converts it to a positive integer index.
  
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
  The function \code{positiveIndex} interprets any such index \code{i}
  and converts it to the first type of index
  (a vector of positive integers specifying the elements to be
  included).

  If argument \code{i} is missing or \code{NULL},
  it is interpreted as a missing index,
  and the result is the full sequence from 1 to \code{len}.
}
\section{Entries outside the range}{
  Values in the argument \code{i}
  which imply a position outside the range
  from \code{1} to \code{len} are returned as \code{NA}.
}
\value{
  An integer vector.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{logicalIndex}}
}
\examples{
   positiveIndex(2:4, len=5)
   positiveIndex(-3, len=5)
   positiveIndex(letters[4:6], letters)
   positiveIndex(len=5)
}
\keyword{manip}
