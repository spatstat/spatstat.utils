\name{geomseq}
\alias{geomseq}
\title{
  Geometric Sequence 
}
\description{
  Generate a geometric sequence between two endpoints.
  The sequence is equally spaced on a logarithmic scale.
}
\usage{
geomseq(from, to, length.out)
}
\arguments{
  \item{from}{
    Starting value. A positive number.
  }
  \item{to}{
    Ending value. A positive number.
  }
  \item{length.out}{
    Number of elements in the sequence. A positive integer.
  }
}
\details{
  This is a wrapper for \code{\link[base]{seq.default}} which generates
  a geometric sequence between the two endpoints. 
}
\value{
  Numeric vector.
}
\author{
  \spatstatAuthors.
}
\seealso{
  \code{\link[base]{seq.default}}
}
\examples{
  geomseq(1, 32, length.out=6)
}
\keyword{datagen}
