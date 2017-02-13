\name{cat.factor}
\alias{cat.factor}
\title{
  Combine Several Factors
}
\description{
  Combine (concatenate) several factor objects, to produce a factor.
}
\usage{
cat.factor(\dots)
}
\arguments{
  \item{\dots}{
    Any number of arguments. Each argument should be a factor,
    or will be converted to a factor.
  }
}
\details{
  The arguments \code{\dots} are concatenated as they would be
  using \code{\link[base]{c}()} or \code{\link[base]{cat}()},
  except that factor levels are retained
  and merged correctly. See the Examples.
}
\value{
  A factor, whose length is the sum of the lengths of all
  arguments. The levels of the resulting factor are the union of the
  levels of the arguments.
}
\author{
  \rolf.
}
\seealso{
  \code{\link[base]{c}}.
}
\examples{
   f <- factor(letters[1:3])
   g <- factor(letters[3:5])
   f
   g
   cat(f,g)
   c(f,g)
   cat.factor(f, g)
}
\keyword{manip}
