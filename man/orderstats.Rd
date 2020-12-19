\name{orderstats}
\alias{orderstats}
\alias{orderwhich}
\title{
  Compute Order Statistics
}
\description{
  Compute the \code{k}-th smallest value in a dataset,
  or find which entry in a dataset is the \code{k}-th smallest.
}
\usage{
orderstats(x, k, decreasing = FALSE)
orderwhich(x, k, decreasing = FALSE)
}
\arguments{
  \item{x}{
    Data whose order statistics will be computed. A numeric vector.
  }
  \item{k}{
    Rank. An integer, or vector of integers.
  }
  \item{decreasing}{
    Logical value specifing whether a rank of 1
    is assigned to the highest value (\code{decreasing=TRUE})
    or the lowest value (\code{decreasing=FALSE}, the default).
  }
}
\details{
  These are low-level functions for efficiently computing order statistics:
  \code{orderstats(x, k)} returns the \code{k}-th smallest value in \code{x},
  and \code{orderwhich(x, k)} returns the \emph{position} of the
  \code{k}-th smallest value in \code{x}.

  Given a dataset of values \eqn{x_1, \dots, x_n}{x[1], ..., x[n]},
  the \emph{order statistic} of rank \eqn{k} is the \eqn{k}-th smallest
  value in the dataset. The order statistic of rank 1 is the smallest
  value, and the order statistic of rank \eqn{n} is the largest value.
  The order statistic of rank \eqn{k} is denoted \eqn{x_{[k]}}{x([k])}.

  The full sequence of order statistics
  \deqn{
    x_{[1]} \le x_{[2]} \le \dots \le x_{[n]}
  }{
    x([1]) <= x([2]) <= ... <= x([n])
  }
  can simply be obtained by sorting the original values
  into increasing order.

  The command \code{orderstats(x, k)} is equivalent to
  \code{\link{sort}(x)[k]}; it calculates the
  \code{k}-th smallest value in \code{x}. 

  The command \code{orderwhich(x, k)} is equivalent to
  \code{\link{order}(x)[k]}. It identifies the \emph{position} of the
  \code{k}-th smallest value in \code{x}, that is, it returns the
  index \code{j} such that \code{x[j]} is the \code{k}-th smallest value
  in \code{x}.
  
  The functions \code{orderstats} and \code{orderwhich} are more
  efficient than using \code{sort} and \code{order}
  when it is only desired to calculate a few of the
  order statistics (for example, only the smallest and second-smallest
  values in the dataset).
}
\value{
  \code{orderstats} returns a vector of the same kind as \code{x},
  with the same length as \code{k}.
  \code{orderwhich} returns an integer vector 
  with the same length as \code{k}.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{sort}}, \code{\link{order}}.
}
\examples{
  x <- runif(10)
  orderstats(x, 2)
  sort(x)[2]
  orderwhich(x, 2:3)
  order(x)[2:3]
}
\keyword{math}
