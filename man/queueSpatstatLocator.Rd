\name{queueSpatstatLocator}
\alias{queueSpatstatLocator}
\title{
  Add Coordinates to a Queue for Use by Locator Function
}
\description{
  Add the coordinates of a spatial location to a queue.
  The queue can be accessed by the \code{spatstatLocator} function
  in a non-interactive session.
}
\usage{
queueSpatstatLocator(x, y)
}
\arguments{
  \item{x,y}{
    Numeric values, or vectors of the same length,
    containing spatial coordinates. Any data acceptable to
    \code{\link[grDevices]{xy.coords}}.
  }
}
\details{
  The \code{\link{spatstatLocator}} function is a replacement
  for the \code{\link[graphics]{locator}} function
  that can be used to test software which depends on user input.

  When \code{queueSpatstatLocator(x,y)} is called, the coordinate
  data \code{x,y} are saved in a queue. The first-listed coordinate
  pair \code{x[1], y[1]} is at the front of the queue. Subsequently, when
  \code{\link{spatstatLocator}} is called, the coordinates are
  taken from the front of the queue and returned as if they had been
  clicked by the user.

  This only works in a \bold{non-}interactive
  session, that is, when \code{\link[base]{interactive}()} returns \code{FALSE}.
}
\value{
  Integer (invisible).
  The length of the queue, after inclusion of the new points.
}
\author{
  \spatstatAuthors.
}
\seealso{
\code{\link{spatstatLocator}}
}
\examples{
queueSpatstatLocator(0.5, 0.7)
queueSpatstatLocator(c(0.3, 0.4), c(0.2, 0.9))
if(!interactive()) {
   spatstatLocator(2)   
   spatstatLocator(1)
}
}
\keyword{iplot}
