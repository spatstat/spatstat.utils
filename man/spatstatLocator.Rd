\name{spatstatLocator}
\alias{spatstatLocator}
\title{
  Graphical Input
}
\description{
  This is an alternative to the \code{\link[graphics]{locator}}
  function. It contains a workaround for a bug that occurs in
  \code{RStudio}.
}
\usage{
spatstatLocator(n, type = c("p", "l", "o", "n"), \dots)
}
\arguments{
  \item{n}{
    Optional. Maximum number of points to locate.
  }
  \item{type}{
    Character specifying how to plot the locations.
    If \code{"p"} or \code{"o"} the points are plotted;
    if \code{"l"} or \code{"o"} they are joined by lines.
  }
  \item{\dots}{
    Additional graphics parameters used to plot the locations.
  }
}
\details{
  This is a replacement/workaround for the \code{\link{locator}}
  function in some versions of \pkg{RStudio} which do not seem to
  recognise the option \code{type="p"}.

  See \code{\link[graphics]{locator}} for a description of the
  behaviour.
}
\section{Software Testing}{
  Programmers may like to know that
  code which depends on \code{spatstatLocator}
  can be tested in a non-interactive session,
  if the coordinates are previously queued using
  \code{\link{queueSpatstatLocator}}.
}
\value{
  A list containing components \code{x} and \code{y} which are vectors
  giving the coordinates of the identified points in the
  user coordinate system, i.e., the one specified by \code{par("usr")}.
}
\author{
  \spatstatAuthors.
}
\seealso{
  \code{\link[graphics]{locator}}.

  \code{\link{queueSpatstatLocator}}
}
\examples{
   if(interactive()) locator(1, type="p")
}
\keyword{iplot}
