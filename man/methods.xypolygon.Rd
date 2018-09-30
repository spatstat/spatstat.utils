\name{methods.xypolygon}
\alias{methods.xypolygon} %DoNotExport
\alias{verify.xypolygon}
\alias{is.hole.xypolygon}
\alias{Area.xypolygon}       
\alias{bdrylength.xypolygon} 
\alias{reverse.xypolygon}    
\alias{overlap.xypolygon}    
\alias{simplify.xypolygon}
\alias{inside.xypolygon}     
\title{
  Calculations for Polygons in the Plane
}
\description{
  Compute the area or boundary length of a polygon,
  determine whether a point falls inside a polygon,
  compute the area of overlap between two polygons,
  and related tasks.
}
\usage{
verify.xypolygon(p, fatal = TRUE)
is.hole.xypolygon(polly)
Area.xypolygon(polly)
bdrylength.xypolygon(polly)
reverse.xypolygon(p, adjust=FALSE)
overlap.xypolygon(P, Q)
simplify.xypolygon(p, dmin)
inside.xypolygon(pts, polly, test01, method)
}
\arguments{
  \item{p,polly,P,Q}{
     Data representing a polygon. See Details.
  }
  \item{dmin}{
     Single numeric value giving the minimum permissible
     length of an edge in the simplified polygon.
  }
  \item{fatal}{
     Logical value indicating whether failure is a fatal error.
  }
  \item{pts}{
     Coordinates of points to be tested.
     A named list with entries \code{x,y} which are numeric vectors
     of coordinates.
  }
  \item{adjust}{
     Logical value indicating whether internal data should be adjusted.
     See Details.
  }
  \item{test01,method}{
     For developer use only.
  }
}
\details{
   In the \pkg{spatstat} family of packages, a polygon in the
   Euclidean plane is represented as a named list 
   with the following entries:
   \describe{
     \item{\code{x},\code{y}}{
     Numeric vectors giving the coordinates of the vertices.
     The vertices should be traversed in anti-clockwise order
     (unless the polygon is a hole, when they should be traversed
     in clockwise order) and the last vertex should \bold{not} repeat
     the first vertex.
     }
     \item{hole}{
       Optional. A logical value indicating whether the
       polygon is a hole. 
     }
     \item{area}{
       Optional. Single numeric value giving the area of the polygon
       (negative if it is a hole).
     }
   }
   The function \code{verify.xypolygon} checks whether its argument
   satisfies this format. If so, it returns \code{TRUE}; if not,
   it returns \code{FALSE} or (if \code{fatal=TRUE}) generates a fatal error.
   
   The other functions listed here perform basic calculations for
   polygons using elementary Cartesian analytic geometry in \R.

   \code{is.hole.xypolygon} determines whether a polygon is a hole or not.
   
   \code{Area.xypolygon} computes the area of the polygon using the
   discrete Green's formula.
   
   \code{bdrylength.xypolygon} calculates the total length of edges
   of the polygon.
   
   \code{reverse.xypolygon} reverses the order of the
   coordinate vectors \code{x} and \code{y}. If \code{adjust=TRUE},
   the other entries \code{hole} and \code{area} will be adjusted as well.
   
   \code{overlap.xypolygon} computes the area of overlap between two
   polygons using the discrete Green's formula. It is slow compared
   to the code in the \pkg{polyclip} package.
   
   \code{simplify.xypolygon} removes vertices of the polygon until
   every edge is longer than \code{dmin}.

   \code{inside.xypolygon(pts, polly)} determines whether each point
   in \code{pts} lies inside the polygon \code{polly} and returns a
   logical vector.
}
\value{
  \code{verify.xypolygon} and 
  \code{is.hole.xypolygon} return a single logical value.
  
  \code{inside.xypolygon} returns a logical vector.

  \code{Area.xypolygon}, \code{bdrylength.xypolygon}
  and \code{overlap.xypolygon}    
  return a single numeric value.
  
  \code{reverse.xypolygon} and \code{simplify.xypolygon}
  return another polygon object.
}
\author{
  \adrian.
}
\examples{
  p <- list(x=c(0,1,4,2), y=c(0,0,2,3))
  is.hole.xypolygon(p)
  Area.xypolygon(p)
  bdrylength.xypolygon(p)
  overlap.xypolygon(p, list(x=p$x+1, y=p$y+1))
  reverse.xypolygon(p)

  plot(c(0,5),c(0,3),type="n",xlab="x", ylab="y")
  polygon(p)
  polygon(simplify.xypolygon(p, 1.1), lty=3)

  plot(c(0,5),c(0,3),type="n",xlab="x", ylab="y")
  polygon(p)
  xx <- runif(10, max=5)
  yy <- runif(10, max=3)
  points(xx, yy)
  ok <- as.logical(inside.xypolygon(list(x=xx, y=yy), p))
  points(xx[ok], yy[ok], pch=16)
}
\keyword{math}
