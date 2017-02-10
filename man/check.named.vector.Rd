\name{check.named.vector}
\alias{check.named.vector}
\alias{check.named.list}
\alias{check.named.thing}
\title{
  Check Whether Object Has Required Components
}
\description{
  These functions check whether the object \code{x} has
  components with the required names, and does not have
  any unexpected components.
}
\usage{
check.named.vector(x, nam, context = "", namopt = character(0),
                   onError = c("fatal", "null"))

check.named.list(x, nam, context = "", namopt = character(0),
                   onError = c("fatal", "null"))

check.named.thing(x, nam, namopt = character(0),
                  xtitle = NULL, valid = TRUE, type = "object",
                  context = "", fatal = TRUE)
}
\arguments{
  \item{x}{
    The object to be checked.
  }
  \item{nam}{
    Vector of character strings giving the names of all the
    components which must be present.
  }
  \item{namopt}{
    Vector of character strings giving the names of components
    which may optionally be present.
  }
  \item{context}{
    Character string describing the context in which \code{x} is being checked.
  }
  \item{xtitle}{
    Optional character string to be used when referring to \code{x}.
  }
  \item{valid}{
    Logical value indicating whether \code{x} belongs to the required
    class of objects.
  }
  \item{type}{
    Character string describing the required class of objects.
  }
  \item{onError}{
    Character string indicating what to do if \code{x} fails the checks.
  }
  \item{fatal}{
    Logical value indicating what to do if \code{x} fails the checks.
    If \code{fatal=TRUE} (the default), an error occurs.
  }
}
\details{
  \code{check.named.thing} checks whether \code{x} has all the
  required components, in the sense that \code{names(x)}
  includes all the names in \code{nam},
  and that every entry in \code{names(x)} belongs to either \code{nam} or
  \code{namopt}. If all these checks are true, the result is a
  zero-length character vector. Otherwise, if \code{fatal=TRUE} (the
  default), an error occurs; otherwise the result is a character
  vector describing the checks which failed.
  
  \code{check.named.vector} checks whether \code{x} is a numeric vector
  and \code{check.named.list} checks whether \code{x} is a list.
  They then call \code{check.named.thing} to check whether all the
  required components are present. If all these checks are true, the result is
  a reordered version of \code{x} in which all the compulsory entries
  appear first. Otherwise, if \code{onError="fatal"} (the default)
  an error occurs; otherwise the result is \code{NULL}.
}
\value{
  \code{check.named.vector} returns a numeric vector or \code{NULL}.

  \code{check.named.list} returns a list or \code{NULL}.

  \code{check.named.thing} returns a character vector.
}
\author{
  \adrian.
}
\seealso{
  \code{\link{check.1.integer}}
}
\examples{
  z <- list(a=1, b=2, e=42)
  check.named.list(z, c("a", "b"), namopt=c("c", "d", "e"))
  check.named.thing(z, c("a", "b"), namopt=c("c", "d", "e"))
  zz <- unlist(z)
  check.named.vector(zz, c("a", "b"), namopt=c("c", "d", "e"))
  check.named.thing(z, c("b", "c"), namopt=c("d", "e"), fatal=FALSE)
}
\keyword{error}
