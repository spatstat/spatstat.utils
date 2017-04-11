\name{termsinformula}
\alias{termsinformula}
\alias{offsetsinformula}
\alias{variablesinformula}
\alias{lhs.of.formula}
\alias{lhs.of.formula<-}
\alias{rhs.of.formula}
\alias{rhs.of.formula<-}
\alias{can.be.formula}
\alias{identical.formulae}
\title{
  Manipulate Formulae
}
\description{
  Operations for manipulating formulae.
}
\usage{
termsinformula(x)
variablesinformula(x)
offsetsinformula(x)
lhs.of.formula(x)
rhs.of.formula(x, tilde=TRUE)
lhs.of.formula(x) <- value
rhs.of.formula(x) <- value
can.be.formula(x)
identical.formulae(x,y)
}
\arguments{
  \item{x,y}{
    Formulae, or character strings representing formulae.
  }
  \item{tilde}{
    Logical value indicating whether to retain the tilde.
  }
  \item{value}{
    Symbol or expression in the \R language. See Examples.
  }
}
\details{
  \code{variablesinformula(x)}
  returns a character vector of the names
  of all variables which appear in the formula \code{x}.
  
  \code{termsinformula(x)} returns a character vector of all 
  terms in the formula \code{x} (after expansion of interaction terms).

  \code{offsetsinformula(x)} returns a character vector of all
  offset terms in the formula.

  \code{rhs.of.formula(x)} returns the right-hand side of the formula
  as another formula (that is, it removes the left-hand side) provided
  \code{tilde=TRUE} (the default). If \code{tilde=FALSE}, then the
  right-hand side is returned as a language object.
  
  \code{lhs.of.formula(x)} returns the left-hand side of the formula
  as a symbol or language object, or \code{NULL} if the formula has no
  left-hand side.

  \code{lhs.of.formula(x) <- value}
  and \code{rhs.of.formula(x) <- value}
  change the formula \code{x} by replacing the left or right hand side
  of the formula by \code{value}.

  \code{can.be.formula(x)} returns \code{TRUE} if \code{x} is a formula
  or a character string that can be parsed as a formula, and returns
  \code{FALSE} otherwise.

  \code{identical.formulae(x,y)} returns \code{TRUE} if \code{x} and
  \code{y} are identical formulae (ignoring their environments).
}
\value{
  \code{variablesinformula},
  \code{termsinformula} and
  \code{offsetsinformula} return a character vector.
  
  \code{rhs.of.formula} returns a formula.
  \code{lhs.of.formula} returns a symbol or language object, or \code{NULL}.

  \code{can.be.formula} and \code{identical.formulae} return
  a logical value.
}
\author{
  \spatstatAuthors.
}
\examples{
  f <- (y ~ x + z*w + offset(h))
  lhs.of.formula(f)
  rhs.of.formula(f)
  variablesinformula(f)  
  termsinformula(f)
  offsetsinformula(f)
  g <- f
  environment(g) <- new.env()
  identical(f,g)
  identical.formulae(f,g)
  lhs.of.formula(f) <- quote(mork) # or as.name("mork")
  f
  rhs.of.formula(f) <- quote(x+y+z) # or parse(text="x+y+z")[[1]]
  f
}
\keyword{utilities}

