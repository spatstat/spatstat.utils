\name{resolve.defaults}
\alias{resolve.defaults}
\alias{resolve.1.default}
\title{
  Determine Values of Variables Using Several Default Rules
}
\description{
  Determine the values of variables by applying
  several different default rules in a given order.
}
\usage{
   resolve.defaults(\dots, .MatchNull = TRUE, .StripNull = FALSE)

   resolve.1.default(.A, \dots)
}
\arguments{
  \item{\dots}{
    Several lists of \code{name=value} pairs.
  }
  \item{.MatchNull}{
    Logical value. If \code{TRUE} (the default), an entry of the form
    \code{name=NULL} will be treated as assigning the value
    \code{NULL} to the variable \code{name}. If \code{FALSE},
    such entries will be ignored.
  }
  \item{.StripNull}{
    Logical value indicating whether 
    entries of the form \code{name=NULL} should be removed
    from the result.
  }
  \item{.A}{
    Either a character string giving the name of the variable
    to be extracted, or a list consisting of one \code{name=value} pair
    giving the variable name and its fallback default value.
  }
}
\details{
  These functions determine the values of variables by applying a series
  of default rules, in the order specified.

  Each of the arguments \code{\dots} should be a list of
  \code{name=value} pairs giving a value
  for a variable \code{name}. Each list could represent a
  set of arguments given by the user, or a 
  rule assigning default values to some variables.
  Lists that appear earlier in the sequence of arguments \code{\dots}
  take precedence.

  The arguments \code{\dots} will be concatenated into a single list.
  The earliest occurrence of each \code{name} is then used to
  determine the final value of the variable \code{name}.

  The function \code{resolve.defaults} returns a list of
  \code{name=value} pairs for all variables encountered.
  It is commonly used to decide the values of
  arguments to be passed to another function
  using \code{\link[base]{do.call}}.

  The function \code{resolve.1.default} returns the value
  of the specified variable as determined by \code{resolve.defaults}.
  It is commonly used inside a function to determine the value
  of an argument.
}
\value{
  The result of \code{resolve.defaults} is
  a list of \code{name=value} pairs.

  The result of \code{resolve.1.default} can be any kind of value.
}
\author{
  \adrian
}
\seealso{
  \code{\link[base]{do.call}}
}
\examples{
  user <- list(day="Friday")
  ruleA <- list(month="Jan", gravity=NULL)
  ruleB <- list(day="Tuesday", month="May", gravity=42)
  resolve.defaults(user, ruleA, ruleB)
  resolve.defaults(user, ruleA, ruleB, .StripNull=TRUE)
  resolve.defaults(user, ruleA, ruleB, .MatchNull=FALSE)

  resolve.1.default("month", user, ruleA, ruleB)
}
\keyword{programming}
\keyword{utilities}
