\name{spatstat.utils-package}
\alias{spatstat.utils-package} 
\alias{spatstat.utils} 
\docType{package}
\title{The spatstat.utils Package}
\description{
  The \pkg{spatstat.utils} package contains low-level utilities,
  written for the \pkg{spatstat} package, which may be useful in
  other packages as well.
}
\details{
  The functions in \pkg{spatstat.utils} were originally written
  as internal, undocumented, utility functions in the \pkg{spatstat} package.

  Many of these functions could be useful to other programmers,
  so we have made them available in a separate package
  \pkg{spatstat.utils} and provided documentation.

  The functionality contained in \pkg{spatstat.utils} includes:
  \describe{
    \item{Factorisation of integers}{
      Find prime numbers (\code{\link{primesbelow}}),
      factorise a composite number into its prime factors
      (\code{\link{primefactors}}),
      determine whether a number is prime (\code{\link{is.prime}})
      or whether two numbers are relatively prime 
      (\code{\link{relatively.prime}}),
      and find the least common multiple
      or greatest common divisor of two numbers
      (\code{\link{least.common.multiple}},
      \code{\link{greatest.common.divisor}}).
    }
    \item{Faster versions of basic \R tools}{
      Faster versions of some basic \R tools and idioms are provided.
      These are only faster in particular cases, but if you know that
      your data have a particular form, the acceleration can be substantial.
      See \code{\link{ifelseAB}}, \code{\link{fave.order}},
      \code{\link{revcumsum}}, \code{\link{tapplysum}}.
    }
    \item{Grammar}{
      Use the correct word in English to refer to
      an ordinal number (\code{\link{ordinal}},
      \code{\link{ordinalsuffix}})
      and the correct indefinite article
      (\code{\link{articlebeforenumber}}).
    }
    \item{Tools for generating printed output}{
      The function \code{\link{splat}} is a replacement for
      \code{cat(paste(\dots))} which ensures that output stays
      inside the declared text margin (\code{\link{getOption}("width")})
      and can also perform automatic indentation.
      There are useful functions to add or remove parentheses
      (\code{\link{paren}}, \code{\link{unparen}}) and
      to make comma-separated lists (\code{\link{commasep}}).
    }
    \item{Handling intervals (ranges) of real numbers}{
      Simple functions handle an interval (range) of numerical
      values: \code{\link{check.range}},
      \code{\link{intersect.ranges}}, 
      \code{\link{inside.range}}, 
      \code{\link{check.in.range}},
      \code{\link{prange}}.
    }
    \item{Handling a formula}{
      Tools for handling a formula in the \R language include
      \code{\link{lhs.of.formula}},
      \code{\link{rhs.of.formula}},
      \code{\link{variablesinformula}},
      \code{\link{termsinformula}},
      \code{\link{offsetsinformula}},
      \code{\link{can.be.formula}} and
      \code{\link{identical.formulae}}.
    }
    \item{Polynomials}{
      There are tools for creating and manipulating symbolic expressions
      for polynomials, as they might appear in a formula
      (\code{\link{sympoly}},
      \code{\link{expand.polynom}}).
    }
    \item{Validating arguments}{
      There are many tools for validating an argument and
      generating a comprehensible error or warning message
      if the argument is not valid:
      \code{\link{check.1.integer}},
      \code{\link{check.nvector}},
      \code{\link{check.named.vector}}.
    }
    \item{Passing arguments}{
      There are many tools for calling a function while passing
      only some of the arguments in a supplied list of arguments:
      \code{\link{do.call.matched}}, \code{\link{do.call.without}},
      \code{\link{resolve.defaults}}.
    }
    \item{Traced optimization}{
      \code{\link{optimizeWithTrace}} is a simple wrapper for
      the one-dimensional optimization routine
      \code{\link[stats]{optimize}}. It stores the values of the
      function argument each time it is called, stores the resulting
      function values, and returns them along with the optimal value.
    }
    \item{Workarounds}{
      There are workarounds for known bugs or undesirable features in
      other software.
      \code{\link{spatstatLocator}} is a replacement for
      \code{\link[graphics]{locator}} which works around a bug in the
      \code{RStudio} graphics interface.
      \code{\link{cat.factor}} concatenates several factors,
      merging the levels, to produce a new factor.
    }
  }
}
\section{Licence}{
  This library and its documentation are usable under the terms of the
  \dQuote{GNU General Public License},
  a copy of which is distributed with \R.
}
\author{
  \spatstatAuthors.
}
\keyword{spatial}
\keyword{package}
