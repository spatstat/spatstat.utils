\name{primefactors}
\alias{primefactors}
\alias{primesbelow}
\alias{divisors}
\alias{is.prime}
\alias{relatively.prime}
\alias{greatest.common.divisor}
\alias{least.common.multiple}
\title{
  Primes, Prime Factorization, Common Divisor
}
\description{
  These functions find prime numbers, 
  factorise a composite number into its prime factors,
  determine whether a number is prime, and find the least common multiple
  or greatest common divisor of two numbers.
}
\usage{
primefactors(n, method=c("C", "interpreted"))
divisors(n)
is.prime(n)
relatively.prime(n, m)
least.common.multiple(n,m)
greatest.common.divisor(n,m)
primesbelow(nmax)
}
\arguments{
  \item{n,m}{
    Integers to be factorized.
  }
  \item{nmax}{
    Integer. Upper limit on prime numbers to be found.
  }
  \item{method}{
    Character string indicating the choice of algorithm.
    (Developer use only.)
  }
}
\details{
  \code{is.prime(n)} returns \code{TRUE} if \code{n} is a prime number,
  and \code{FALSE} otherwise.

  \code{primefactors(n)} factorises the integer \code{n}
  into its prime number factors, and returns an integer vector
  containing these factors. Some factors may be repeated.

  \code{divisors(n)} finds all the integers which divide
  the integer \code{n}, and returns them as a sorted vector of integers
  (beginning with \code{1} and ending with \code{n}).

  \code{relatively.prime(n, m)} returns \code{TRUE} if the integers
  \code{n} and \code{m} are relatively prime, that is, if they have no
  common factors.

  \code{least.common.multiple} and \code{greatest.common.divisor}
  return the least common multiple or greatest common divisor of two
  integers \code{n} and \code{m}.

  \code{primesbelow(nmax)} returns an integer vector containing all the
  prime numbers less than or equal to \code{nmax}.
}
\value{
  \code{is.prime} and \code{relatively.prime} return a logical value.

  \code{least.common.multiple} and \code{greatest.common.divisor}
  return a single integer.
  
  \code{primefactors} and \code{primesbelow}
  return an integer vector.
}
\author{
  \adrian.
}
\examples{
  is.prime(17)

  relatively.prime(2, 3)
  
  primefactors(24) ## Note repeated factors

  divisors(24)

  greatest.common.divisor(60, 100) 

  primesbelow(20)
}
\keyword{math}
