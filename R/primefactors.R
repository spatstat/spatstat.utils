#
#  primefactors.R
#
#  $Revision: 1.16 $   $Date: 2025/01/19 05:11:27 $
#

## Table of prime numbers is now in sysdata object 'Spatstat.PrimesTable'
## Maximum prime number in table is 'Spatstat.maxPrime'

primesbelow <- function(nmax) {
  if(nmax <= Spatstat.maxPrime)
    return(Spatstat.PrimesTable[Spatstat.PrimesTable <= nmax])
  eratosthenes(nmax, c(Spatstat.PrimesTable, (Spatstat.maxPrime+1):nmax))
}

eratosthenes <- function(nmax, startset=2:nmax) {
  # The Sieve of Eratosthenes
  if(nmax < 2) return(integer(0))
  numbers <- as.integer(startset)
  prime <- startset[1]
  repeat{
    retain <-  (numbers <= prime) | (numbers %% prime != 0)
    numbers <- numbers[retain]
    remaining <- (numbers > prime)
    if(!any(remaining))
      break
    prime <- min(numbers[remaining])
  }
  return(numbers)
}

primefactors <- function(n, method=c("C", "interpreted")) {
  check.1.integer(n)
  if(n <= 0) return(integer(0))
  method <- match.arg(method)
  MaxInt <- .Machine$integer.max
  if(method == "C" && n > MaxInt)
    method <- "interpreted"
  switch(method,
         interpreted = {
           prmax <- floor(sqrt(n))
           if(prmax <= Spatstat.maxPrime) {
             ## table of primes is sufficient
             result <- findprimefactors(n, primesbelow(prmax))
           } else {
             ## try all tabulated primes first
             result <- integer(0)
             candidateprimes <- Spatstat.PrimesTable
             while(n > 1 && any(divides <- (n %% candidateprimes == 0))) {
               ## reduce problem size
               primedivisors <- candidateprimes[divides]
               result <- sort(c(result, primedivisors))
               n <- n/prod(primedivisors)
               if(n <= MaxInt) n <- as.integer(n)
               prmax <- floor(sqrt(n))
               candidateprimes <- primedivisors
             }
             if(n > 1) {
               ## dang it's hard
               secondresult <- findprimefactors(n, primesbelow(prmax))
               result <- sort(c(result, secondresult))
             }
           }
         },
         C = {
           kmax <- ceiling(log2(n))
           z <- .C(C_primefax,
                   n=as.integer(n),
                   factors=as.integer(integer(kmax)),
                   nfactors=as.integer(integer(1L)))
           result <- z$factors[seq_len(z$nfactors)]
         },
         stop("Unrecognised method"))
  return(result)
}

findprimefactors <- function(n, primes) {
  divides.n <- (n %% primes == 0)
  if(!any(divides.n)) 
    return(n)
  divisors <- primes[divides.n]
  m <- n/prod(divisors)
  if(m == 1) return(divisors)
  mfactors <- findprimefactors(as.integer(m), divisors)
  return(sort(c(divisors, mfactors)))
}

is.prime <- function(n) { length(primefactors(n)) == 1 }

relatively.prime <- function(n, m) {
  if(n == 0 || m == 0) return(FALSE)
  cf <- intersect(primefactors(n), primefactors(m))
  return(length(cf) == 0)
}

least.common.multiple <- function(n, m) {
  nf <- primefactors(n)
  mf <- primefactors(m)
  p <- sortunique(c(nf,mf))
  nfac <- table(factor(nf, levels=p))
  mfac <- table(factor(mf, levels=p))
  prod(p^pmax.int(nfac,mfac))
}

greatest.common.divisor <- function(n, m) {
  nf <- primefactors(n)
  mf <- primefactors(m)
  p <- sortunique(c(nf,mf))
  nfac <- table(factor(nf, levels=p))
  mfac <- table(factor(mf, levels=p))
  prod(p^pmin.int(nfac,mfac))
}
  
divisors <- local({

  divisors <- function(n) {
    p <- primefactors(n)
    up <- sortunique(p)
    k <- table(factor(p, levels=up))
    return(rest(k, up))
  }

  rest <- function(kk, uu) {
    powers <- uu[1]^(0:(kk[1]))
    if(length(uu) == 1)
      return(powers)
    rr <- rest(kk[-1], uu[-1])
    products <- as.vector(outer(powers, rr, "*"))
    return(sortunique(products))
  }

  divisors
})

is.square <- function(n) {
  check.1.integer(n)
  if(n < 0) return(FALSE)
  v <- sqrt(n)
  m <- c(floor(v), ceiling(v))
  any(m^2 == n)
}

is.cube <- function(n) {
  check.1.integer(n)
  v <- n^(1/3)
  m <- c(floor(v), ceiling(v))
  any(m^3 == n)
}

is.power <- function(n) {
  check.1.integer(n)
  tp <- table(primefactors(abs(n)))
  if(length(tp) == 0) return(TRUE)
  m <- Reduce(greatest.common.divisor, tp)
  (m > 1) && ((n > 0) || any(primefactors(m) > 2))
}
