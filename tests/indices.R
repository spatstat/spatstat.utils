#' Header for spatstat.utils/tests/*R
#'

require(spatstat.utils)
ALWAYS <- FULLTEST <- TRUE
#' indices.R
#' Tests of code for understanding index vectors etc
#' $Revision: 1.3 $ $Date: 2020/05/11 01:25:34 $

if(FULLTEST) {
local({

  a <- grokIndexVector(c(FALSE,TRUE),         10)
  b <- grokIndexVector(rep(c(FALSE,TRUE), 7), 10)
  d <- grokIndexVector(c(2,12),               10)
  e <- grokIndexVector(letters[4:2], nama=letters)
  f <- grokIndexVector(letters[10:1], nama=letters[1:5])
  g <- grokIndexVector(-c(2, 5),              10)
  h <- grokIndexVector(-c(2, 5, 15),          10)

  Nam <- letters[1:10]
  j  <- positiveIndex(-c(2,5), nama=Nam)
  jj <- logicalIndex(-c(2,5), nama=Nam)
  k  <- positiveIndex(-c(2,5), nama=Nam)
  kk <- logicalIndex(-c(2,5), nama=Nam)
  mm <- positiveIndex(c(FALSE,TRUE), nama=Nam)
  nn <- positiveIndex(FALSE, nama=Nam)
})
}

