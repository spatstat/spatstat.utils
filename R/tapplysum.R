#'  tapplysum.R
#'
#'  Faster replacement for tapply(..., FUN=sum)
#'
#'  Adrian Baddeley and Tilman Davies
#' 
#'  $Revision: 1.14 $  $Date: 2018/06/14 03:23:16 $

tapplysum <- function(x, flist, do.names=FALSE, na.rm=TRUE) {
  if(is.complex(x)) {
    zr <- tapplysum(Re(x), flist, do.names=do.names, na.rm=na.rm)
    zi <- tapplysum(Im(x), flist, do.names=do.names, na.rm=na.rm)
    z <- zr + zi * 1i ## preserves names and dimensions
    return(z)
  }
  stopifnot(is.numeric(x))
  stopifnot(is.list(flist))
  stopifnot(all(lengths(flist) == length(x)))
  stopifnot(all(sapply(flist, is.factor)))
  nfac <- length(flist)
  goodx <- is.finite(x)
  if(na.rm) goodx <- goodx | is.na(x)
  if(nfac > 3 || !all(goodx)) {
    y <- tapply(x, flist, sum)
    y[is.na(y)] <- 0
    return(y)
  }
  #' extract data
  ifac <- flist[[1L]]
  mi <- length(levels(ifac))
  ii <- as.integer(ifac)
  if(nfac >= 2) {
    jfac <- flist[[2L]]
    mj <- length(levels(jfac))
    jj <- as.integer(jfac)
  }
  if(nfac == 3) {
    kfac <- flist[[3L]]
    mk <- length(levels(kfac))
    kk <- as.integer(kfac)
  }
  #' remove NA entries
  switch(nfac,
  {  # case 1
    if(anyNA(x) || anyNA(ii)) {
      ok <- !(is.na(x) | is.na(ii))
      ii <- ii[ok]
      x <- x[ok]
    }
  },
  {  # case 2
    if(anyNA(x) || anyNA(ii) || anyNA(jj)) {
      ok <- !(is.na(x) | is.na(ii) | is.na(jj))
      ii <- ii[ok]
      jj <- jj[ok]
      x <- x[ok]
    }
  },
  {
    if(anyNA(x) || anyNA(ii) || anyNA(jj) || anyNA(kk)) {
      ok <- !(is.na(x) | is.na(ii) | is.na(jj) | is.na(kk))
      ii <- ii[ok]
      jj <- jj[ok]
      kk <- kk[ok]
      x <- x[ok]
    }
  })
  #' process data
  n <- length(ii)
  switch(nfac,
  {  # case 1
    result <- numeric(mi)
    if(n > 0) {
      oo <- order(ii)
      zz <- .C(C_ply1sum,
               nin = as.integer(n),
               xin = as.double(x[oo]),
               iin = as.integer(ii[oo]),
               nout = as.integer(integer(1L)),
               xout = as.double(numeric(n)),
               iout = as.integer(integer(n)))
      nout <- zz$nout
      if(nout > 0) {
        iout <- zz$iout
        xout  <- zz$xout[1:nout]
        result[iout] <- xout
      }
    }
  },
  {  # case 2
    result <- matrix(0, mi, mj)
    if(n > 0) {
      oo <- order(ii, jj)
      zz <- .C(C_ply2sum,
               nin = as.integer(n),
               xin = as.double(x[oo]),
               iin = as.integer(ii[oo]),
               jin = as.integer(jj[oo]),
               nout = as.integer(integer(1L)),
               xout = as.double(numeric(n)),
               iout = as.integer(integer(n)),
               jout = as.integer(integer(n)))
      nout <- zz$nout
      if(nout > 0) {
        ijout <- cbind(zz$iout, zz$jout)[1:nout,,drop=FALSE]
        xout  <- zz$xout[1:nout]
        result[ijout] <- xout
      }
    }
  },
  { # case 3
    result <- array(0, dim=c(mi, mj, mk))
    if(n > 0) {
      oo <- order(ii, jj, kk)
      zz <- .C(C_ply3sum,
               nin = as.integer(n),
               xin = as.double(x[oo]),
               iin = as.integer(ii[oo]),
               jin = as.integer(jj[oo]),
               kin = as.integer(kk[oo]),
               nout = as.integer(integer(1L)),
               xout = as.double(numeric(n)),
               iout = as.integer(integer(n)),
               jout = as.integer(integer(n)),
               kout = as.integer(integer(n)))
      nout <- zz$nout
      if(nout > 0) {
        ijkout <- cbind(zz$iout, zz$jout, zz$kout)[1:nout,,drop=FALSE]
        xout  <- zz$xout[1:nout]
        result[ijkout] <- xout
      }
    }
  })
  if(do.names) 
    dimnames(result) <- lapply(flist, levels)
  return(result)
}


                       
           
