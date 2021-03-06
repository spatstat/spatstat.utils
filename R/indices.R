#'
#'   indices.R
#'
#'   Code for handling vector/array indices
#'
#'   Copyright (c) Adrian Baddeley, Ege Rubak and Rolf Turner 2016-2020
#'   GNU Public Licence >= 2.0 
#' 
#'   $Revision: 1.15 $  $Date: 2021/03/06 09:30:27 $
#'

grokIndexVector <- function(ind, len, nama=NULL) {
  #' Parse any kind of index vector,
  #' returning
  #'      a logical index 'lo' (the subset of elements),
  #'      a positive integer index 'i' ( = which(lo) ),
  #'      the number 'n' of values required
  #'      the number 'nind' of values indexed
  #' and if appropriate
  #'      a character vector 's' of names
  #'      a mapping 'map' (matching 'ind' to 'i')
  #'
  #' There are two versions:
  #'    'strict' (confined to specified bounds 1:len and specified names 'nama')
  #'    'full'   (allowing implied extension of array bounds)
  named <- !is.null(nama)
  if(missing(len) && named) len <- length(nama)
  force(len)
  # special cases
  if(is.null(ind)) {
    #' all entries (implied)
    return(list(strict=list(lo=rep(TRUE, len),
                            i=seq_len(len),
                            n=len,
                            s=nama,
                            nind=len,
                            map=NULL)))
  }
  if(length(ind) == 0) {
    #' no entries
    return(list(strict=list(lo=logical(len),
                            i=integer(0),
                            n=0L,
                            s=character(0),
                            nind=0L,
                            map=NULL)))
  }
  #' main cases
  if(is.logical(ind)) {
    # logical (subset) index into 1:len
    lo <- ind
    m <- length(lo)
    if(m < len) {
      #' recycle
      oldlo <- lo
      lo <- logical(len)
      lo[oldlo] <- TRUE
      m <- len
    }
    if(m == len) {
      n <- sum(lo)
      result <- list(strict=list(lo=lo, i=which(lo), n=n, s=nama[lo],
                       nind=n, map=NULL))
      return(result)
    }
    #' new elements implied
    lostrict <- lo[1:len]
    newones <- (len+1L):m
    nstrict <- sum(lostrict)
    strict <- list(lo=lostrict,
                   i=which(lostrict),
                   n=nstrict,
                   s=nama[lostrict],
                   nind=nstrict,
                   map=NULL)
    nfull <- sum(lo)
    full <- list(newones=newones,
                 fullset=1:m,
                 lo=lo,
                 i=which(lo),
                 n=nfull,
                 s=if(named) c(nama, rep("", length(newones)))[lo] else NULL,
                 nind=nfull,
                 map=NULL)
    return(list(strict=strict, full=full))
  }
  if(is.character(ind)) {
    #' character index into 'nama'
    #' order is important
    imap <- match(ind, nama)
    unknown <- is.na(imap)
    i <- sortunique(imap[!unknown])
    lo <- logical(len)
    lo[i] <- TRUE
    map <- match(imap, i)
    n <- length(ind)
    s <- nama[map]
    nind <- length(ind)
    if(identical(map, seq_along(map))) map <- NULL
    strict <- list(lo=lo, i=i, n=n, s=s, nind, map=map)
    if(!any(unknown)) return(list(strict=strict))
    # some unrecognised strings
    newones <- unique(ind[unknown])
    fullset <- c(nama, newones)
    imapfull <- match(ind, fullset)
    ifull <- sortunique(imapfull)
    lofull <- logical(length(fullset))
    lofull[ifull] <- TRUE
    mapfull <- match(imapfull, ifull)
    nfull <- length(ind)
    sfull <- fullset[mapfull]
    if(identical(mapfull, seq_along(mapfull))) mapfull <- NULL
    full <- list(newones=newones, fullset=fullset,
                 lo=lofull, i=ifull, n=nfull, s=sfull, nind=nind, map=mapfull)
    return(list(strict=strict, full=full))
  }
  if(is.numeric(ind)) {
    if(all(ind > 0)) {
      #' integer index into 1:len
      #' order is important
      ifull <- sortunique(ind)
      inside <- (ifull <= len)
      i <- ifull[inside]
      map <- match(ind, i)
      lo <- logical(len)
      lo[i] <- TRUE
      n <- length(ind)
      s <- nama[ind]
      if(identical(map, seq_along(map))) map <- NULL
      strict <- list(lo=lo,i=i,n=n,s=s,nind=length(i),map=map)
      if(all(inside)) return(list(strict=strict))
      newones <- ifull[!inside]
      mapfull <- match(ind, ifull)
      fullset <- 1:max(ifull)
      lofull <- logical(length(fullset))
      lofull[ifull] <- TRUE
      nfull <- length(ind)
      sfull <- if(named) c(nama, rep("", length(newones)))[ind] else NULL
      if(identical(mapfull, seq_along(mapfull))) mapfull <- NULL
      return(list(strict=strict, full=list(newones=newones, fullset=fullset,
                                           lo=lofull, i=ifull,
                                           n=nfull, s=sfull,
                                           nind=nfull, map=mapfull)))
    }
    if(all(ind < 0)) {
      #' exclusion index
      #' ignore indices outside bounds
      negind <- -ind
      negind <- negind[negind <= len]
      lo <- rep(TRUE, len)
      lo[negind] <- FALSE
      i <- which(lo)
      n <- length(i)
      map <- seq_len(n)
      return(list(strict=list(lo=lo, i=i, n=n, s=nama[i], nind=n, map=map)))
    }
    stop("An integer index may not contain both negative and positive values",
         call.=FALSE)
  }
  stop("Unrecognised format for index", call.=FALSE)
}

#' g is the result of 'grokIndexVector'

strictIndexSequence <- function(g) { g$strict$i }
fullIndexSequence   <- function(g) { g$full$i %orifnull% g$strict$i }

replacementIndex <- function(ii, stuff) {
  ## 'stuff' is predigested information about a subset index.
  ## Find the location in the original array
  ## whose value should be replaced by the 'ii'-th replacement value
  ## according to this info.
  if(length(stuff) == 0)
    stop("Internal error - no predigested info", call.=FALSE)
  with(stuff, {
    if(!is.null(map)) ii <- map[ii]
    i[ii]
  })
}

positiveIndex <- function(i, nama, len=length(nama)) {
  #' convert any kind of index to a positive integer sequence
  x <- seq_len(len)
  if(is.null(i)) return(x)
  stopifnot(is.vector(i))
  if(is.numeric(i) && !all(ok <- (abs(i) <= len))) {
    warning("Index values lie outside array bounds", call.=FALSE)
    i <- i[ok]
  }
  names(x) <- nama
  y <- x[i]
  return(unname(y))
}

logicalIndex <- function(i, nama, len=length(nama)) {
  #' convert any kind of index to a logical vector
  if(is.null(i)) return(rep(TRUE, len))
  stopifnot(is.vector(i))
  if(is.numeric(i) && !all(ok <- (abs(i) <= len))) {
    warning("Index values lie outside array bounds", call.=FALSE)
    i <- i[ok]
  }
  x <- logical(len)
  names(x) <- nama
  x[i] <- TRUE
  return(unname(x))
}

