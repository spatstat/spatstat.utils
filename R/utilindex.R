#'    utilindex.R
#'
#'    Copyright (c) Adrian Baddeley
#'
#'    GNU Public Licence >= 2
#'

matchIntegerDataFrames <- function(X, Y) {
  X <- as.data.frame(X)
  Y <- as.data.frame(Y)
  stopifnot(ncol(X) == ncol(Y))
  if(!all(sapply(X, storage.mode) == "integer"))
    X <- as.data.frame(lapply(X, as.integer))
  if(!all(sapply(Y, storage.mode) == "integer"))
    Y <- as.data.frame(lapply(Y, as.integer))
  ans <- rep(NA_integer_, nrow(X))
  switch(ncol(X),
         {
	    #' one column
            ans <- match(X[,1], Y[,1])
	 },
	 {
	    #' two columns
	    #' order them
	    oX <- order(X[,1], X[,2])
	    oY <- order(Y[,1], Y[,2])
	    XX <- X[oX, , drop=FALSE]
	    YY <- Y[oY, , drop=FALSE]
	    z <- .C(C_Cmatch2int,
	       na = as.integer(nrow(XX)),
	       xa = as.integer(XX[,1]),
	       ya = as.integer(XX[,2]),
	       nb = as.integer(nrow(YY)),
	       xb = as.integer(YY[,1]),
	       yb = as.integer(YY[,2]),
	       match = as.integer(integer(nrow(XX))),
	       PACKAGE="spatstat.utils")
	    zz <- z$match
	    zz[zz == 0] <- NA
	    ans[oX] <- oY[zz]
	 },
	 {
	    #' three columns
	    #' order them
	    oX <- order(X[,1], X[,2], X[,3])
	    oY <- order(Y[,1], Y[,2], Y[,3])
	    XX <- X[oX, , drop=FALSE]
	    YY <- Y[oY, , drop=FALSE]
	    z <- .C(C_Cmatch3int,
	       na = as.integer(nrow(XX)),
	       xa = as.integer(XX[,1]),
	       ya = as.integer(XX[,2]),
	       za = as.integer(XX[,3]),
	       nb = as.integer(nrow(YY)),
	       xb = as.integer(YY[,1]),
	       yb = as.integer(YY[,2]),
	       zb = as.integer(YY[,3]),
	       match = as.integer(integer(nrow(XX))),
	       PACKAGE="spatstat.utils")
	    zz <- z$match
	    zz[zz == 0] <- NA
	    ans[oX] <- oY[zz]
	 },
	 stop("Sorry, not implemented for more than 3 columns", call.=FALSE))
    return(ans)
}

   