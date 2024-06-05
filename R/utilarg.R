#'
#'   utilarg.R
#'
#'   Utilities for checking/handling arguments
#'
#'  $Revision: 1.13 $  $Date: 2024/06/05 08:16:37 $
#'

"%orifnull%" <- function(a, b) {
  if(!is.null(a)) return(a)
  # b is evaluated only now
  return(b)
}

check.anyvector <- function(v, npoints=NULL, fatal=TRUE, things="data points",
                            naok=FALSE, warn=FALSE, vname, oneok=FALSE) {
  # vector or factor of values for each point/thing
  if(missing(vname))
    vname <- sQuote(short.deparse(substitute(v)))
  whinge <- NULL
  nv <- length(v)
  if(!is.atomic(v) || !is.null(dim(v)))  # vector with attributes
    whinge <- paste(vname, "is not a vector or factor")
  else if(!(is.null(npoints) || (nv == npoints)) &&
          !(oneok && nv == 1)) 
    whinge <- paste("The length of", vname,
                    paren(paste0("=", nv)), 
                    "should equal the number of", things,
                    paren(paste0("=", npoints)))
  else if(!naok && anyNA(v))
    whinge <- paste("Some values of", vname, "are NA or NaN")
  #
  if(!is.null(whinge)) {
    if(fatal) stop(whinge)
    if(warn) warning(whinge)
    ans <- FALSE
    attr(ans, "whinge") <- whinge
    return(ans)
  }
  return(TRUE)
}

check.nvector <- function(v, npoints=NULL, fatal=TRUE, things="data points",
                          naok=FALSE, warn=FALSE, vname, oneok=FALSE) {
  # vector of numeric values for each point/thing
  if(missing(vname))
    vname <- sQuote(short.deparse(substitute(v)))
  whinge <- NULL
  nv <- length(v)
  if(!is.numeric(v))
    whinge <- paste(vname, "is not numeric")
  else if(!is.atomic(v) || !is.null(dim(v)))  # vector with attributes
    whinge <- paste(vname, "is not a vector")
  else if(!(is.null(npoints) || (nv == npoints)) &&
          !(oneok && nv == 1)) 
    whinge <- paste("The length of", vname,
                    paren(paste0("=", nv)), 
                    "should equal the number of", things,
                    paren(paste0("=", npoints)))
  else if(!naok && anyNA(v))
    whinge <- paste("Some values of", vname, "are NA or NaN")
  #
  if(!is.null(whinge)) {
    if(fatal) stop(whinge)
    if(warn) warning(whinge)
    ans <- FALSE
    attr(ans, "whinge") <- whinge
    return(ans)
  }
  return(TRUE)
}

check.nmatrix <- function(m, npoints=NULL, fatal=TRUE, things="data points",
                          naok=FALSE, squarematrix=TRUE, matchto="nrow",
                          warn=FALSE, mname) {
  ## matrix of values for each thing or each pair of things
  if(missing(mname)) mname <- sQuote(short.deparse(substitute(m)))
  whinge <- NULL
  if(!is.matrix(m))
    whinge <- paste(mname, "should be a matrix")
  else if(squarematrix && (nrow(m) != ncol(m)))
    whinge <- paste(mname, "should be a square matrix")
  else if(!naok && anyNA(m))
    whinge <- paste("Some values of", mname, "are NA or NaN")
  else if(!is.null(npoints)) {
    if(matchto=="nrow" && nrow(m) != npoints)
      whinge <- paste("Number of rows in", mname,
                      paren(paste0("=", nrow(m))),
                      "does not match number of", things,
                      paren(paste0("=", npoints)))
    else if(matchto=="ncol" && ncol(m) != npoints)
      whinge <- paste("Number of columns in", mname,
                      paren(paste0("=", ncol(m))),
                      "does not match number of", things,
                      paren(paste0("=", npoints)))
  }
  ##
  if(!is.null(whinge)) {
    if(fatal) stop(whinge)
    if(warn) warning(whinge)
    return(FALSE)
  }
  return(TRUE)
}

check.named.vector <- function(x, nam, context="", namopt=character(0),
                               onError=c("fatal", "null"), xtitle=NULL) {
  onError <- match.arg(onError)
  fatal <- (onError == "fatal")
  if(is.null(xtitle) && fatal)
    xtitle <- short.deparse(substitute(x))
  problem <- check.named.thing(x, nam, namopt, sQuote(xtitle),
                               is.numeric(x), "vector", context,
                               fatal=fatal)
  if(length(problem) > 0)
    return(NULL)
  opt <- namopt %in% names(x)
  return(x[c(nam, namopt[opt])])
}

check.named.list <- function(x, nam, context="", namopt=character(0),
                             onError=c("fatal", "null"), xtitle=NULL) {
  onError <- match.arg(onError)
  fatal <- (onError == "fatal")
  if(is.null(xtitle) && fatal)
    xtitle <- short.deparse(substitute(x))
  problem <- check.named.thing(x, nam, namopt, sQuote(xtitle),
                               is.list(x), "list", context, fatal=fatal)
  if(length(problem) > 0)
    return(NULL)
  opt <- namopt %in% names(x)
  return(x[c(nam, namopt[opt])])
}

check.named.thing <- function(x, nam, namopt=character(0), xtitle=NULL,
                              valid=TRUE, type="object", context="",
                              fatal=TRUE) {
  ## Check whether names(x) contains all obligatory names 'nam'
  ## and possibly some of the optional names 'namopt'.
  ## Return a character string, length 0 if OK, otherwise contains message
  namesx <- names(x)
  omitted <- !(nam %in% namesx)
  foreign <- !(namesx %in% c(nam, namopt))
  if(valid && !any(omitted) && !any(foreign))
    return(character(0))
  ## Some condition violated
  ## give details
  if(is.null(xtitle))
    xtitle <- sQuote(short.deparse(substitute(x)))
  if(nzchar(context))
    xtitle <- paste(context, xtitle)
  whinge <- paste(xtitle,
                  "must be a named", paste(type, ",", sep=""),
                  "with components", commasep(nam))
  if(length(namopt) > 0)
    whinge <- paste(whinge, paren(paste("and optionally", commasep(namopt))))
  if(any(omitted)) {
    grizzle <- paste(ngettext(sum(omitted), "parameter", "parameters"),
                     commasep(nam[omitted]),
                     "omitted")
    whinge <- paste(whinge, grizzle, sep="; ")
  }
  if(any(foreign)) {
    grizzle <- paste(ngettext(sum(foreign), "component", "components"),
                     commasep(namesx[foreign]),
                     "not recognised")
    whinge <- paste(whinge, grizzle, sep="; ")
  }
  if(fatal)
    stop(whinge, call.=FALSE)
  return(whinge)
}

validposint <- function(n, caller, fatal=TRUE) {
  if(length(n) != 1 || n != round(n) || n <=0) {
    if(!fatal)
      return(FALSE)
    prefix <- if(!missing(caller)) paste("In ", caller, ", ", sep="") else NULL
    nname <- short.deparse(substitute(n))
    stop(paste0(prefix, nname, "should be a single positive integer"),
         call.=FALSE)
  }
  return(TRUE)
}


# errors and checks

forbidNA <- function(x, context="", xname, fatal=TRUE, usergiven=TRUE, warn=TRUE) {
  if(!anyNA(x)) return(TRUE)
  if(fatal || warn) {
    if(missing(xname)) xname <- sQuote(short.deparse(substitute(x)))
    if(usergiven) {
      ## argument came from user
      offence <- ngettext(length(x), "be NA", "contain NA values")
      whinge <- paste(context, xname, "must not", offence)
    } else {
      ## argument was computed internally
      violates <- ngettext(length(x), "is NA", "contains NA values")
      whinge <- paste(context, xname, violates)
    }
    if(fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  }
  return(FALSE)
}

check.finite <- function(x, context="", xname, fatal=TRUE, usergiven=TRUE, warn=TRUE) {
  offence <- if(anyNA(x)) "na" else if(any(!is.finite(x))) "NaNInf" else NULL
  if(is.null(offence)) return(TRUE)
  if(fatal || warn) {
    if(missing(xname)) xname <- sQuote(short.deparse(substitute(x)))     
    if(usergiven) {
      ## argument came from user
      violates <- switch(offence,
                         na     = ngettext(length(x),
                                           "must not be NA",
                                           "must not contain NA values"),
                         NaNInf = ngettext(length(x),
                                           "must be a finite value",
                                           "must contain finite values"))
    } else {
      ## argument was computed internally
      violates <- switch(offence,
                         na     = ngettext(length(x),
                                           "is NA",
                                           "contains NA values"),
                         NaNInf = ngettext(length(x),
                                           "is not finite",
                                           "contains non-finite values"))
    }
    whinge <- paste(context, xname, violates)
    if(fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  }
  return(FALSE)
}

check.satisfies <- function(cond, xname, should, context="",
                            fatal=TRUE, warn=TRUE) {
  if(cond) return(TRUE)
  if(fatal || warn) {
    whinge <-  paste(sQuote(xname), should)
    if(nzchar(context)) whinge <- paste(context, whinge)
    if(fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  }
  return(FALSE)
}

check.1.real <- function(x, context="", fatal=TRUE, warn=TRUE) {
  if(is.numeric(x) && length(x) == 1)
    return(TRUE)
  if(fatal || warn) {
    xname <- short.deparse(substitute(x))
    whinge <- paste(sQuote(xname), "should be a single number")
    if(nzchar(context)) whinge <- paste(context, whinge)
    if(fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  }
  return(FALSE)
}

check.1.integer <- function(x, context="", fatal=TRUE, warn=TRUE) {
  if(is.numeric(x) && length(x) == 1 && is.finite(x) && x %% 1 == 0)
    return(TRUE)
  if(fatal || warn) {
    xname <- short.deparse(substitute(x))
    whinge <- paste(sQuote(xname), "should be a single finite integer")
    if(nzchar(context)) whinge <- paste(context, whinge)
    if(fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  }
  return(FALSE)
}
  
check.1.string <- function(x, context="", fatal=TRUE, warn=TRUE) {
  if(is.character(x) && length(x) == 1)
    return(TRUE)
  if(fatal || warn) {
    xname <- short.deparse(substitute(x))
    whinge <- paste(sQuote(xname), "should be a single character string")
    if(nzchar(context)) whinge <- paste(context, whinge)
    if(fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  }
  return(FALSE)
}

complaining <- function(whinge, fatal=FALSE, value=NULL) {
  if(fatal) stop(whinge, call.=FALSE)
  warning(whinge, call.=FALSE)
  return(value)
}

explain.ifnot <- function(expr, context="") {
  ans <- expr
  if(!(is.logical(ans) && length(ans) == 1 && ans)) {
    ex <- deparse(substitute(expr))
    stop(paste(context, "it must be TRUE that", sQuote(ex)), call.=FALSE)
  }
}

warn.ignored.args <- function(..., context=NULL) {
  if((narg <- length(list(...))) > 0) {
    whinge <- paste(narg, "unrecognised",
                    ngettext(narg, "argument was", "arguments were"),
                    "ignored")
    if(!is.null(context)) whinge <- paste(context, whinge)
    warning(context)
  }
}

trap.extra.arguments <- function(..., .Context="", .Fatal=FALSE) {
  z <- list(...)
  if((narg <- length(z)) == 0) return(FALSE)
  nama <- names(z)
  named <- nzchar(nama)
  whinge <- paste(.Context, ":", sep="")
  if(any(named)) {
    # some arguments are named: ensure all are named
    nama <- sQuote(nama)
    if(!all(named)) 
      nama[!named] <- paste("[Arg", 1:length(nama), ,"]", sep="")[!named]
    whinge <- paste(whinge,
                    "unrecognised",
                    ngettext(narg, "argument", "arguments"),
                    commasep(nama),
                    ngettext(narg, "was", "were"), "ignored")
  } else {
    # all arguments unnamed
    whinge <- paste(whinge, 
                    narg, "unrecognised",
                    ngettext(narg, "argument was", "arguments were"),
                    "ignored")   
  }
  if(.Fatal) stop(whinge, call.=FALSE) else warning(whinge, call.=FALSE)
  return(TRUE)
}

there.can.be.only.one <- function(..., .NeedOne=TRUE, .Fatal=TRUE) {
  argh <- list(...)
  given <- !sapply(argh, is.null)
  ngiven <- sum(given)
  if(.NeedOne && ngiven == 0) {
    if(!.Fatal) return(FALSE)
    nama <- sQuote(names(argh))
    stop(paste("One of the arguments", commasep(nama, "or"), "is required"),
         call.=FALSE)
  }
  if(ngiven > 1) {
    if(!.Fatal) return(FALSE)
    namesgiven <- sQuote(names(argh)[given])
    stop(paste("The arguments", commasep(namesgiven), "are incompatible"),
         call.=FALSE)
  }
  return(TRUE)
}

## replace recognised keywords by other keywords
mapstrings <- function(x, map=NULL) {
  if(is.null(map)) return(x)
  x <- as.character(x)
  from <- names(map)
  map <- as.character(map)
  if(sum(nzchar(from)) != length(map))
    stop("input names are missing in map", call.=FALSE)
  if(any(duplicated(from)))
    stop("input names are duplicated in map", call.=FALSE)
  i <- match(x, from)
  hit <- !is.na(i)
  x[hit] <- map[i[hit]]
  return(x)
}

there.is.no.try <- function(...) {
  #' do, or do not
  y <- try(..., silent=TRUE)
  if(inherits(y, "try-error")) return(NULL)
  return(y)
}

dont.complain.about <- function(...) {
  #' prevents code checkers complaining about 'unused variables'
  #' Typically needed where the variables in question
  #' are referenced in an expression that will be evaluated elsewhere. 
  return(invisible(NULL))
}


matchNameOrPosition <- function(expected, avail) {
  if(length(avail) < length(expected))
    stop("Not enough arguments to match", call.=FALSE)
  j <- match(expected, avail)
  if(!anyNA(j)) return(j)
  everything <- seq_along(avail)
  for(k in seq_along(expected)) {
    if(is.na(j[k]))
      j[k] <- min(setdiff(everything, j[-k]))
  }
  return(j)
}

badprobability <- function(x, NAvalue=NA) {
  ifelse(is.na(x), NAvalue, !is.finite(x) | x < 0 | x > 1)
}

# test for equivalence of two functions 
samefunction <- function(f, g) {
  identical(deparse(f), deparse(g))
}

#' .................. calls and expressions ..................


fakecallstring <- function(fname, parlist) {
  cl <- do.call(call, append(list(name = fname), parlist))
  return(format(cl))
}

dotexpr.to.call <- function(expr, dot="funX", evaluator="eval.fv") {
  # convert an expression into a function call
  # replacing "." by the specified variable 
  stopifnot(is.expression(expr))
  aa <- substitute(substitute(ee, list(.=as.name(d))),
                   list(ee=expr, d=dot))
  bb <- eval(parse(text=deparse(aa)))
  cc <- as.call(bb)
  cc[[1]] <- as.name(evaluator)
  return(cc)
}

inject.expr <- function(base, expr) {
  ## insert an expression inside a call and parse it
  txt <- sub(".", as.character(expr), as.character(base), fixed=TRUE)
  parse(text=txt)
}

  
## Match variable names to objects in 'data' list or environment
getdataobjects <- function(nama, envir, datalist=NULL, fatal=FALSE) {
  if(is.null(nama)) return(NULL)
  stopifnot(is.character(nama))
  n <- length(nama)
  y <- vector(mode="list", length=n)
  names(y) <- nama
  if(!is.null(datalist)) {
    hit <- nama %in% names(datalist)
    if(any(hit))
      y[hit] <- as.list(datalist)[nama[hit]]
    external <- unlist(lapply(y, is.null))
  } else external <- rep(TRUE, n)
  y[external] <- mget(nama[external], envir=envir,
                    ifnotfound=list(NULL), inherits=TRUE)
  if(fatal && any(bad <- unlist(lapply(y, is.null)))) {
    nbad <- sum(bad)
    stop(paste(ngettext(nbad, "Covariate", "Covariates"),
               commasep(sQuote(nama[bad])),
               ngettext(nbad, "was not found", "were not found")),
         call.=FALSE)
  }
  names(y) <- nama
  attr(y, "external") <- external
  return(y)
}

