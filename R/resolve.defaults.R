#
#   resolve.defaults.R
#
#  $Revision: 1.44 $ $Date: 2024/12/02 04:15:06 $
#
# Resolve conflicts between several sets of defaults
# Usage:
#     resolve.defaults(list1, list2, list3, .......)
# where the earlier lists have priority 
#
# Also
#     graphicsPars(functionname)
# gives names of arguments recognised by 'functionname'
# which may not be formal arguments.

resolve.defaults <- function(..., .MatchNull=TRUE, .StripNull=FALSE) {
  ## Each argument is a list. Append them.
  argue <- c(...)
  ## does a NULL value overwrite a non-null value occurring later in the sequence?
  if(isFALSE(.MatchNull)) {
    isnul <- sapply(argue, is.null)
    argue <- argue[!isnul]
  }
  if(!is.null(nam <- names(argue))) {
    named <- nzchar(nam)
    arg.unnamed <- argue[!named]
    arg.named <-   argue[named]
    if(any(discard <- duplicated(names(arg.named)))) 
      arg.named <- arg.named[!discard]
    argue <- append(arg.unnamed, arg.named)
  }
  ## should a NULL value mean that the argument is missing?
  if(isTRUE(.StripNull)) {
    isnull <- sapply(argue, is.null)
    argue <- argue[!isnull]
  }
  return(argue)
}

do.call.without <- function(fun, ..., avoid, envir=parent.frame()) {
  argh <- list(...)
  nama <- names(argh)
  if(!is.null(nama))
    argh <- argh[!(nama %in% avoid)]
  do.call(fun, argh, envir=envir)
}

do.call.matched <- function(fun, arglist, funargs,
                            extrargs=NULL,
                            matchfirst=FALSE,
                            sieve=FALSE,
                            skipargs=NULL,
                            envir=parent.frame()) {
  if(!is.function(fun) && !is.character(fun))
    stop("Internal error: wrong argument type in do.call.matched")
  if(is.character(fun)) {
    fname <- fun
    fun <- get(fname, mode="function")
    if(!is.function(fun))
      stop(paste("internal error: function", sQuote(fname), "not found",
                 sep=""))
  }
  ## determine list of argument names to be matched
  if(missing(funargs))
    funargs <- names(formals(fun))
  funargs <- c(funargs, extrargs)
  funargs <- setdiff(funargs, skipargs)
  ## identify which arguments in the call actually match a formal argument
  givenargs <- names(arglist) %orifnull% rep("", length(arglist))
  matched <- givenargs %in% funargs
  ## deem the first argument to be matched?
  if(length(givenargs) && matchfirst && !nzchar(givenargs[1L]))
    matched[1L] <- TRUE
  # apply 'fun' to matched arguments
  usedargs <- arglist[matched]
  out <- do.call(fun, usedargs, envir=envir)
  # retain un-matched arguments?
  if(sieve)
    out <- list(result=out,
                otherargs=arglist[!matched],
                usedargs=usedargs)
  return(out)
}


resolve.1.default <- function(.A, ...) {
  if(is.character(.A)) {
    ## .A is the name of the parameter to be returned
    Aname <- .A
    res <- resolve.defaults(...)
  } else if(is.list(.A) && length(.A) == 1) {
    ## .A is a list giving the name and default value of the parameter
    Aname <- names(.A)
    res <- resolve.defaults(..., .A)
  } else stop("Unrecognised format for .A")
  hit <- (names(res) == Aname)
  if(!any(hit)) return(NULL)
  return(res[[min(which(hit))]])
}

# extract all the arguments that match '...' rather than a named argument

passthrough <- function(.Fun, ..., .Fname=NULL) {
  if(is.null(.Fname))
    .Fname <- deparse(substitute(.Fun))
  # make a fake call to the named function using the arguments provided
  cl <- eval(substitute(call(.Fname, ...)))
  # match the call to the function 
  mc <- match.call(.Fun, cl)
  # extract the arguments
  mcargs <- as.list(mc)[-1]
  # figure out which ones are actually formal arguments of the function
  nam <- names(formals(.Fun))
  nam <- setdiff(nam, "...")
  known <- names(mcargs) %in% nam
  # return the *other* arguments
  return(mcargs[!known])
}

graphicsPars <- local({
  ## recognised additional arguments to image.default(), axis() etc
    AxisArgs <-  c("cex", "font", 
                   "cex.axis", "cex.lab",
                   "col.axis", "col.lab",
                   "font.axis", "font.lab", 
                   "mgp", "xaxp", "yaxp", "tck", "tcl", "las", "fg", "xpd")
    PlotArgs <- c(
        "main", "asp", "sub", "axes", "ann",
        AxisArgs, 
        "cex.main", "cex.sub",
        "col.main", "col.sub",
        "font.main", "font.sub")
    TextDefArgs <- setdiff(names(formals(text.default)), "...")
    TextArgs <- c(TextDefArgs, "srt", "family", "xpd")
    TitleArgs <- c("line", "outer", "adj")
                        
  TheTable <- 
    list(plot = PlotArgs,
         image = c(
           "main", "asp", "sub", "axes", "ann",
           "xlim", "ylim", "zlim",
           "box",  # note 'box' is not an argument of image.default
           "cex", "font", 
           "cex.axis", "cex.lab", "cex.main", "cex.sub",
           "col.axis", "col.lab", "col.main", "col.sub",
           "font.axis", "font.lab", "font.main", "font.sub",
           "claim.title.space", "adj.main"),
         axis = c(
           "cex", 
           "cex.axis", "cex.lab",
           "col.axis", "col.lab",
           "font.axis", "font.lab",
           "mgp", "xaxp", "yaxp", "tck", "tcl", "las", "fg", "xpd"),
         owin = c(
           "main", "sub",
           "xlim", "ylim",
           "cex", "font", "col",
           "border", "box", 
           "cex.main", "cex.sub",
           "col.main", "col.sub",
           "font.main", "font.sub",
           "xaxs", "yaxs",
           "claim.title.space", "adj.main"),
         lines = c("lwd", "lty", "col", "lend", "ljoin", "lmitre"),
         symbols = c(PlotArgs, "fg", "bg"),
         points = c("pch", "col", "bg", "fg", "cex", "lwd", "lty"),
         text = TextArgs,
         title = union(TitleArgs, AxisArgs),
         persp = union(TitleArgs,
                       c("x", "y", "z",
                         "xlim", "ylim", "zlim",
                         "xlab", "ylab", "zlab",
                         "main", "sub",
                         "theta", "phi", "r", "d", "scale",
                         "expand", "col", "border",
                         "ltheta", "lphi", "shade", "box",
                         "axes", "nticks", "ticktype"))
         )

    TheTable$ppp <- unique(c(TheTable$owin,
                             TheTable$symbols,
                             TheTable$points,
                             "etch",
                             "annotate", "labelmap",
                             "markrange", "marklevels"))

  graphicsPars <- function(key) {
    n <- pmatch(key, names(TheTable))
    if(is.na(n)) return(NULL)
    return(TheTable[[n]])
  }

  graphicsPars
})
