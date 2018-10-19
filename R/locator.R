#'
#'     locator.R
#'
#'    Replacement for locator()
#' 
#'    $Revision: 1.7 $  $Date: 2018/10/19 04:19:09 $

.spatstatLocatorEnv <- new.env()

getSpatstatLocatorQueue <- function() {
  get("locatorqueue", envir=.spatstatLocatorEnv)
}

putSpatstatLocatorQueue <- function(x) {
  assign("locatorqueue", x, envir=.spatstatLocatorEnv)
}
  
putSpatstatLocatorQueue(data.frame(x=numeric(0), y=numeric(0)))

queueSpatstatLocator <- function(x, y) {
  locatorqueue <- getSpatstatLocatorQueue()
  if(missing(y)) y <- NULL
  xy <- xy.coords(x,y)
  x <- xy$x
  y <- xy$y
  locatorqueue <- rbind(locatorqueue,
                        data.frame(x=x, y=y))
  putSpatstatLocatorQueue(locatorqueue)
  return(nrow(locatorqueue))
}

spatstatLocator <- function(n, type=c("p","l","o","n"), ...) {
  #' Replacement for locator()
  #' Remedy for failure of locator(type="p") in RStudio
  #' Also allows software testing in non-interactive sessions
  if(!interactive()) {
    #' Return previously-queued coordinates
    if(missing(n) || is.null(n)) n <- Inf
    locatorqueue <- getSpatstatLocatorQueue()
    navail <- nrow(locatorqueue)
    popoff <- (seq_len(navail) <= n)
    result <- locatorqueue[popoff, , drop=FALSE]
    locatorqueue <- locatorqueue[!popoff, , drop=FALSE]
    putSpatstatLocatorQueue(locatorqueue)
    return(as.list(result))
  }
  # ............... interactive ......................
  if(!identical(TRUE, dev.capabilities()$locator))
    stop("Sorry, this graphics device does not support the locator() function")
  # validate
  type <- match.arg(type)
  do.points <- type %in% c("p","o")
  do.lines <- type %in% c("l","o")
  argh <- list(...)
  pointsArgs <- c("cex", "col", "pch", "fg", "bg")
  segmentArgs <- graphicsPars("lines")
  # go
  res <- list(x=numeric(0), y = numeric(0))
  i <- 1L
  if(missing(n)) n <- Inf
  while(i<=n){
    tmp <- locator(n=1)
    if(is.null(tmp)) return(res)
    if(do.points)
      do.call.matched(points.default, append(tmp, argh), extrargs=pointsArgs)
    res$x <- c(res$x,tmp$x)
    res$y <- c(res$y,tmp$y)
    if(do.lines && i > 1L) {
      xy <- with(res, list(x0=x[i-1L], y0=y[i-1L], x1=x[i], y1=y[i]))
      do.call.matched(segments, append(xy, argh), extrargs=segmentArgs)
    }
    i <- i+1L
  }
  return(res)
}

