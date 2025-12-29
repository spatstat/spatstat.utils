#'
#'  longvec.R
#'
#'  Utilities for long vectors
#'
#'  $Revision: 1.5 $ $Date: 2025/12/29 05:58:39 $

difflong <- function(x, drop=TRUE) {
  if(!is.null(dim(x))) stop("difflong is only implemented for vectors")
  sm <- storage.mode(x)
  switch(sm,
         double = {
           z <- .Call(C_Cdiffdouble,
                      rx=as.double(x))
         },
         integer = {
           z <- .Call(C_Cdiffint,
                      rx=as.integer(x))
         },
         logical = {
           z <- .Call(C_Cdiffint,
                      rx=as.integer(x))
         },
         complex = {
           Rez <- .Call(C_Cdiffdouble,
                        rx=as.double(Re(x)))
           Imz <- .Call(C_Cdiffdouble,
                        rx=as.double(Im(x)))
           z <- complex(real=Rez, imaginary=Imz)
         },
         stop(paste("difflong is not supported for vectors of type",
                    sQuote(sm)),
              call.=FALSE)
         )
  if(drop) z <- z[-1]
  return(z)
}

