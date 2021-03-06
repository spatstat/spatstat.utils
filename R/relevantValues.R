#'
#' relevantValues.R
#'
#' $Revision: 1.2 $ $Date: 2021/03/06 11:08:11 $
#'

RelevantZero <- function(x) vector(mode=typeof(x), length=1L)
isRelevantZero <- function(x) identical(x, RelevantZero(x))
RelevantEmpty <- function(x) vector(mode=typeof(x), length=0L)
RelevantNA <- function(x) { RelevantZero(x)[2] }

