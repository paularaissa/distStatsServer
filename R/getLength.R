##'
#' @title Length of an Object
#' @description Computes the size of a given vector.
#' @details Get or set the length of vectors (including lists) and factors, and of any other R object for
#' which a method has been defined.
#'
#' For vectors (including lists) and factors the length is the number of elements.
#' For an environment it is the number of objects in the environment, and NULL has length 0.
#' For expressions and pairlists (including language objects and dotlists) it is the length of the pairlist chain.
#' All other objects (including functions) have length one.
#' @param x is an R object.
#'
#' @return a numerical value.
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'

getLength <- function(x) {
  bind.x <- getVarByName(x)
  x.length <- length(bind.x)
  return(x.length)
}
