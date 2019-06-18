##'
#' @title Product of Study Variable Elements
#' @description  It computes the product of all values in its arguments.
#' @details If na.rm is FALSE an NA value in any of the arguments will cause a value of NA to be returned, 
#' otherwise NA values are ignored.
#' 
#' Logical true values are regarded as one, false values as zero. For historical reasons, NULL is accepted and 
#' treated as if it were numeric(0).
#' @param x is the name of a study variable.
#' @param na.rm logical (TRUE/FALSE). Remove, or not the missing values.
#' @return The product as a numerical value.
#' @author Paula R. Costa e Silva
#' @section Dependencies: 
#' \code{\link{getVarByName}}
#' @export
#'

getProd <- function(x, naRm=FALSE) {
  bind.x <- getVarByName(x)
  product <- prod(bind.x, na.rm=naRm)
  return(product)
}