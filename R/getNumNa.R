##'
#' @title Count of missing values
#' @description Computes the missing values of a given vector.
#' @details Counts all na or another missing value indicator into a vector.
#' @param x is a numerical variable.
#' @return the NA count as numerical value.
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'
getNumNa <- function(x){
  bind.x <- getVarByName(x)
  num.na <- length(which(is.na(bind.x)))
  return (num.na)
}
