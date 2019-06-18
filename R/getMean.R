##'
#' @title Arithmetic Mean
#' @description Computes the arithmetic mean.
#' @details Generic function for the arithmetic mean.
#' @param x is a study variable.
#' @return The arithmetic mean of the values in x.
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'

getMean <- function(x) {
  bind.x <- getVarByName(x)
  mean <- mean(bind.x)
  return(mean)
}
