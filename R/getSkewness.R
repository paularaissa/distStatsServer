##'
#' @title Skewness
#' @description Skewness partial computation for a study variable.
#' @details Partial calculations of skew measure.
#'
#' Considering \eqn{x[i]} for elements of \eqn{x} and \eqn{x*} for their mean, so \eqn{m[r] = \sum{i}(x[i]-x*)^r}
#' is a partial sample moments of order r.
#'
#' This function computes the 2nd and 3th partial sample moments.
#' @param x is a study variable
#' @param mean the arithmetic mean as a numeric value
#' @param datasources a list of data connection parameters.
#'
#' @return A list with components:
#' \item{m2}{second sample moment.}
#' \item{m3}{third sample moment.}
#'
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'

getSkewness <- function(x, mean) {
  mean <- as.numeric(unlist(strsplit(mean, split = "x")))
  bind.x <- getVarByName(x)
  m3 <- sum((bind.x - mean)^3)
  m2 <- sum((bind.x - mean)^2)
  return(list(m3=m3,m2=m2))
}
