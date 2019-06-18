#' @title Kurtosis
#' @description Kurtosis partial computation for a study variable.
#' @details Partial calculations of skew measure.
#'
#' Considering x[i] for elements of x and x* for their mean, so \eqn{m[r] = \sum[i](x[i]-x*)^r/n} is a partial sample moments of order r.
#'
#' This function computes the 2nd and 4th partial sample moments.
#'
#' @param x is a study variable.
#' @param mean the arithmetic mean.
#'
#' @return A list with components:
#' \item{m2}{second sample moment.}
#' \item{m4}{fourth sample moment.}
#'
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export

getKurtosis <- function(x, mean) {
  mean <- as.numeric(unlist(strsplit(mean, split = "x")))

  if (is.null(datasources))
    bind.x <- getVarByName(x)

  m4 <- sum((bind.x - mean)^4)
  m2 <- sum((bind.x - mean)^2)

  return(list(m4=m4,m2=m2))
}
