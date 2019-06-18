##'
#' @title Sum of Squares
#' @description Computes the sum of squares for a study variable.
#' @details Requires the function \code{\link{getVarByName}}.
#' @param x is a study variable.
#' @return the sum of squares as a numeric value.
#' @author Paula R. Costa e Silva
#' @export
#'
getSquaredSum <- function(x) {
  bind.x <- getVarByName(x)
  sum <- sum(bind.x^2)
  return(sum)
}
