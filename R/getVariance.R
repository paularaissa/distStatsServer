##'
#' @title Sample Variance
#' @description Computes the variance for a study variable.
#' @details By default, computes the sample variance.
#' @param x a character, the name of a study variable.
#' @param mean numerical, the arithmetic mean.
#' @return the variance.
#' @author Paula R. Costa e Silva
#' @export
#'
getVariance <- function(x, mean) {

  bind.x <- getVarByName(x)
  mean <- as.numeric(unlist(strsplit(mean, split = "x")))
  dif <- bind.x - mean
  quad <- dif ^ 2
  sum <- sum(quad)

  return(sum)
}
