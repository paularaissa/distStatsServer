##'
#' @title Sum of Study Variable Elements
#' @description It computes the sum of all values into a numeric vector.
#' @details Logical true values are regarded as one, false values as zero. For historical reasons,
#' NULL is accepted and treated as if it were integer(0).
#' Loss of accuracy can occur when summing values of different signs: this can even occur for sufficiently long
#' integer inputs if the partial sums would cause integer overflow. Where possible extended-precision accumulators are
#' used, but this is platform-dependent.
#' @param x numeric vector.
#' @return The sum.
#' If all of vakues are of type integer or logical, then the sum is integer, and in that case the result
#' will be NA (with a warning) if integer overflow occurs. Otherwise it is a length-one numeric or complex vector.
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'

getSum <- function(x) {
  bind.x <- getVarByName(x)
  sum <- sum(bind.x, na.rm = TRUE)
  return(sum)
}
