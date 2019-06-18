##'
#' @title Maximum
#' @description Computes the maximum value in the sample according to a study variable.
#' @details Return the maximum of all the values present in their arguments, as integer if all are logical or integer,
#' as double if all are numeric, and character otherwise.
#'
#' The maximum of a numeric empty set is +Inf which ensures transitivity.
#' For numeric x ds.max(x) == -Inf whenever ds.length(x) == 0 (after removing missing values if requested).
#'
#' By definition the max of a numeric vector containing an NaN is NaN, except that the max of any vector containing
#' an NA is NA even if it also contains an NaN. Note that ds.max(NA, Inf) == NA even though the maximum would be Inf whatever
#' the missing value actually is.
#'
#' Character versions are sorted lexicographically. The max of an empty character vector is defined to be character NA.
#' @param x is a study variable.
#' @return The maximum numeric value inside the vector.
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'

getMax <- function(x) {
  bind.x <- getVarByName(x)
  max <- max(bind.x)
  return(max)
}
