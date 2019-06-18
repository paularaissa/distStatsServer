##'
#' @title getGm
#' @description getGm returns the sum of logs of all values present in its arqguments
#' @details The geometric mean is one type of average.
#' It differs to arithmetic mean when the arithmetic mean adds items and the geometric mean multiplies items.
#' Formally the geometric mean is the i_n root of the \emph{n} numbers products or \emph{exp} to the mean log of \emph{x}.
#' @param x is name of a variable
#' @param na.rm logical. Should misssing values (including NaN) be removed?
#' @param zero.propagate logical. Control the propagation of zeros.
#' @return the sum of logs
#' @author Paula R. Costa e Silva
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
#'
getGm <- function(x, na.rm=TRUE, zero.propagate=FALSE) {

  bind.x <- getVarByName(x)

  if(any(bind.x < 0, na.rm=TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(bind.x==0, na.rm=TRUE)) {
      return(0)
    }
    sum.log <- sum(log(bind.x), na.rm = TRUE)
  } else {
    sum.log <- sum(log(bind.x[bind.x>0]),na.rm = na.rm)
  }
  return(sum.log)
}
