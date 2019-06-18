#'
#' @title Compute the levels for a categorical variable.
#' @description Calculates the fitted values in each data node.
#' @details Considering \code{y} as a response variable and x as study variable, the fitted values are the y-values that
#' would expect for the given x-values according to the best-fitting straight line.
#'
#' @param x is a study variable.
#' @param g is the categorical variable.
#'
#' @return a vector of fitted values.
#'
#' @section Dependencies:
#' \code{\link{getVarbyFormula}}
#'
#' @author Paula R. Costa e Silva
#' @export
#'

getLevels <- function(x, g, datasources=NULL) {
  g.data <- eval(parse(text=g))
  g.factor <- as.factor(g.data)
  g.levels <- levels(g.factor)

  ord.level <- list()
  for (level in g.levels) {
    ord.level[[level]] <- allData[allData[[g]] == level,]
  }
  return(list(g.levels=g.levels, ord.level=ord.level))

  #return(g.levels)

}
