##'
#' @title Samples Counting
#' @description Computes the x value corresponding to a given percentile.
#' @details Returns the x value corresponding to a given percentile [0..1]
#' @param varName a study variable, it must be a numeric vector.
#' @param xREF a numeric value, given reference do calculate the sample size.
#' @return The numeric value corresponding to a percentile.
#' @author Rui Camacho, Paula Raissa
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
##'

getSamplesCounting <- function(x=NULL, formula=NULL, xREF=NULL, cVar=NULL, relation=NULL, threshold=NULL, relation2=NULL, threshold2=NULL) {
  formula.temp <- as.formula(formula)
  formula2use <- formula.temp[-2]
  originalValues <- NULL

  if (!is.null(x)) {
    originalValues <- as.numeric(eval(parse(text=x)))
  }

  if (!is.null(formula) && !is.null(cVar) && !is.null(relation) && !is.null(threshold)) {
     originalValues <- getVarByCondFormula(formula = formula2use, cVar = cVar, relation = relation, relation2 = relation2, threshold = threshold, threshold2 = threshold2)
  }

 # totalSamples <- nrow(originalValues)

  return(originalValues)

  # vect.values <- as.vector(originalValues)
  # contVal <- length(which(as.numeric(vect.values) <= xREF))
  #
  # return(list(contVal=contVal, totalSamples=totalSamples))
}
