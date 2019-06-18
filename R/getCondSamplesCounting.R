###############################################################
# return the x value corresponding to a give percentile [0..1]
###############################################################
##'
#' @title Conditional Samples Counting
#' @description Computes the x value corresponding to a given percentile and categories.
#' @details Returns the x value corresponding to a given percentile [0..1]
#' @param varName a study variable, it must be a numeric vector.
#' @param catName a cetegorical variable.
#' @param cat a character, given category to compute the sample size.
#' @param xREF a numeric value, given reference do calculate the sample size.
#' @param datasources a list of data connection parameters.
#' @return The numeric value corresponding to a percentile.
#' @author Rui Camacho, Paula Raissa
#' @section Dependencies:
#' \code{\link{getVarByName}}
#' @export
##'
getCondSamplesCounting <- function(varName, catName, cat, xREF, datasources){
  originalValues <- read.csv(datasources, header = TRUE)
  allValues <- subset(originalValues, select=c(catName, varName))
  categorySubset <- subset(allValues, allValues[catName]==cat)
  totalSamples <- nrow(categorySubset)
  xREF <- as.numeric(xREF)

  cont <- 0
  for (val in categorySubset[[varName]]) {
    if (val <= xREF) {
      cont <- cont + 1
    }
  }
  return(list(contVal=cont, totalSamples=totalSamples))
}
