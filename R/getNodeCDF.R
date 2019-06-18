#-------------------------------------- HEADER --------------------------------------------#
#' @title Get CDf for Node
#' @description Computes the x value from given percentile.
#' @details Read each data file (node) and compute the local cumDistFunc for each data node.
#' @param varName a character, the name of study variable.
#' @param datasources a list of parameters to access files sytems or databases.
#' @author Rui Camacho, Paula Raissa
#'
#'
getNodeCDF <- function(varName, datasources){
  originalValues <- getVarByName(varName)
  # get ordered list of non repeated values
  xValues <- originalValues[order(originalValues),]
  vals <- unique(xValues)
  rval <- cumsum(tabulate(match(xValues, vals)))

  return(list(xValues=vals, cdfValues=rval))
}
