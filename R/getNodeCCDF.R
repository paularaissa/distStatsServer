#-------------------------------------- HEADER --------------------------------------------#
#' @title Conditional CDf for Node
#' @description Computes the x value from given percentile.
#' @details Read each data file (node) and compute the local cumDistFunc for each data node.
#' @param varName a character, the name of study variable.
#' @param catName a character, categorical/conditional variable
#' @param datasources a list of parameters to access files sytems or databases.
#' @author Rui Camacho, Paula Raissa
#'
#'

getNodeCCDF <- function(varName, catName, fileName){
  categories <- list()
  CDFs <- list()

  originalValues <- read.csv(fileName, header = TRUE)
  allValues <- subset(originalValues, select=c(catName, varName))
  categories <- getDomain(allValues, catName)


  for (category in categories){
    # Split the sample by factor
    categorySubset <- subset(allValues, allValues[catName]==category)
    CDFs[[category]] <- getCondCDF(categorySubset[[varName]])
  }

  return(list(categories=categories, CDFs=CDFs))
}
