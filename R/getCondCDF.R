#############################################
# Compute the Cumulative Distribution Function
#############################################

getCondCDF <- function(originalValues) {
  #get ordered list of non repeated values
  xValues <- sort(originalValues)
  vals <- unique(xValues)
  cdfValues <- cumsum(tabulate(match(xValues, vals)))

  return(list(xValues=vals, cdfValues=cdfValues))
}
